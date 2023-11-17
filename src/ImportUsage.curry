-----------------------------------------------------------------------------
--- Show the usage, i.e., all calls, of imported entities in a module
---
--- @author Michael Hanus
--- @version November 2023
-----------------------------------------------------------------------------

module ImportUsage ( main, showImportCalls )
 where

import Data.List          ( intercalate, isPrefixOf, nub, sort, union )
import System.Directory   ( doesFileExist, getModificationTime )
import System.FilePath    ( (</>), takeFileName )
import System.Environment ( getArgs )

import FlatCurry.Types
import FlatCurry.Files
import System.CurryPath ( lookupModuleSourceInLoadPath, runModuleAction )

-- Check arguments and call main function:
main :: IO ()
main = do
  args <- getArgs
  case args of
    [prog] -> runModuleAction showAllImportedCalls prog
    _      -> putStrLn $ "ERROR: Illegal arguments: " ++ unwords args ++ "\n" ++
                         "Usage: curry-usedimports <module_name>"

showAllImportedCalls :: String -> IO ()
showAllImportedCalls modname = do
  prog <- readCurrentFlatCurry modname
  putStrLn $ "Uses of imported types/functions/constructors in module '" ++
             modname ++ "':\n"
  putStrLn $ showImportCalls prog

showImportCalls :: Prog -> String
showImportCalls = formatImpCalls . getAllImpCalls

-- format import calls as import declarations:
formatImpCalls :: [(String,[String])] -> String
formatImpCalls impcalls =
  concatMap (\(mod,imps) -> "import " ++ mod ++ "(" ++
                            intercalate ", " (map showName imps) ++ ")\n")
            impcalls
 where
   showName name = if isAlpha (head name) then name else '(':name++")"

getAllImpCalls :: Prog -> [(String,[String])]
getAllImpCalls (Prog mod imps tdecls funs _) =
  groupByModules imps
    (foldr union [] (map (allImpFTypes mod) funs ++
                     map (allImpDTypes mod) tdecls))
    (foldr union [] (map (allFunCalls mod) funs))

groupByModules :: [String] -> [QName] -> [QName] -> [(String,[String])]
groupByModules mods typs funs = map callsFromModule mods
 where
  callsFromModule mod =
    (mod,
     sort (map snd (filter (\ (m,_) -> m==mod) typs)) ++
     sort (map snd (filter (\ (m,_) -> m==mod) funs)))

-- Computes the set of called functions in a function declaration
-- that are not defined in the module given as the first argument.
allFunCalls :: String -> FuncDecl -> [QName]
allFunCalls mod (Func _ _ _ _ rl) =
  case rl of Rule _ e   -> globalFunsInExpr mod e
             External _ -> []

-- Gets a list of all functions called in an expression that are not defined
-- in the module given as the first argument.
globalFunsInExpr :: String -> Expr -> [QName]
globalFunsInExpr mod exp = funsInExpr exp
 where
  funsInExpr (Var _) = []
  funsInExpr (Lit _) = []
  funsInExpr (Comb _ (m,f) es) =
    if m==mod || isSpecialName f || -- ignore generated operations
       (m=="Prelude" && f `elem` ["commit","apply","cond"])
      then nub (concatMap funsInExpr es)
      else nub ((m,f) : concatMap funsInExpr es)
  funsInExpr (Free _ e) = funsInExpr e
  funsInExpr (Let bs e) = union (nub (concatMap (funsInExpr . snd) bs))
                                (funsInExpr e)
  funsInExpr (Or e1 e2) = union (funsInExpr e1) (funsInExpr e2)
  funsInExpr (Case _ e bs) = union (funsInExpr e)
                                   (nub (concatMap funsInBranch bs))
                       where funsInBranch (Branch _ be) = funsInExpr be
  funsInExpr (Typed e _) = funsInExpr e


-- Computes the set of type names used in a type declaration
-- that are not defined in the module given as the first argument.
allImpDTypes :: String -> TypeDecl -> [QName]
allImpDTypes mod (Type _ _ _ cdecls)  = nub (concatMap consTypes cdecls)
 where
  consTypes (Cons _ _ _ texps) = nub (concatMap (importedTypes mod) texps)
allImpDTypes mod (TypeSyn _ _ _ texp) = importedTypes mod texp
allImpDTypes mod (TypeNew _ _ _ (NewCons _ _ texp)) = importedTypes mod texp

-- Computes the set of type names used in a function declaration
-- that are not defined in the module given as the first argument.
allImpFTypes :: String -> FuncDecl -> [QName]
allImpFTypes mod (Func (_,f) _ _ texp _) =
  if isSpecialName f
    then []  -- ignore types of generated operations
    else importedTypes mod texp

importedTypes :: String -> TypeExpr -> [QName]
importedTypes _   (TVar _)          = []
importedTypes mod (FuncType t1 t2)  =
  union (importedTypes mod t1) (importedTypes mod t2)
importedTypes mod (ForallType _ te) = importedTypes mod te
importedTypes mod (TCons tc tes) =
  nub (itc tc ++ concatMap (importedTypes mod) tes)
 where
  itc (m,t) = if m == mod || isSpecialName t -- ignore dictioniaries
                then []
                else [(m,t)]

-- Is the name special, e.g., starts with an underscore?
isSpecialName :: String -> Bool
isSpecialName s = "_" `isPrefixOf` s || '#' `elem` s


----------------- Auxiliaries:

-- Reads a FlatCurry program (parse only if necessary).
readCurrentFlatCurry :: String -> IO Prog
readCurrentFlatCurry modname = do
  mbdirfn <- lookupModuleSourceInLoadPath modname
  let progname    = maybe modname snd mbdirfn
      fcyprogname = maybe "" (\(d,_) -> d </> flatCurryFileName modname) mbdirfn
  fcyexists <- doesFileExist fcyprogname
  if not fcyexists
    then readFlatCurry modname
    else do ctime <- getModificationTime progname
            ftime <- getModificationTime fcyprogname
            if ctime > ftime
              then readFlatCurry modname
              else readFlatCurryFile fcyprogname

-----------------------------------------------------------------------------
