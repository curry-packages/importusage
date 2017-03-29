-----------------------------------------------------------------------------
--- Show the usage, i.e., all calls, of imported entities in a module
---
--- @author Michael Hanus
--- @version April 2016
-----------------------------------------------------------------------------

module ImportUsage(main,showImportCalls) where

import Char
import Directory
import Distribution
import FilePath ((</>), takeFileName)
import FlatCurry.Types
import FlatCurry.Files
import List
import Sort
import System


-- Check arguments and call main function:
main :: IO ()
main = do
  args <- getArgs
  if length args /= 1
   then putStrLn $ "ERROR: Illegal arguments: " ++
                   concat (intersperse " " args) ++ "\n" ++
                   "Usage: curry-usedimports <module_name>"
   else showAllImportedCalls (stripCurrySuffix (head args))

showAllImportedCalls :: String -> IO ()
showAllImportedCalls modname = do
  prog <- readCurrentFlatCurry modname
  putStrLn $ "Calls to imported functions/constructors in module '" ++
             modname ++ "':\n"
  putStrLn $ showImportCalls prog

showImportCalls :: Prog -> String
showImportCalls = formatImpCalls . getAllImpCalls

-- format import calls as import declarations:
formatImpCalls :: [(String,[String])] -> String
formatImpCalls impcalls =
  concatMap (\(mod,imps)->"import "++mod++"("++
                          concat (intersperse "," (map showName imps))++")\n")
            impcalls
 where
   showName name = if isAlpha (head name) then name else '(':name++")"

getAllImpCalls :: Prog -> [(String,[String])]
getAllImpCalls (Prog mod imps _ funs _) =
  calls2impCalls imps
                (mergeSortBy (\ (_,n1) (_,n2) -> n1 <= n2)
                             (allFunCalls mod funs))

calls2impCalls :: [String] -> [QName] -> [(String,[String])]
calls2impCalls [] _ = []
calls2impCalls (mod:mods) funs =
 (mod, map snd (filter (\(m,_)->m==mod) funs)) : calls2impCalls mods funs

-- Computes the list of called functions in a list of function declarations
allFunCalls :: String -> [FuncDecl] -> [QName]
allFunCalls _ [] = []
allFunCalls mod (Func _ _ _ _ (Rule _ e) : funs) =
     union (globalFunsInExpr mod e) (allFunCalls mod funs)
allFunCalls mod (Func _ _ _ _ (External _) : funs) = allFunCalls mod funs

-- Gets a list of all functions called in an expression that are not defined
-- in a module (first argument):
globalFunsInExpr :: String -> Expr -> [QName]
globalFunsInExpr mod exp = funsInExpr exp
 where
  funsInExpr (Var _) = []
  funsInExpr (Lit _) = []
  funsInExpr (Comb _ (m,f) es) =
    if m==mod || (m=="Prelude" && f `elem` ["commit","apply","cond"])
    then nub (concatMap funsInExpr es)
    else nub ((m,f) : concatMap funsInExpr es)
  funsInExpr (Free _ e) = funsInExpr e
  funsInExpr (Let bs e) = union (nub (concatMap (funsInExpr . snd) bs)) (funsInExpr e)
  funsInExpr (Or e1 e2) = union (funsInExpr e1) (funsInExpr e2)
  funsInExpr (Case _ e bs) = union (funsInExpr e)
                                   (nub (concatMap funsInBranch bs))
                       where funsInBranch (Branch _ be) = funsInExpr be
  funsInExpr (Typed e _) = funsInExpr e


----------------- Auxiliaries:

-- Read a FlatCurry program (parse only if necessary):
readCurrentFlatCurry :: String -> IO Prog
readCurrentFlatCurry modname = do
  mbdirfn <- lookupModuleSourceInLoadPath modname
  let progname = maybe modname snd mbdirfn
  let fcyprogname = flatCurryFileName
                        (maybe modname (\ (d,_) -> d </> takeFileName modname)
                                       mbdirfn)
  fcyexists <- doesFileExist fcyprogname
  if not fcyexists
    then readFlatCurry progname
    else do ctime <- getModificationTime progname
            ftime <- getModificationTime fcyprogname
            if ctime>ftime
             then readFlatCurry modname
             else readFlatCurryFile fcyprogname

-----------------------------------------------------------------------------
