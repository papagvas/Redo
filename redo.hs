import Control.Monad (filterM)
import Data.Map.Lazy (insert, fromList, toList, adjust)
import Data.Maybe (listToMaybe)
import System.Directory (renameFile, removeFile, doesFileExist, getDirectoryContents)
import System.Environment (getArgs, getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath (hasExtension, replaceBaseName, takeBaseName)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, waitForProcess, shell, CreateProcess(..))

main :: IO ()
main = getArgs >>= mapM_ redo
  

redo :: String -> IO ()
redo target = do
  redoPath target >>= maybe (error $ "No .do file found for target '" ++ target ++ "'") redo'  
  where redo' :: FilePath -> IO ()
        redo' path = do
          oldEnv <- getEnvironment
	  let newEnv = toList $ adjust (++ ":.") "PATH" $ insert "REDO_TARGET" target $ fromList oldEnv
          (_, _, _, ph) <- createProcess $ (shell $ cmd path) {env = Just newEnv}
          exit <- waitForProcess ph
          case exit of
            ExitSuccess -> do renameFile tmp target
            ExitFailure code -> hPutStrLn stderr ("Redo script exited with a non-zero exit code: " ++ show code) >> removeFile tmp
        tmp = target ++ "---redoing"
        cmd path = unwords ["sh", path, "0", takeBaseName target, tmp, ">", tmp]

redoPath :: FilePath -> IO (Maybe FilePath)
redoPath target = filterM doesFileExist candidates >>= return . listToMaybe
  where candidates = [target ++ ".do"] ++ if hasExtension target then [replaceBaseName target "default" ++ ".do"] else []

upToDate :: String -> IO Bool
upToDate target = do
  sumsAndDeps <- readFile (depDir </> checksums) >>= return . words
  mapM depUpToDate deps
  where sums = [sumsAndDeps !! x | x <- [1,3..(length sumsAndDeps - 1)]]
        deps = [sumsAndDeps !! x | x <- [0,2..(length sumsAndDeps - 1)]]
	checksumDict = fromList $ zip deps sums  
	depUpToDate :: [Char] -> IO Bool 
	depUpTodate dep = lookup dep checksumDict >>= maybe (return False) (return . (md5hash dep ==))
	
	   
	  