module TaskWarriorWeb.Task where
import TaskWarriorWeb.Data
import TaskWarriorWeb.Utility

import Control.Applicative
import Control.Monad
import System.Process
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec as A

getTasksIO :: IO [Task]
getTasksIO = do
  out <- readProcess "task" ["export"] ""
  let input = BS.pack $ "[" ++ out ++ "]"
  let tasks = case A.parse json input of 
        A.Done _ val -> fromJSON val
        _ -> mzero
  return . join .  ntrans $ (ntrans tasks :: Either String [Task])

filterOutZeroId :: [Task] -> [Task]
filterOutZeroId = filter $ (> 0) <$> taskId
