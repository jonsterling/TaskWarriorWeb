{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module TaskWarrior where
       
import Yesod
import Data.Aeson
import qualified Data.Aeson.Types as T
import Control.Applicative
import Control.Monad
import Data.Either
import Data.Maybe
import Data.Text (Text,unpack)
import qualified Data.ByteString.Char8 as BS
-- Aeson's "encode" to json generates lazy bytestrings
import qualified Data.ByteString.Lazy.Char8 as BSL

import Data.Attoparsec
import Data.List (intercalate)
import System.Process


data TaskWarrior = TaskWarrior

mkYesod "TaskWarrior" [parseRoutes|
/ HomeR GET
|]

instance Yesod TaskWarrior

getHomeR :: Handler RepHtml
getHomeR = defaultLayout $ do
  toWidget mainStyle
  tasksTable . filterOutZeroId =<< liftIO getTasksIO

mainStyle =
  [cassius|
   body
     font-family: "Menlo"
     font-size: 12px
     
   table
     margin: 45px
     width: 550px
     border-collapse: collapse
     text-align: left
     
   th  
     font-size: 14px
     font-weight: normal
     color: #039
     padding: 10px 8px
     border-bottom: 2px solid #6678b1
     
   td
     border-bottom: 1px solid #ccc
     color: #669
     padding: 6px 8px
     
   tbody tr:hover td  
     color: #009
     
   tbody tr:hover  
     background-color: rgb(242,244,255)
   |]

tasksTable :: [Task] -> Widget
tasksTable ts = 
  [whamlet|
   <table>
     <thead>
       <tr>
         <th>Id
         <th>Description
         <th>Project
         <th>Priority
         <th>Tags
     <tbody>    
       $forall t <- ts
         <tr>
           <td>#{taskId t}
           <td>#{taskDescription t}
           <td>#{fromMaybe "" $ taskProject t}
           <td>#{maybe "" show $ taskPriority t} 
           <td>#{showTags $ taskTags t}
   |]
  
taskWidget :: Task -> Widget
taskWidget t = 
  [whamlet|
   #{show t}
   |]
  
main :: IO ()
main = warpDebug 3000 TaskWarrior


--- Data

data Task = Task
  { taskId          :: Integer
  , taskDescription :: Text
  , taskProject     :: Maybe Text
  , taskPriority    :: Maybe Priority
  , taskTags        :: [Tag]
  } deriving Show

newtype Tag = Tag Text

instance Show Tag where
  show (Tag s) = "+" ++ unpack s

showTags :: [Tag] -> String
showTags ts = intercalate ", " $ show <$> ts

data Priority = High | Medium | Low
              deriving Show
  

instance FromJSON Task where
  parseJSON (Object v) = do
    ident <- v .: "id"
    desc  <- v .: "description"
    proj  <- v .:? "project"
    pri   <- v .:? "priority"
    tags  <- v .:? "tags"
    return Task { taskId          = ident
                , taskDescription = desc
                , taskProject     = proj
                , taskPriority    = pri
                , taskTags        = [] `fromMaybe` tags
                }

instance FromJSON Priority where
  parseJSON (String s) = case s of
    "H" -> return High
    "M" -> return Medium
    "L" -> return Low
    
instance FromJSON Tag where
  parseJSON (String s) = return $ Tag s
  
    
-- natural transformation
class f :~> g where
  ntrans :: f a -> g a
  
instance T.Result :~> Maybe where
  ntrans (Error _) = Nothing
  ntrans (Success x) = Just x

instance T.Result :~> Either String where
  ntrans (Error x) = Left x
  ntrans (Success x) = Right x
  
instance (Either e) :~> [] where
  ntrans = rights . (:[])
  
getTasksIO :: IO [Task]
getTasksIO = do
  out <- readProcess "task" ["export"] ""
  let input = BS.pack $ "[" ++ out ++ "]"
  let tasks = case parse json input of 
        Done _ val -> fromJSON val
        _ -> mzero
  return . join .  ntrans $ (ntrans tasks :: Either String [Task])

filterOutZeroId :: [Task] -> [Task]
filterOutZeroId = filter $ (> 0) <$> taskId
