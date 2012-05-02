{-# LANGUAGE OverloadedStrings #-}

module TaskWarriorWeb.Data where
import Yesod

import Data.Aeson
import qualified Data.Aeson.Types as T
import Control.Applicative
import Data.Text (Text,unpack)
import Data.List (intercalate)
import Data.Maybe

data Task = Task
  { taskId          :: Integer
  , taskDescription :: Text
  , taskProject     :: Maybe Text
  , taskPriority    :: Maybe Priority
  , taskTags        :: [Tag]
  , taskStarted     :: Bool
  }

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
    strtd <- v .:? "start" :: T.Parser (Maybe Text)
    return Task { taskId          = ident
                , taskDescription = desc
                , taskProject     = proj
                , taskPriority    = pri
                , taskTags        = [] `fromMaybe` tags
                , taskStarted     = maybe False (const True) strtd
                }

instance FromJSON Priority where
  parseJSON (String s) = case s of
    "H" -> return High
    "M" -> return Medium
    "L" -> return Low
    
instance FromJSON Tag where
  parseJSON (String s) = return $ Tag s
  
