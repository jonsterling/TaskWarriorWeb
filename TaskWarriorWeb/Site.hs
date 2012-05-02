{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module TaskWarriorWeb.Site where
import TaskWarriorWeb.Data
import TaskWarriorWeb.Task

import Yesod
import Data.Maybe (fromMaybe)
import Data.Text (Text)

data TaskWarrior = TaskWarrior
instance Yesod TaskWarrior

mkYesod "TaskWarrior" [parseRoutes|
/ HomeR GET
|]

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
     
   tr.started
     font-weight: bold
     font-style: italic
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
         <tr class=#{rowClass t}>
           <td>#{taskId t}
           <td>#{taskDescription t}
           <td>#{fromMaybe "" $ taskProject t}
           <td>#{maybe "" show $ taskPriority t} 
           <td>#{showTags $ taskTags t}
   |]
   where rowClass :: Task -> Text
         rowClass t = if taskStarted t then "started" else ""
