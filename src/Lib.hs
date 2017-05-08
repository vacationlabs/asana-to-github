{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib where

import Control.Lens
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.Map as DM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Csv hiding (header, decode, Name)
import Data.Monoid
import Data.List
import GHC.Generics (Generic)
import Network.Wreq
import qualified Network.Wreq.Session as S
import Text.InterpolatedString.Perl6 (qc)
import Data.Maybe
import Config
import qualified GitHub.Auth   as Github
import qualified GitHub.Data.Issues as Github
import qualified GitHub.Endpoints.Issues as Github
import qualified GitHub.Endpoints.Issues.Comments as Github
import qualified GitHub.Data.Definitions as Github
import qualified GitHub.Data.Id as Id
import GitHub.Data.Name
import Data.Vector hiding (foldM, mapM_, mapM)
import qualified Data.Text as T
import Prelude hiding (id)
import qualified GitHub as GH
import Control.Exception
import Control.Retry
import Control.Monad
import Data.List.Split
import Control.Concurrent.Async

fieldNamer = (aesonPrefix lcaseFirst)
  where
    lcaseFirst :: String -> String
    lcaseFirst [] = []
    lcaseFirst (x:xs) = (toLower x) : xs

-- *Main Data.Aeson> decode "{\"id\": 12312, \"name\": \"asdasd\"}" :: Maybe Tag
data Developer = Developer
  { _developerId :: Int
  , _developerName :: T.Text
  } deriving (Generic, Show)

instance FromJSON Developer where
  parseJSON v = genericParseJSON fieldNamer v

data Project = Project
  { _projectId :: Int
  , _projectName :: T.Text
  } deriving (Generic, Show)

instance FromJSON Project where
  parseJSON v = genericParseJSON fieldNamer v

data Section = Section
  { _sectionId :: Int
  , _sectionName :: T.Text
  } deriving (Generic, Show)

instance FromJSON Section where
  parseJSON v = genericParseJSON fieldNamer v

data Tag = Tag
  { _tagId :: Int
  , _tagName :: T.Text
  } deriving (Generic, Show)

instance FromJSON Tag where
  parseJSON v = genericParseJSON fieldNamer v

data Workspace = Workspace
  { _workspaceId :: Int
  , _workspaceName :: T.Text
  } deriving (Generic, Show)

instance FromJSON Workspace where
  parseJSON v = genericParseJSON fieldNamer v

data Membership = Membership
  { _membershipProject :: Project
  , _membershipSection :: Maybe Section
  } deriving (Generic, Show)

data Story = Story
  { _storyCreated_by :: Developer
  , _storyHtml_text :: Maybe T.Text
  , _storyType :: T.Text
  } deriving (Generic, Show)

instance FromJSON Membership where
  parseJSON v = genericParseJSON fieldNamer v

instance FromJSON Story where
  parseJSON v = genericParseJSON fieldNamer v

data Task = Task
  { _taskId :: Int
  , _taskAssignee :: Maybe Developer
  , _taskAssignee_status :: Maybe T.Text
  , _taskCompleted :: Maybe Bool
  , _taskCompleted_at :: Maybe T.Text
  , _taskCreated_at :: Maybe T.Text
  , _taskDue_at :: Maybe T.Text
  , _taskDue_on :: Maybe T.Text
  , _taskFollowers :: Maybe [Developer]
  , _taskHearted :: Maybe Bool
  , _taskHearts :: Maybe Value
  , _taskMemberships :: Maybe [Membership]
  , _taskModified_at :: Maybe T.Text
  , _taskName :: T.Text
  , _taskNotes :: Maybe T.Text
  , _taskNum_hearts :: Maybe Int
  , _taskParent :: Maybe Task
  , _taskProjects :: Maybe [Project]
  , _taskSubtasks :: Maybe [Task]
  , _taskTags :: Maybe [Tag]
  , _taskWorkspace :: Maybe Workspace
  } deriving (Generic, Show)

data Data a = Data
  { _dataData :: [a]
  } deriving (Generic, Show)

makeLensesWith abbreviatedFields ''Developer

makeLensesWith abbreviatedFields ''Workspace

makeLensesWith abbreviatedFields ''Task

makeLensesWith abbreviatedFields ''Tag

instance (FromJSON a) => FromJSON (Data a) where
  parseJSON v = genericParseJSON fieldNamer v

instance FromJSON Task where
  parseJSON v = genericParseJSON fieldNamer v

instance ToField Tag where
  toField tag = toField $ tag ^. name

instance ToField [Tag] where
  toField tags = BS.intercalate "," (toField <$> tags)

instance ToRecord Task where
  toRecord task = record [toField (task ^. name), toField (task ^. notes), toField (task ^. tags)]

type TidIssueAssoc = DM.Map Int Github.Issue

collectTasks :: [Task] -> Int -> [Task]
collectTasks tasks taskId = collectTasks' taskId
  where
  findTask :: Int -> Task
  findTask taskId = case Data.List.find (\t -> t ^. id == taskId) tasks of
    Just task -> task
    Nothing -> error $ "Task not found" Prelude.++ (show taskId)
  collectTasks' :: Int -> [Task]
  collectTasks' taskId =
    let
      task = findTask taskId
      in task:(Prelude.concat $ collectTasks' <$> ((^. id) <$> (fromMaybe [] (task ^. subtasks))))

doImport :: [Task] -> IO [Github.Issue]
doImport tasks = do
  assoc <- importTasks tasks
  return assoc

linkParentTask :: Github.Issue -> [Github.Issue] -> IO (Maybe Github.Issue)
linkParentTask issue subissues = do
  let subIssueNumbers = T.intercalate ", " $ (T.pack.(\x -> '#':x).show.Github.issueNumber) <$> subissues
  putStrLn [qc|Linking parent task {Github.issueId issue} with {subIssueNumbers}|]
  appendMessage issue [qc|This task is a parent of {subIssueNumbers}|]

data CreatedIssueNotFoundException = CreatedIssueNotFoundException deriving (Show)

instance Exception CreatedIssueNotFoundException

safeLookupIssue :: TidIssueAssoc -> Task -> Github.Issue
safeLookupIssue map task = do
  let maybeIssue = DM.lookup (task ^. id) map
  case maybeIssue of
    Just issue -> issue
    Nothing -> throw CreatedIssueNotFoundException

appendMessage :: Github.Issue -> T.Text -> IO (Maybe Github.Issue)
appendMessage issue message = do
  let
    newMessage = (\oldmessage -> T.concat [message, "\n", oldmessage]) <$> Github.issueBody issue
    edit = Github.editOfIssue { Github.editIssueBody = newMessage }
  editIssue issue edit

importTasks :: [Task] -> IO [Github.Issue]
importTasks tasks = Prelude.concat <$> (mapM importTasks' $ chunksOf 40 tasks)
  where
  importTasks' :: [Task] -> IO [Github.Issue]
  importTasks' tasks = Prelude.concat <$> mapConcurrently importTask' tasks
    where
      importSubtask :: Github.Issue -> Task -> IO [Github.Issue]
      importSubtask parent task = do
        let msg = [qc|This is a sub task of #{Github.issueNumber parent}.|]
            newTask = task & notes .~ ((\note -> T.concat [msg, "\n", note]) <$> (task ^. notes))
          in importTask' newTask
      importTask' :: Task -> IO [Github.Issue]
      importTask' task = do
        maybeIssue <- importTask task
        case maybeIssue of
          Just issue -> case task ^. subtasks of
            Just subtasks_@(_:_) -> do
              subIssues <- Prelude.concat <$> mapM (importSubtask issue) subtasks_
              linkParentTask issue subIssues
              return $ (issue:subIssues)
            _ -> return [issue]
          Nothing -> return []

makeTaskUrl :: Task -> T.Text
makeTaskUrl task = [qc|https://app.asana.com/0/{(fromJust $ task ^. workspace) ^. id}/{task ^. id}|]

rtPolicy = (exponentialBackoff 1000000) <> (limitRetries 30)

importTask :: Task -> IO (Maybe Github.Issue)
importTask task = do
  putStrLn $ "Importing task" Prelude.++ (show (task ^. id))
  comments <- getCommentsForTask task 
  if task ^. name == "" then return Nothing else do
    possibleIssue <- createGitHubIssue $ prependOriginalUrl task
    case possibleIssue of
      Just issue -> do
        Prelude.mapM (createComment issue) comments
        case task ^. completed of
          Just isCompleted -> if isCompleted then closeIssue issue else return Nothing
          Nothing -> return Nothing
        return $ Just issue
      Nothing -> return Nothing

prependOriginalUrl :: Task -> Task
prependOriginalUrl task = task & notes .~ ((\note -> T.concat [(makeTaskUrl task), "\n", note]) <$> (task ^. notes))

getCommentsForTask :: Task -> IO [T.Text]
getCommentsForTask task = do
  maybeStories <- retrying rtPolicy (\rs c -> if isNothing c then return True else return False) $ getStoriesForTask task
  case maybeStories of
    Just stories -> return $ ((makeCommentText) <$> (Prelude.filter (\t -> _storyType t == "comment") stories))
    Nothing -> return ["Failed to fetch comments"]

makeCommentText :: Story -> T.Text
makeCommentText story = [qc|{(_storyCreated_by story ^. name :: T.Text)} : {fromMaybe "Comment content not found" (_storyHtml_text story)}|]
      
getStoriesForTask :: Task -> RetryStatus -> IO (Maybe [Story])
getStoriesForTask task rs = catch (do
  putStrLn $ "Getting stories : Attempt :"  Prelude.++  (show $ rsIterNumber rs)
  r <- (getEndpoint [qc|/tasks/{task ^. id}/stories?opt_fields=html_text,created_by.id,created_by.name,type|]) 
  return $ Just $ _dataData $ fromJust $ decode $ r ^. responseBody) handler 
  where
  handler :: SomeException -> IO (Maybe [Story])
  handler e = do
    putStrLn "Exception while fetching stories"
    putStrLn $ show e
    return Nothing

getEndpoint :: String -> IO (Response BSL.ByteString)
getEndpoint endpoint = let opts = defaults & header "Authorization" .~ [[qc|Bearer {asanaToken}|]]
  in getWith opts [qc|https://app.asana.com/api/1.0{endpoint}|]

createComment :: Github.Issue -> T.Text -> IO (Maybe GH.Comment)
createComment issue comment = 
  retrying rtPolicy (\rs c -> if isNothing c then return True else return False) $ createComment' issue comment

createComment' :: Github.Issue -> T.Text -> RetryStatus -> IO (Maybe GH.Comment)
createComment' issue comment rs = do
  putStrLn $ "Creating comment for issue" Prelude.++ (show (Github.issueId issue)) Prelude.++ " Attempt :"  Prelude.++  (show $ rsIterNumber rs)
  let auth = Github.OAuth gitHubToken
  putStrLn $ show $ (Github.issueId issue)
  putStrLn $ show $ comment
  possibleComment <- Github.createComment
    auth
    repoUser
    repoName 
    (Id.Id $ Github.issueNumber issue) -- The Api function expects issue Id here. But github expects issue number. 
    comment
  case possibleComment of
    Right comment -> return $ Just comment
    Left err      -> do
      putStrLn "Comment creation failed"
      putStrLn $ show $ err
      return Nothing

createGitHubIssue :: Task -> IO (Maybe Github.Issue)
createGitHubIssue task = retrying rtPolicy (\rs c -> if isNothing c then return True else return False) $ createGitHubIssue' task

createGitHubIssue' :: Task -> RetryStatus -> IO (Maybe Github.Issue)
createGitHubIssue' task rs = do
  putStrLn $ "Creating github issue : Attempt :"  Prelude.++  (show $ rsIterNumber rs)
  let
    auth = Github.OAuth gitHubToken
    newiss = Github.NewIssue
        { Github.newIssueTitle = task ^. name
        , Github.newIssueBody = task ^. notes
        , Github.newIssueAssignee = Nothing
        , Github.newIssueMilestone = Nothing
        , Github.newIssueLabels = Just $ makeNames $ task ^. tags
        }
  possibleIssue <- Github.createIssue auth repoUser repoName newiss
  putStrLn $ show possibleIssue
  case possibleIssue of
    Right issue -> return $ Just issue
    Left err           -> do
      putStrLn $ show err
      return Nothing

editIssue :: Github.Issue -> Github.EditIssue -> IO (Maybe Github.Issue)
editIssue issue edit = retrying rtPolicy (\rs c -> if isNothing c then return True else return False) $ editIssue' issue edit

editIssue' :: Github.Issue -> Github.EditIssue -> RetryStatus -> IO (Maybe Github.Issue)
editIssue' issue edit rs = do
  putStrLn $ "Editing issue" Prelude.++ (show (Github.issueId issue)) Prelude.++ "Attempt :"  Prelude.++  (show $ rsIterNumber rs)
  let auth = Github.OAuth gitHubToken
  possibleIssue <- Github.editIssue
    auth
    repoUser
    repoName
    (Id.Id $ Github.issueNumber issue) -- The Api function expects issue Id here. But github expects issue number. 
    edit
  putStrLn $ show possibleIssue
  case possibleIssue of
    Right issue -> return $ Just issue
    Left err           -> do
      putStrLn $ show err
      return Nothing

closeIssue :: Github.Issue -> IO (Maybe Github.Issue)
closeIssue issue = do
  putStrLn $ "Closing issue" Prelude.++ (show (Github.issueId issue))
  let edit = Github.editOfIssue { Github.editIssueState = Just GH.StateClosed }
  editIssue issue edit

addAssignee :: Github.Issue -> T.Text -> IO ()
addAssignee issue assignee = do
  let auth = Github.OAuth gitHubToken
      edit = Github.editOfIssue { Github.editIssueAssignee = Just $ N assignee }
  possibleIssue <- Github.editIssue
    auth
    repoUser
    repoName
    (Id.Id $ Github.issueNumber issue) -- The Api function expects issue Id here. But github expects issue number. 
    edit

  putStrLn $ show possibleIssue

makeNames :: (HasName a T.Text) => Maybe [a] -> Vector (Name b)
makeNames Nothing = fromList []
makeNames (Just a) = fromList ((N.(^. name)) <$> a)

(repoUser, repoName) = targetRepo

-- formatIssue issue =
--   (Github.simpleOwnerLogin $ Github.issueUser issue) ++
--     " opened this issue " ++
--     (show $ Github.fromDate $ Github.issueCreatedAt issue) ++ "\n" ++
--   (Github.issueState issue) ++ " with " ++
--       (show $ Github.issueComments issue) ++ " comments" ++ "\n\n" ++
--   (Github.issueTitle issue)
-- 

-- curl -H "Authorization: Bearer <personal_access_token>" \
-- https://app.asana.com/api/1.0/tasks/1001/stories
