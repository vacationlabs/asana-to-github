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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Char
import Data.Csv hiding (header, decode, Name)
import Data.Monoid
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
import GitHub.Data.Name
import Data.Vector
import qualified Data.Text as T
import Prelude hiding (id)

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
  , _storyCreated_at :: Maybe T.Text
  , _storyText :: Maybe T.Text
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

doImport :: [Task] -> IO [Github.Issue]
doImport tasks = Prelude.mapM importTask tasks

importTask :: Task -> IO Github.Issue
importTask task = do
  comments <- Prelude.filter (\a -> _storyType a == "comment") <$> getStoriesForTask task
  issue <- createGitHubIssue task
  Prelude.mapM (createComment issue) comments
--   case task ^. assignee of
--     Just dev -> addAssignee issue  (dev ^. name)
--     Nothing -> return ()
  return issue

getStoriesForTask :: Task -> IO [Story]
getStoriesForTask task = do
  r <- (getEndpoint [qc|/tasks/{task ^. id}/stories|]) 
  return $ _dataData $ fromJust $ decode $ r ^. responseBody

getEndpoint :: String -> IO (Response BSL.ByteString)
getEndpoint endpoint = let opts = defaults & header "Authorization" .~ [[qc|Bearer {asanaToken}|]]
  in getWith opts [qc|https://app.asana.com/api/1.0{endpoint}|]

createComment :: Github.Issue -> Story -> IO ()
createComment issue story = do
  let auth = Github.OAuth gitHubToken
  case _storyText story of 
    Just comment -> do
      Github.createComment auth repoUser repoName (Github.issueId issue) comment
      return ()
    Nothing -> return ()

createGitHubIssue :: Task -> IO (Github.Issue)
createGitHubIssue task = do
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
    Right issue -> return issue
    _           -> error "did not create issue"

addAssignee :: Github.Issue -> T.Text -> IO ()
addAssignee issue assignee = do
  let auth = Github.OAuth gitHubToken
      edit = Github.editOfIssue { Github.editIssueAssignee = Just $ N assignee }
  possibleIssue <- Github.editIssue auth repoUser repoName (Github.issueId issue) edit
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
