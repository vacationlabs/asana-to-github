{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Lens
import Control.Lens.TH
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.ByteString as BS
import Data.Char
import Data.Csv
import Data.Monoid
import GHC.Generics (Generic)

fieldNamer = (aesonPrefix lcaseFirst)
  where
    lcaseFirst :: String -> String
    lcaseFirst [] = []
    lcaseFirst (x:xs) = (toLower x) : xs

-- *Main Data.Aeson> decode "{\"id\": 12312, \"name\": \"asdasd\"}" :: Maybe Tag
data Developer = Developer
  { _developerId :: Int
  , _developerName :: String
  } deriving (Generic, Show)

instance FromJSON Developer where
  parseJSON v = genericParseJSON fieldNamer v

data Project = Project
  { _projectId :: Int
  , _projectName :: String
  } deriving (Generic, Show)

instance FromJSON Project where
  parseJSON v = genericParseJSON fieldNamer v

data Section = Section
  { _sectionId :: Int
  , _sectionName :: String
  } deriving (Generic, Show)

instance FromJSON Section where
  parseJSON v = genericParseJSON fieldNamer v

data Tag = Tag
  { _tagId :: Int
  , _tagName :: String
  } deriving (Generic, Show)

instance FromJSON Tag where
  parseJSON v = genericParseJSON fieldNamer v

data Workspace = Workspace
  { _workspaceId :: Int
  , _workspaceName :: String
  } deriving (Generic, Show)

instance FromJSON Workspace where
  parseJSON v = genericParseJSON fieldNamer v

data Membership = Membership
  { _membershipProject :: Project
  , _membershipSection :: Maybe Section
  } deriving (Generic, Show)

instance FromJSON Membership where
  parseJSON v = genericParseJSON fieldNamer v

data Task = Task
  { _taskId :: Int
  , _taskAssignee :: Maybe Developer
  , _taskAssignee_status :: Maybe String
  , _taskCompleted :: Maybe Bool
  , _taskCompleted_at :: Maybe String
  , _taskCreated_at :: Maybe String
  , _taskDue_at :: Maybe String
  , _taskDue_on :: Maybe String
  , _taskFollowers :: Maybe [Developer]
  , _taskHearted :: Maybe Bool
  , _taskHearts :: Maybe Value
  , _taskMemberships :: Maybe [Membership]
  , _taskModified_at :: Maybe String
  , _taskName :: String
  , _taskNotes :: Maybe String
  , _taskNum_hearts :: Maybe Int
  , _taskParent :: Maybe Task
  , _taskProjects :: Maybe [Project]
  , _taskSubtasks :: Maybe [Task]
  , _taskTags :: Maybe [Tag]
  , _taskWorkspace :: Maybe Workspace
  } deriving (Generic, Show)

data Data = Data
  { _dataData :: [Task]
  } deriving (Generic, Show)

makeLensesWith abbreviatedFields ''Task

makeLensesWith abbreviatedFields ''Tag

instance FromJSON Data where
  parseJSON v = genericParseJSON fieldNamer v

instance FromJSON Task where
  parseJSON v = genericParseJSON fieldNamer v

instance ToField Tag where
  toField tag = toField $ tag ^. name

instance ToField [Tag] where
  toField tags = BS.intercalate "," (toField <$> tags)

instance ToRecord Task where
  toRecord task = record [toField (task ^. name), toField (task ^. notes), toField (task ^. tags)]
