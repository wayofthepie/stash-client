{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Types.Repo where

import Data.Aeson
import GHC.Generics

import Stash.Types.Link
import Stash.Types.Links
import Stash.Types.Project

data Repo = Repo
    { slug          :: String
    , id            :: Int
    , name          :: String
    , state         :: String
    , statusMessage :: String
    , forkable      :: Bool
    , project       :: Project
    , public        :: Bool
    , link          :: Link
    , cloneUrl      :: String
    , links         :: Links
    } deriving (Eq, Generic, Show)


instance FromJSON Repo


