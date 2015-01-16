{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Types.Project where

import Control.Applicative
import Data.Aeson

import Stash.Types.Link
import Stash.Types.Links

data Project = Project
    { key           :: String
    , id            :: Int
    , name          :: String
    , public        :: Bool
    , projectType   :: String
    , link          :: Link
    , links         :: Links
    } deriving (Eq, Show)

instance FromJSON Project where
    parseJSON (Object x) = Project
        <$> x .: "key"
        <*> x .: "id"
        <*> x .: "name"
        <*> x .: "public"
        <*> x .: "type"
        <*> x .: "link"
        <*> x .: "links"
    parseJSON _ = fail "Expected an object!"

