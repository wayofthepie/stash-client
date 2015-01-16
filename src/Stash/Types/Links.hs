
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Types.Links where

import Control.Applicative
import Data.Aeson
import GHC.Generics

data Links = Links
    { clone :: Maybe [CloneInfo]
    , self  :: [Href]
    } deriving (Eq, Generic, Show)


data Href = Href { href :: String }
    deriving (Eq, Generic, Show)


data CloneInfo = CloneInfo
    { cloneHref :: String
    , cloneName :: String
    } deriving (Eq, Show)

instance FromJSON Links
instance FromJSON Href

instance FromJSON CloneInfo where
    parseJSON (Object x) = CloneInfo
        <$> x .: "href"
        <*> x .: "name"
    parseJSON _ = fail "Expecting an Object!"
