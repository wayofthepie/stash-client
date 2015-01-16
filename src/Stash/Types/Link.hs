
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Stash.Types.Link where

import Data.Aeson
import GHC.Generics

data Link = Link
    { url :: String
    , rel :: String
    } deriving (Eq, Generic, Show)

instance FromJSON Link


