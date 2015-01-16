{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Stash.Types.PagedResponse where

import Data.Aeson
import GHC.Generics

import Stash.Types.Project  as P
import Stash.Types.Repo     as R

data PagedResponse a = PagedResponse
    { size      :: Int
    , limit     :: Int
    , isLastPage:: Bool
    , values    :: a
    , start     :: Maybe Int
    } deriving (Eq, Generic, Show)

instance FromJSON (PagedResponse [R.Repo])
instance FromJSON (PagedResponse [P.Project])

