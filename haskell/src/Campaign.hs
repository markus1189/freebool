{-# LANGUAGE OverloadedStrings #-}
module Campaign (Campaign
                ,hasKeyword
                ,isBefore
                ,isAfter
                ,RichCampaign
                ,isCyberMonday
                ,fromTo
                ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)

import           BoolAlg
import           FreeBool

data Campaign = HasKeyword Text
              | IsBefore UTCTime
              | IsAfter UTCTime
              deriving (Eq, Show)

hasKeyword = Inject . HasKeyword
isBefore = Inject . IsBefore
isAfter = Inject . IsAfter

data RichCampaign = IsCyberMonday
                  | IsBlackWeek
                  | FromTo UTCTime UTCTime
                  deriving (Eq, Show)

isCyberMonday = Inject IsCyberMonday
isBlackWeek = Inject IsBlackWeek
fromTo = Inject . FromTo

campaign = hasKeyword "cybermonday" `xor` hasKeyword "blackweek"

richCampaign = isCyberMonday `xor` isBlackWeek

foo = traverse go richCampaign
  where go :: RichCampaign -> Either _ Bool
        go IsCyberMonday = Right True
        go IsBlackWeek = Right True
        go p@(FromTo _ _) = Left (Inject p)
