{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main
       ( main
       )
       where

import           Control.Monad              (forM_)
import           Data.Data                  (Data)
import           Data.Typeable              (Typeable)

import           Control.Lens
import qualified Data.ByteString            as Strict (ByteString)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy       as BS
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.Default               (def)
import           Data.Time                  (UTCTime, addUTCTime,
                                             getCurrentTime)
import           Network.HTTP.Client        (Cookie (..), CookieJar)
import           Network.Wreq
import qualified Network.Wreq.Session       as S
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree     (flattenTree, tagTree)
import           Web.Cookie                 (SetCookie, setCookieDomain,
                                             setCookieName, setCookieValue)

main :: IO ()
main = do
  now <- getCurrentTime
  let options =
        defaults
          & header "Cookie" .~ ["Setting-205-Calendar Year=Last Month"]
          -- & cookie "Setting-205-Calendar Year" .~ mkCookie now "Setting-205-Calendar Year" "Last Month"
          -- & header "User-Agent" .~ ["Mozilla/5.0 (Windows NT 6.3; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36"]

  r <- getWith options "http://madison.legistar.com/Calendar.aspx"
  let body = r ^. responseBody
      tags = parseTags body
  forM_ (sections isRowStart tags) $ \section -> do
    let cells = sectionsTree (isTagOpenName "td") section
        committeeCell = cells !! 0
        dateCell = cells !! 1
        timeCell = cells !! 3
        locationCell = cells !! 4
        agendaCell = cells !! 5
        minutesCell = cells !! 6

    putStrLn $ mangledText committeeCell
    putStrLn $ "  " ++ mangledText dateCell
    putStrLn $ "  " ++ mangledText timeCell
    putStrLn $ "  " ++ mangledText locationCell
  putStrLn ""
  print $ r ^. responseCookieJar

data DateRange
  = ThisMonth
  | LastMonth
  | NextMonth
  deriving (Eq, Ord, Show, Read, Enum, Data, Typeable)

dateRangeValue ThisMonth = "This Month"
dateRangeValue LastMonth = "Last Month"
dateRangeValue NextMonth = "Next Month"

cookieForRange ::
  UTCTime -- ^ Now
  -> DateRange -- ^ Date range to select
  -> Cookie
cookieForRange now dt =
  mkCookie
    now
    "Setting-205-Calendar Year"
    (dateRangeValue dt)

mkCookie ::
  UTCTime -- ^ Now
  -> Strict.ByteString -- ^ Name
  -> Strict.ByteString -- ^ Value
  -> Cookie
mkCookie now name value =
  Cookie
   { cookie_name = name
   , cookie_value = value
   , cookie_expiry_time = (86400 * 365) `addUTCTime` now
   , cookie_domain = "madison.legistar.com"
   , cookie_path = "/"
   , cookie_creation_time = now
   , cookie_last_access_time = now
   , cookie_host_only = True
   , cookie_secure_only = False
   , cookie_http_only = False
   , cookie_persistent = True
   }


-- | Workes similar to the tagsoup 'sections' except we
-- include the *tree* whose parent element satisfies the
-- given predicate.
sectionsTree ::
  (Tag ByteString -> Bool)
  -> [Tag ByteString]
  -> [[Tag ByteString]]
sectionsTree p tags =
  map (flattenTree . take 1 . tagTree)
   $ sections p tags

mangledText :: [Tag ByteString] -> String
mangledText =
  unwords . words . B8.unpack . innerText

isRowStart :: Tag ByteString -> Bool
isRowStart tag =
  isTagOpenName "tr" tag &&
   (fromAttrib "class" tag == "rgRow" || fromAttrib "class" tag == "rgAltRow")
