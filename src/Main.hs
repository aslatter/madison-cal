{-# LANGUAGE OverloadedStrings #-}

module Main
       ( main
       )
       where

import           Control.Monad              (forM_)
import           Data.Monoid                (mappend)

import           Control.Lens
import qualified Data.ByteString            as Strict
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Network.Wreq
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Tree     (flattenTree, tagTree)

main :: IO ()
main =
  withManager $ \baseOpts -> do

  mapM_ (dumpOneMonth baseOpts)
    [ "Last Month"
    , "ThisMonth"
    , "Next Month"
    ]

dumpOneMonth :: Options -> Strict.ByteString -> IO ()
dumpOneMonth baseOpts month = do
  let opts =
        baseOpts & header "Cookie" .~ ["Setting-205-Calendar Year=" `mappend` month]

  r <- getWith opts "http://madison.legistar.com/Calendar.aspx"
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
