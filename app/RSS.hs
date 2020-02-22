module RSS where

import Data.Maybe (fromJust)
import Data.List (find)
import Data.Time
import Text.XML.Light


data Item = Item {
    itTitle :: String,
    itLink :: String,
    itPubDate :: UTCTime
}

data Channel = Channel {
    chTitle :: String,
    chDescription :: String,
    chItems :: [Item]
}

instance Show Item where
    show (Item title link _) = title ++ "\n" ++ link ++ "\n"

instance Show Channel where
    show (Channel title description items) =
        fullTitle ++ "\n" ++ content where
            fullTitle = title ++ " - " ++ description
            content = unlines $ map show items


findRoot :: [Content] -> Maybe Element
findRoot = findRoot' . onlyElems
    where findRoot' = find $ (== QName "rss" Nothing Nothing) . elName


prop :: Element -> String -> String
prop node name = strContent . fromJust $ findChild (QName name Nothing Nothing) node


parseItem :: Element -> Item
parseItem node = Item { itTitle = title, itLink = link, itPubDate = pubDate }
    where title = prop node "title"
          link = prop node "link"
          pubDate = parseTimeOrError False defaultTimeLocale rfc822DateFormat (prop node "pubDate")


parseChannel :: Element -> Channel
parseChannel node = Channel { chTitle = title, chDescription = desc, chItems = items }
    where title = prop node "title"
          desc = prop node "description"
          items = map parseItem $ findChildren (QName "item" Nothing Nothing) node


parseChannels :: Element -> [Channel]
parseChannels node = map parseChannel $ findChildren (QName "channel" Nothing Nothing) node