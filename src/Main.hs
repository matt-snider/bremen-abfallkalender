module Main where

import           Data.Char
import           Data.Time
import           Network.HTTP
import           System.Environment
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match


main :: IO ()
main = do
    (street : streetNum : xs) <- getArgs
    src                       <- getCalendar street streetNum
    let tagsByYear  = splitByYear $ parseTags src
    -- TODO: this is wrong, we need to extract the year
    -- from each entry in the list and then extract pickupdates
    -- e.g. map (\x -> extractPickupDates (extractYear x) x)
    let pickupDates = extractPickupDates 2018 $ head tagsByYear
    mapM_ (putStrLn . show) pickupDates


-----------
-- Model --
-----------

-- A date on which garbage or recycling will be picked up
data PickupDate = PickupDate Day Type

-- The type of pickup (e.g. Bio / Müll, Recyling)
type Type = String

--  A year is just an int
type Year = Integer

-- Implement show to pretty print
instance Show PickupDate where
  show (PickupDate day pickupType) = show day ++ " - " ++ pickupType


-------------
-- Parsing --
-------------

-- Extracts the pickup dates for a single year
extractPickupDates :: Year -> [Tag String] -> [PickupDate]
extractPickupDates year tags =
    map makeEntry
        . filter (not . isMonth . takeWhile (/= ' '))
        . filter (/= "")
        . map stringify
        $ partitions (~== "<nobr>") tags
  where
    makeEntry txt =
        let (day, month) = parseDayMonth txt
            pickupType   = dropWhile (== ' ') . dropWhile (/= ' ') $ txt
        in  PickupDate (fromGregorian year month day) pickupType


-- Splits a list of tags into lists by year
-- NOTE: we filter out <b> because that corresponds to the month labels
-- e.g. <b>Januar 2019</b>
splitByYear :: [Tag String] -> [[Tag String]]
splitByYear tags = map (filter (~/= "<b>")) $ partitions isYearTag tags


-- Year tags have the 'bakY' class and colspan 2
isYearTag :: Tag String -> Bool
isYearTag = (~== "<td class=bakY colspan=2>")


-- The day and month are provided in the form DD.MM.
-- e.g. 31.10.
parseDayMonth :: String -> (Int, Int)
parseDayMonth s =
    let digits = filter isDigit s
    in  (read $ take 2 digits :: Int, read $ take 2 . drop 2 $ digits :: Int)


-- Extract the text from a list of tags
stringify :: [Tag String] -> String
stringify = unwords . words . innerText


-- Is it a month?
isMonth :: String -> Bool
isMonth "Januar"    = True
isMonth "Februar"   = True
isMonth "März"      = True
isMonth "April"     = True
isMonth "Mai"       = True
isMonth "Juni"      = True
isMonth "Juli"      = True
isMonth "August"    = True
isMonth "September" = True
isMonth "Oktober"   = True
isMonth "November"  = True
isMonth "Dezember"  = True
isMonth _           = False


----------
-- HTTP --
----------

-- Given a street and street number, make a request
getCalendar :: String -> String -> IO String
getCalendar street streetNum =
    let params = urlEncodeVars [("strasse", street), ("hausnummer", streetNum)]
        url    = "http://213.168.213.236/bremereb/bify/bify.jsp?" ++ params
    in  getResponseBody =<< simpleHTTP (getRequest url)
