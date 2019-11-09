module Main where

import           Data.Char
import           Data.Maybe
import           Data.Time
import           Network.HTTP
import           System.Environment
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.Read


main :: IO ()
main = do
    (street : streetNum : xs) <- getArgs
    page                      <- getCalendar street streetNum
    let tags        = parseTags page
    let pickupDates = parsePage tags
    mapM_ (putStrLn . show) pickupDates


-----------
-- Model --
-----------

-- A date on which garbage or recycling will be picked up
data PickupDate = PickupDate Day PickupInfo

-- The type of pickup (e.g. Bio / Müll, Recyling)
type PickupInfo = String

--  A year is just an int
type Year = Integer

-- Implement show to pretty print
instance Show PickupDate where
  show (PickupDate day info) = show day ++ " - " ++ info


-------------
-- Parsing --
-------------

-- Parses the webpage into PickupDates
parsePage :: [Tag String] -> [PickupDate]
parsePage tags =
    [ parsePickupDate (fromJust year) pickupStr
    | section    <- splitByYear tags
    , pickupTags <- splitByPickup section
    , let year      = maybeParseYear section
    , let pickupStr = stringify pickupTags
    , shouldKeep year pickupStr
    ]
  where
    -- Filter out sections where the year could not be parsed
    -- and empty pickupStrs or month headers which are redundant
    shouldKeep :: Maybe Year -> String -> Bool
    shouldKeep year pickupStr =
        isJust year && pickupStr /= "" && (not $ containsMonth pickupStr)

    -- Partition the tags by the year in which they occur
    -- Year tags have the 'bakY' class and colspan 2
    -- The last section is additional info we don't want (e.g. Containerplätze)
    splitByYear :: [Tag String] -> [[Tag String]]
    splitByYear = init . partitions (~== "<td class=bakY colspan=2>")

    -- Partition the list of tags into sections each of which should contain the
    -- info for a single PickupDate
    splitByPickup :: [Tag String] -> [[Tag String]]
    splitByPickup = partitions (~== "<nobr>")

    -- The year should be the first 4 digits of a section
    maybeParseYear :: [Tag String] -> Maybe Year
    maybeParseYear tags =
        readMaybe $ take 4 . filter isDigit . innerText $ tags

    -- If the string begins with a month, then it is a month header that we
    -- don't need since that information is redundant (e.g. <b>Januar 2019</b>
    containsMonth :: String -> Bool
    containsMonth = isMonth . takeWhile (/= ' ')


-- Given the Year and a String with the pickup date info, returns a PickupDate.
-- The String only stores the day and month, which is why the Year must be provided.
-- Entries that begin with a day of the week in brackets indicate that the pickup
-- occurs on an irregular day of the week (e.g. '(Sa.)' when pickup is usually Di.)
parsePickupDate :: Year -> String -> PickupDate
parsePickupDate year str =
    let
        (day, month) = parseDayMonth str
        date         = fromGregorian year month day
        isIrregular  = head str == '('
        info         = if isIrregular
            then drop 9 . dropWhile (/= ')') $ str
            else drop 7 str
    in
        PickupDate date info


-- The day and month are provided in the form DD.MM. (e.g. 31.10.)
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

