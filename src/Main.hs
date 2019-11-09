module Main where

import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock
import           Network.HTTP
import           System.Environment
import           System.Exit
import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match
import           Text.Read


-- Supports the following methods:
--   list: show all pickup dates
--   next: get the next pickup date (optionally with a type)
--   next cat: get the next pickup date in pickup category
--   version: show version
main :: IO ()
main = do
    progName <- getProgName
    args     <- getArgs
    today    <- getCurrentTime >>= return . utctDay
    case args of
        ("list" : streetName : streetNum : []) -> tryWithStreetNum
            progName
            streetNum
            (bremerAbfallkalender List streetName)

        ("next" : streetName : streetNum : []) -> tryWithStreetNum
            progName
            streetNum
            (bremerAbfallkalender (Next today) streetName)

        ("next" : catStr : streetName : streetNum : []) ->
            case readMaybe $ normalizeCategory catStr :: Maybe Category of
                Just cat -> tryWithStreetNum
                    progName
                    streetNum
                    (bremerAbfallkalender (NextOf today cat) streetName)
                Nothing -> die $ usageError
                    ("invalid category for next '" ++ catStr ++ "'")
                    progName

        (arg : xs) | (arg == "-h" || arg == "--help" || arg == "help") -> do
            putStrLn $ usage progName
            exitSuccess

        (arg : xs) | (arg == "-v" || arg == "--version" || arg == "version") ->
            do
                putStrLn $ version progName
                exitSuccess

        _ -> die $ usage progName
  where
    usage progName =
        "Usage: "
            ++ progName
            ++ " OPERATION STREET_NAME STREET_NUMBER\n"
            ++ "\tOPERATION        list, next, next PICKUP_CATEGORY\n"
            ++ "\tPICKUP_CATEGORY  rest[müll], bio[abfall], pap[ier], gelber[sack], tannen[baumabfuhr]\n"

    usageError msg progName = "Error: " ++ msg ++ "\n" ++ usage progName

    version progName = progName ++ " v0.1"

    -- Runs the method with the streetnum if it is one, otherwise
    -- returns error about invalid street num
    tryWithStreetNum :: String -> String -> (Int -> IO ()) -> IO ()
    tryWithStreetNum progName s fn = case readMaybe s :: Maybe Int of
        Just streetNum -> fn streetNum
        Nothing ->
            die $ usageError ("invalid street number '" ++ s ++ "'") progName


bremerAbfallkalender :: Operation -> String -> Int -> IO ()
bremerAbfallkalender op streetName streetNum = do
    page <- getCalendar streetName streetNum
    let tags           = parseTags page
    let allPickupDates = parsePage tags
    let pickupDates    = performOperation op allPickupDates
    if length pickupDates > 0
        then mapM_ (putStrLn . show) pickupDates
        else putStrLn "No pickup dates found"


performOperation :: Operation -> [PickupDate] -> [PickupDate]
performOperation (Next day) dates
    = take 1
    . filter (`isAfter` day)
    $ dates
performOperation (NextOf day cat) dates
    = performOperation (Next day)
    $ filter (`hasCategory` cat) dates
performOperation List dates = dates


-----------
-- Model --
-----------

-- There are three operations
--   - List         show all pickup dates
--   - Next         get the next pickup date after a certain date
--   - NextOf cat   get the next pickup date in given category
data Operation
    = List
    | Next Day
    | NextOf Day Category
    deriving (Show)


-- A date on which garbage or recycling will be picked up
data PickupDate = PickupDate Day [Category]


-- The type of pickup
data Category
    = Restmuell
    | Bioabfall
    | Papier
    | GelberSack
    | Tannenbaumabfuhr
    | Other String
    deriving (Eq, Ord, Read)


--  A year is just an int
type Year = Integer


instance Show PickupDate where
  show (PickupDate day cats) =
        let catStr = intercalate ", " (map show cats)
        in show day ++ " - " ++ catStr


instance Show Category where
  show Restmuell        = "Restmüll"
  show Papier           = "Papier"
  show Bioabfall        = "Bioabfall"
  show GelberSack       = "Gelber Sack"
  show Tannenbaumabfuhr = "Tannenbaumabfuhr"
  show (Other str)      = '*' : str


isAfter :: PickupDate -> Day -> Bool
isAfter (PickupDate a _) b = a >= b


hasCategory :: PickupDate -> Category -> Bool
hasCategory (PickupDate _ cats) cat = cat `elem` cats

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
parsePickupDate year str
    = let
          (day, month) = parseDayMonth str
          date         = fromGregorian year month day
          isIrregular  = head str == '('
          infoStr      = if isIrregular
              then drop 9 . dropWhile (/= ')') $ str
              else drop 7 str
          cats =
              map (\s -> fromMaybe (Other s) (readMaybe s))
                  . map normalizeCategory
                  $ split infoStr '/'
      in
          PickupDate date cats


-- The day and month are provided in the form DD.MM. (e.g. 31.10.)
parseDayMonth :: String -> (Int, Int)
parseDayMonth s =
    let digits = filter isDigit s
    in  (read $ take 2 digits :: Int, read $ take 2 . drop 2 $ digits :: Int)


-- Categories are shortened in the list and in order to use the
-- derived Read instance of Cateogry we need to normalize them
-- e.g. Restm. -> Restmuell, Restmüll -> Restmuell, Bioabf. -> Bioabfall, etc
normalizeCategory :: String -> String
normalizeCategory = translate . capitalized . trim
  where
    translate "Restmüll"    = "Restmuell"
    translate "Restm."      = "Restmuell"
    translate "Rest"        = "Restmuell"
    translate "Bioabf."     = "Bioabfall"
    translate "Bio"         = "Bioabfall"
    translate "Gelbersack"  = "GelberSack"
    translate "Gelber sack" = "GelberSack"
    translate "Gelber"      = "GelberSack"
    translate "Sack"        = "GelberSack"
    translate "Tannen"      = "Tannenbaumabfuhr"
    translate "Pap"         = "Papier"
    translate x             = x


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
getCalendar :: String -> Int -> IO String
getCalendar streetName streetNum =
    let params = urlEncodeVars
            [("strasse", streetName), ("hausnummer", show streetNum)]
        url = "http://213.168.213.236/bremereb/bify/bify.jsp?" ++ params
    in  getResponseBody =<< simpleHTTP (getRequest url)


-----------
-- Utils --
-----------

-- Trim whitespace from beginning and end
-- via https://stackoverflow.com/a/38283069
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


-- Split a string based on a delimiter character
-- via https://jmmv.dev/2006/08/split-function-in-haskell.html
split :: String -> Char -> [String]
split [] delim = [""]
split (c : cs) delim | c == delim = "" : rest
                     | otherwise  = (c : head rest) : tail rest
    where rest = split cs delim


-- Capitalizes a string
-- via https://stackoverflow.com/a/20093585
capitalized :: String -> String
capitalized (head : tail) = toUpper head : map toLower tail
capitalized []            = []

