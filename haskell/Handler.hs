module Poli.Web.Widgets.TimeSelector.Handler (handler) where

import Control.Applicative (many, optional, some, (<|>))
import Data.Char (isNumber)
import Data.List (findIndices)
import qualified Data.Text as T
import Data.Text (Text, isPrefixOf)
import Data.Time
    ( addGregorianMonthsRollOver, addGregorianYearsRollOver, getCurrentTimeZone
    , utcToLocalTime
    )

import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import qualified Miso.Effect.DOM as DOM
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (alphaNumChar, letterChar, space)
import Text.Megaparsec.Char.Lexer (decimal)

import Poli.Web.Base.Client hiding (isPrefixOf)
import qualified Poli.Web.Widgets.Modal.State as M
import Poli.Web.Widgets.TimeSelector.State

handler :: String -> Action -> State -> Effect Action Output
handler k a s = case a of
    Output o -> pure o

    Load -> Effect None
        [ \sink -> (() <$) . runExceptT $ do
            t <- liftIO getCurrentTime
            timeZone <- liftIO getCurrentTimeZone
            let lt = utcToLocalTime timeZone t
            liftIO . sink . Modify $ sTime ?~ lt
                                 >>> sResults .~ recommendedResults lt
        ]

    ModalAction mo -> do
        case mo of
            M.Close -> pure Close
            M.None -> pure None

    Focus -> effectSub None $ \_ -> do
        DOM.focus (ms $ k <> "-input")

    Modify f -> pure . SetState $ f s

    NoOp -> pure None

    SearchTimes q' -> case (s ^. sTime, q') of
        (Nothing, _) -> pure None
        (Just lt, "") -> effectSub None $ \sink -> do
            liftIO . sink . Modify $ sResults .~ recommendedResults lt
                                 >>> sInput .~ ""
        (Just lt, q) -> effectSub None $ \sink -> (() <$) . runExceptT $ do
            let rs' = parseReminder lt (T.pack $ fromMisoString q)
            let rs = bool id (filter (\(_, lt') -> lt' > lt)) (s ^. sFuture) $ rs'
            liftIO . sink . Modify $ sResults .~ take 5 rs
                                 >>> sInput .~ q

    Select i -> effectSub None $ \sink -> (() <$) . runExceptT $ do
        let t' = snd <$> s ^? sResults . ix i
        case t' of
            Nothing -> liftIO . sink $ Output None
            Just t -> do
                liftIO . sink . Output $ Success t

data AMPM = AM | PM
data TimePeriod = Minute | Hour | Day | Week | Month | Year deriving Show

pers :: [(Text, TimePeriod)]
pers = [ ("minutes", Minute), ("hours", Hour), ("days", Day)
       , ("weeks", Week), ("months", Month), ("years", Year)
       ]

dows :: [Text]
dows = ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]

months :: [Text]
months = [ "january", "february", "march", "april", "may", "june", "july", "august"
         , "september", "october", "november", "december"
         ]

numbers :: [(Text, Int)]
numbers = [ ("an", 1), ("one", 1), ("first", 1), ("1st", 1)
          , ("two", 2), ("second", 2), ("2nd", 2), ("three", 3), ("third", 3), ("3rd", 3)
          , ("fourth", 4), ("4th", 4), ("five", 5), ("fifth", 5), ("5th", 5)
          , ("sixth", 6), ("6th", 6), ("seventh", 7), ("7th", 7), ("eighth", 8), ("8th", 8)
          , ("nine", 9), ("ninth", 9), ("9th", 9), ("tenth", 10), ("10th", 10)
          , ("eleventh", 11), ("11th", 11), ("twelve", 12), ("twelfth", 12), ("12th", 12)
          , ("thirteenth", 13), ("13th", 13), ("fourteenth", 14), ("14th", 14)
          , ("fifteenth", 15), ("15th", 15), ("sixteenth", 16), ("16th", 16)
          , ("seventeenth", 17), ("17th", 17), ("eighteenth", 18), ("18th", 18)
          , ("nineteenth", 19), ("19th", 19), ("twenty", 20), ("twentieth", 20), ("20th", 20)
          , ("twentyone", 21), ("twentyfirst", 21), ("21st", 21), ("twentytwo", 22)
          , ("twentysecond", 22), ("22nd", 22), ("twentythree", 23), ("twentythird", 23)
          , ("23rd", 23), ("twentyfourth", 24), ("24th", 24), ("twentyfive", 25)
          , ("twentyfifth", 25), ("25th", 25), ("twentysixth", 26), ("26th", 26)
          , ("twentyseventh", 27), ("27th", 27), ("twentyeighth", 28), ("28th", 28)
          , ("twentynine", 29), ("twentyninth", 29), ("29th", 29), ("thirty", 30)
          , ("thirtieth", 30), ("30th", 30), ("thirtyone", 31), ("thirtyfirst", 31), ("31st", 31)
          , ("forty", 40), ("fifty", 50), ("sixty", 60), ("seventy", 70), ("eighty", 80)
          , ("ninety", 90), ("hu", 100)
          ]

parseReminder :: LocalTime -> Text -> [(MisoString, LocalTime)]
parseReminder local t' = map (\(t, lt) -> (ms $ cleanPack t, lt)) rs
  where rs = concat . catMaybes $ map (\f -> parseMaybe f (cleanInput t'))
                [ tomorrowBeginP local, tomorrowEndP local, todayBeginP local, todayEndP local
                , tonightBeginP local, tonightEndP local, tmdyP, mdytP
                , tmdP local, mdtP local, mtP local
                , nextBeginP local, nextEndP local, nextDowBeginP local, durationP local
                ]

recommendedResults :: LocalTime -> [(MisoString, LocalTime)]
recommendedResults lt =
    [ ("tomorrow", LocalTime (addDays 1 $ localDay lt) $ TimeOfDay 8 0 0)
    , ("next week", LocalTime (addDays 7 $ localDay lt) $ TimeOfDay 8 0 0)
    , (ms . unwords . words $ formatTime defaultTimeLocale "%B %e at 4:56pm" twelve, twelve)
    ]
  where
    twelve = LocalTime (addDays 12 $ localDay lt) $ TimeOfDay 16 56 0

cleanInput :: Text -> Text
cleanInput t = T.unwords $ filter (\w -> isvalid w && w /= "on") wds
  where
    isvalid w = isdow w || isper w || ismonth w || isnumber w || istime w || keywords w
    isdow w = isJust $ find (\dow -> w `isPrefixOf` dow) dows
    isper w = isJust $ find (\(per, _) -> w `isPrefixOf` per) pers
    ismonth w = isJust $ find (\mo -> w `isPrefixOf` mo) months
    isnumber w = T.all isNumber w || (isJust $ find (\(num, _) -> w `isPrefixOf` num) numbers)
    istime w = not . null $ maybe [] id (parseMaybe timeP w)
    keywords w = w `isPrefixOf` "today" || w `isPrefixOf` "tomorrow" || w `isPrefixOf` "tonight"
              || w `isPrefixOf` "pm" || w `isPrefixOf` "am" || w `isPrefixOf` "next"
              || w `isPrefixOf` "in"
    wds = T.words $ T.map (\c -> bool ' ' c $ isAlphaNum c || c == ':') (T.toLower t)

cleanPack :: Text -> Text
cleanPack t = T.unwords $ T.words t

durationP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
durationP l = do
    _ <- optional ("in" <|> "i")
    space
    duration <- many alphaNumChar
    space
    per <- many letterChar
    space
    ts' <- optional timeP
    let ts = maybe [TimeOfDay 8 0 0] id ts'
    let ds = maybe [] (\x -> bool [] [x] $ x > 31) (readMaybe duration)
          ++ (map snd $ filter (\(num, _) -> T.pack duration `isPrefixOf` num) numbers)
    let ps = map snd $ filter (\(p, _) -> T.pack per `isPrefixOf` p) pers
    pure [ let lt = periodToTime l t d p
            in ("in " <> (T.pack $ show d) <> " " <> (periodToText p (d > 1))
                      <> " at " <> (formatTimeOfDay lt), lt
               )
           | d <- ds, t <- ts, p <- ps
         ]

periodToText :: TimePeriod -> Bool -> Text
periodToText tp plural = case tp of
    Minute -> bool "minute" "minutes" plural
    Hour -> bool "hour" "hours" plural
    Day -> bool "day" "days" plural
    Week -> bool "week" "weeks" plural
    Month -> bool "month" "months" plural
    Year -> bool "year" "years" plural

periodToTime :: LocalTime -> TimeOfDay -> Int -> TimePeriod -> LocalTime
periodToTime l tod c tp = case tp of
    Minute -> addMinutes l c
    Hour -> addHours l c
    Day -> LocalTime (addDays (toInteger c) $ localDay l) tod
    Week -> LocalTime (addDays (7 * (toInteger c)) $ localDay l) tod
    Month -> LocalTime (addGregorianMonthsRollOver (toInteger c) $ localDay l) tod
    Year -> LocalTime (addGregorianYearsRollOver (toInteger c) $ localDay l) tod

nextBeginP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
nextBeginP l = do
    next <- some letterChar
    space
    per <- many letterChar
    space
    ts' <- optional timeP
    let ts = maybe [TimeOfDay 8 0 0] id ts'
    if T.pack next `isPrefixOf` "next"
    then pure $ getNextTimes l (T.pack per) ts
    else pure []

nextEndP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
nextEndP l = do
    ts <- timeP
    space
    next <- some letterChar
    space
    per <- many letterChar
    if T.pack next `isPrefixOf` "next"
    then pure $ getNextTimes l (T.pack per) ts
    else pure []

getNextTimes :: LocalTime -> Text -> [TimeOfDay] -> [(Text, LocalTime)]
getNextTimes l per ts = if
    | per == "" -> map nw ts <> map nm ts <> map ny ts
    | per `isPrefixOf` "week" -> map nw ts
    | per `isPrefixOf` "month" -> map nm ts
    | per `isPrefixOf` "year" -> map ny ts
    | otherwise -> []
  where
    ld = localDay l
    nwt t = LocalTime (addDays 7 ld) t
    nw t = ("next week at " <> formatTimeOfDay (nwt t), nwt t)
    nmt t = LocalTime (addGregorianMonthsRollOver 1 ld) t
    nm t = ("next month at " <> formatTimeOfDay (nmt t), nmt t)
    nyt t = LocalTime (addGregorianYearsRollOver 1 ld) t
    ny t = ("next year at " <> formatTimeOfDay (nyt t), nyt t)

nextDowBeginP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
nextDowBeginP l = do
    next <- some letterChar
    space
    per <- many letterChar
    space
    ts' <- optional timeP
    let ts = maybe [TimeOfDay 8 0 0] id ts'
    if T.pack next `isPrefixOf` "next"
    then pure $ getNextDowTimes l (T.pack per) ts
    else pure []

getNextDowTimes :: LocalTime -> Text -> [TimeOfDay] -> [(Text, LocalTime)]
getNextDowTimes l per ts = if
    | per == "" -> ndows [1..7]
    | otherwise -> let ds = map (+1) (findIndices (\dow -> per `isPrefixOf` dow) dows) in ndows ds
  where
    ndows ds = [ let lt = LocalTime (addDays (toInteger $ diff d) (localDay l)) t
                 in ("next " <> (T.toTitle $ dows !! (d - 1)) <> " at " <> formatTimeOfDay lt, lt)
                 | d <- ds, t <- ts
               ]
    diff d = let (_, _, dow) = toWeekDate (localDay l) in bool (7 + d - dow) (d - dow) (d > dow)

todayBeginP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
todayBeginP l = do
    today <- some letterChar
    space
    ts' <- optional timeP
    if (T.pack today) `isPrefixOf` "today"
    then case ts' of
        Nothing -> let lt = LocalTime (localDay l)
                                      (TimeOfDay (2 + (todHour $ localTimeOfDay l)) 0 0)
                    in pure [("today at " <> formatTimeOfDay lt, lt)]
        Just ts -> pure $ map (\t -> let lt = LocalTime (localDay l) t
                                      in ("today at " <> formatTimeOfDay lt, lt)
                              ) ts
    else pure []

todayEndP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
todayEndP l = do
    ts <- timeP
    today <- some letterChar
    if (T.pack today) `isPrefixOf` "today"
    then pure $ map (\t -> let lt = LocalTime (localDay l) t
                            in ("at " <> formatTimeOfDay lt <> " today", lt)
                    ) ts
    else pure []

tomorrowBeginP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
tomorrowBeginP l = do
    tomorrow <- some letterChar
    space
    ts' <- optional timeP
    if (T.pack tomorrow) `isPrefixOf` "tomorrow"
    then case ts' of
        Nothing -> pure [ ("tomorrow at 8 am"
                          , LocalTime (addDays 1 $ localDay l) (TimeOfDay 8 0 0)
                          )
                        ]
        Just ts -> pure $ map (\t -> let lt = LocalTime (addDays 1 $ localDay l) t
                                      in ("tomorrow at " <> formatTimeOfDay lt, lt)
                              ) ts
    else pure []

tomorrowEndP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
tomorrowEndP l = do
    ts <- timeP
    tomorrow <- some letterChar
    if (T.pack tomorrow) `isPrefixOf` "tomorrow"
    then pure $ map (\t -> let lt = LocalTime (addDays 1 $ localDay l) t
                            in ("at " <> formatTimeOfDay lt <> " tomorrow", lt)
                    ) ts
    else pure []

tonightBeginP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
tonightBeginP l = do
    tn <- some letterChar
    space
    ts' <- optional timeP
    if (T.pack tn) `isPrefixOf` "tonight"
    then case ts' of
        Nothing -> let lt = LocalTime (localDay l)
                                      (TimeOfDay (max 20 $ 2 + (todHour $ localTimeOfDay l)) 0 0)
                    in pure [( "tonight at " <> formatTimeOfDay lt, lt)]
        Just ts -> pure $ map (\t -> let lt = LocalTime (localDay l) t
                                      in ("tonight at " <> formatTimeOfDay lt, lt)
                              ) ts
    else pure []

tonightEndP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
tonightEndP l = do
    ts <- timeP
    tn <- some letterChar
    if (T.pack tn) `isPrefixOf` "tonight"
    then pure $ map (\t -> let lt = LocalTime (localDay l) t
                            in ("at " <> formatTimeOfDay lt <> " tonight", lt)
                    ) ts
    else pure []

tmdP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
tmdP l = do
    ts <- timeP
    space
    mos <- monthP
    space
    ds <- dayP
    space
    let yr = let (y, _, _) = toGregorian (localDay l) in y
    let crosses = [(t, d, mo, y) | d <- ds, t <- ts, mo <- mos, y <- [yr, yr + 1, yr + 2]]
    pure $ map (\( t, d, mo, y
                 ) -> let lt = LocalTime (fromGregorian (fromIntegral y) mo d) t
                       in ( "at " <> formatTimeOfDay lt <> " on "
                                  <> (T.pack $ formatTime defaultTimeLocale "%B %e, %Y" lt)
                          , lt
                          )
               ) crosses

tmdyP :: Parsec () Text [(Text, LocalTime)]
tmdyP = do
    ts <- timeP
    space
    mos <- monthP
    space
    ds <- dayP
    space
    my <- yearP
    let crosses = [(t, d, mo, y) | d <- ds, t <- ts, mo <- mos, y <- toList my]
    pure $ map (\( t, d, mo, y
                 ) -> let lt = LocalTime (fromGregorian (fromIntegral y) mo d) t
                       in ( "at " <> formatTimeOfDay lt <> " on "
                                  <> (T.pack $ formatTime defaultTimeLocale "%B %e, %Y" lt)
                          , lt
                          )
               ) crosses

mtP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
mtP l = do
    mos <- monthP
    space
    ts' <- optional timeP
    let ts = maybe [TimeOfDay 8 0 0] id ts'
    let yr = let (y, _, _) = toGregorian (localDay l) in y
    let crosses = [(t, d, mo, y) | t <- ts, y <- [yr, yr + 1, yr + 2], d <- [1..31], mo <- mos]
    pure $ map (\( t, d, mo, y
                 ) -> let lt = LocalTime (fromGregorian (fromIntegral y) mo d) t
                       in ( "on " <> (T.pack $ formatTime defaultTimeLocale "%B %e, %Y" lt)
                                  <> " at " <> formatTimeOfDay lt
                          , lt
                          )
               ) crosses

mdtP :: LocalTime -> Parsec () Text [(Text, LocalTime)]
mdtP l = do
    mos <- monthP
    space
    ds <- dayP
    space
    ts' <- optional timeP
    let ts = maybe [TimeOfDay 8 0 0] id ts'
    let yr = let (y, _, _) = toGregorian (localDay l) in y
    let crosses = [(t, d, mo, y) | d <- ds, t <- ts, mo <- mos, y <- [yr, yr + 1, yr + 2]]
    pure $ map (\( t, d, mo, y
                 ) -> let lt = LocalTime (fromGregorian (fromIntegral y) mo d) t
                       in ( "on " <> (T.pack $ formatTime defaultTimeLocale "%B %e, %Y" lt)
                                  <> " at " <> formatTimeOfDay lt
                          , lt
                          )
               ) crosses

mdytP :: Parsec () Text [(Text, LocalTime)]
mdytP = do
    mos <- monthP
    space
    ds <- dayP
    space
    my <- yearP
    space
    ts' <- optional timeP
    let ts = maybe [TimeOfDay 8 0 0] id ts'
    let crosses = [(t, d, mo, y) | d <- ds, t <- ts, mo <- mos, y <- toList my]
    pure $ map (\( t, d, mo, y
                 ) -> let lt = LocalTime (fromGregorian (fromIntegral y) mo d) t
                       in ( "on " <> (T.pack $ formatTime defaultTimeLocale "%B %e, %Y" lt)
                                  <> " at " <> formatTimeOfDay lt
                          , lt
                          )
               ) crosses

formatTimeOfDay :: LocalTime -> Text
formatTimeOfDay lt = T.pack $ formatTime defaultTimeLocale
                                         (bool "%l:%M %P" "%l %P" $ (todMin (localTimeOfDay lt) == 0))
                                         lt

dayP :: Parsec () Text [Int]
dayP = do
    dP <- (\d -> snd <$> filter (\(day, _) -> (T.pack d) `isPrefixOf` day) numbers
          ) <$> some alphaNumChar
    space
    pure (foldMap (\x -> bool [] [x] $ x > 0 && x < 32) dP)

monthP :: Parsec () Text [Int]
monthP = do
    let mP = (\m -> (\x -> x + 1) <$> findIndices (\mo -> (T.pack m) `isPrefixOf` mo) months
             ) <$> many letterChar
    let mNumP = (\d -> [d]) <$> decimal
    xs <- mNumP <|> mP
    space
    pure $ filter (\x -> x > 0 && x < 13) xs

yearP :: Parsec () Text (Maybe Int)
yearP = do
    x <- decimal
    space
    pure (bool Nothing (Just x) $ x > 2019 && x < 2177)

timeP :: Parsec () Text [TimeOfDay]
timeP = do
    x <- decimal
    _ <- optional ":"
    y' <- optional decimal
    let y = maybe 0 id y'
    space
    ampm <- ampmP
    pure $ processTimeOfDays x y ampm

ampmP :: Parsec () Text (Maybe AMPM)
ampmP = amP <|> pmP <|> defaultP
  where
    amP = Just AM <$ ("a" <|> "A") <* optional ("m" <|> "M") <* space
    pmP = Just PM <$ ("p" <|> "P") <* optional ("m" <|> "M") <* space
    defaultP = pure Nothing

addMinutes :: LocalTime -> Int -> LocalTime
addMinutes lt l = if m + l >= 60
    then addHours (LocalTime (localDay lt) (TimeOfDay h (m + l - 60 * hours) p)) hours
    else LocalTime (localDay lt) (TimeOfDay h (m + l) p)
  where
    hours = (m + l) `div` 60
    TimeOfDay h m p = localTimeOfDay lt

addHours :: LocalTime -> Int -> LocalTime
addHours lt l = if h + l > 23
    then LocalTime (addDays (toInteger days) $ localDay lt) (TimeOfDay (h + l - 24 * days) m p)
    else LocalTime (localDay lt) (TimeOfDay (h + l) m p)
  where
    days = (h + l) `div` 24
    TimeOfDay h m p = localTimeOfDay lt

processTimeOfDays :: Int -> Int -> Maybe AMPM -> [TimeOfDay]
processTimeOfDays x y ampm = case ampm of
    Nothing -> if
        | x >= 1 && x <= 5 -> [TimeOfDay (x + 12) y 0, TimeOfDay x y 0]
        | x >= 6 && x < 12 -> [TimeOfDay x y 0, TimeOfDay (x + 12) y 0]
        | x == 12 -> [TimeOfDay 12 y 0, TimeOfDay 0 y 0]
        | x > 13 && x <= 23 -> [TimeOfDay x y 0]
        | otherwise -> []
    Just AM -> if
        | x < 0 || x > 12 -> []
        | x == 12 -> [TimeOfDay 0 y 0]
        | otherwise -> [TimeOfDay x y 0]
    Just PM -> if
        | x < 0 || x > 12 -> []
        | x == 12 -> [TimeOfDay 12 y 0]
        | otherwise -> [TimeOfDay (x + 12) y 0]
