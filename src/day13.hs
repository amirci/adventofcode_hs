module Day13 where

import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.MultiMap as MM

type HappinessChange = Int
type HappinessUnits = Int
type Person = String
type PersonHappiness = (HappinessUnits, Person)
type SeatingRequests = MM.MultiMap Person PersonHappiness

hpu :: PersonHappiness -> HappinessUnits
hpu = fst

person :: PersonHappiness -> Person
person = snd

maxHappiness :: SeatingRequests -> HappinessChange
maxHappiness req = maximum $ map tablePoints possibleSeats
  where 
    possibleSeats = permutations $ MM.keys req
    tablePoints seats = sum $ map sumPoints $ hc seats
    hc seats = zip3 seats (rotate 1 seats) (rotate 2 seats)
    hcLookup p1 p2 = hpu $ fromMaybe (0, "") $ find ((== p2) . person) $ MM.lookup p1 req
    sumPoints (p1, person, p2) = (hcLookup person p1) + (hcLookup person p2)


rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs


parse :: String -> (Person, PersonHappiness)
parse str = parse' $ splitOn " " str
  where
    parse' (person:"would":"gain":gain:"happiness":"units":"by":"sitting":"next":"to":target:[]) = (person, ((read gain :: Int), init target))
    parse' (person:"would":"lose":lose:"happiness":"units":"by":"sitting":"next":"to":target:[]) = (person, (-1 * (read lose :: Int), init target))
    parse' _ = ("", (0, ""))

