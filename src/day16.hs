module Day16 where

import Data.Maybe
import Data.List.Split
import Data.List

data Compound t = Compound { children :: t
                   , cats :: t
                   , samoyeds :: t
                   , pomeranians :: t
                   , akitas :: t
                   , vizslas :: t
                   , goldfish :: t
                   , trees :: t
                   , cars :: t
                   , perfumes :: t
                 } deriving (Show, Eq)

type Sample = Compound (Int -> Bool)
type Aunt = Compound (Maybe Int)

empty :: Aunt
empty = Compound Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

toList :: Compound t -> [t]
toList cmp = [ children cmp
              , cats cmp
              , samoyeds cmp
              , pomeranians cmp
              , akitas cmp
              , vizslas cmp
              , goldfish cmp
              , trees cmp
              , cars cmp
              , perfumes cmp
            ]

findAunt :: [String] -> Sample -> Maybe Int
findAunt aunts cmp = findIndex (same . parse) aunts
  where
    cmpList = toList cmp
    same aunt = and $ catMaybes $ zipWith (<$>) cmpList (toList aunt)

parse :: String -> Aunt
parse line = parse' empty tokens
  where
    tokens = drop 2 $ splitOn " " line

    parse' aunt [] = aunt
    parse' aunt (field:n:xs) = parse' updated xs
      where 
        val = Just $ read $ head $ splitOn "," n
        updated = parse'' (init field) val

        parse'' "cats" n        = aunt { cats = n }
        parse'' "children" n    = aunt { children = n }
        parse'' "samoyeds" n    = aunt { samoyeds = n }
        parse'' "pomeranians" n = aunt { pomeranians = n }
        parse'' "akitas" n      = aunt { akitas = n }
        parse'' "vizslas" n     = aunt { vizslas = n }
        parse'' "goldfish" n    = aunt { goldfish = n }
        parse'' "trees" n       = aunt { trees = n }
        parse'' "cars" n        = aunt { cars = n }
        parse'' "perfumes" n    = aunt { perfumes = n }
        
