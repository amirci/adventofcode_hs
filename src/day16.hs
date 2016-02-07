module Day16 where

import Data.Maybe
import Data.List.Split
import Data.List

data Compound = Compound { children :: Maybe Int
                          , cats :: Maybe Int
                          , samoyeds :: Maybe Int
                          , pomeranians :: Maybe Int
                          , akitas :: Maybe Int
                          , vizslas :: Maybe Int
                          , goldfish :: Maybe Int
                          , trees :: Maybe Int
                          , cars :: Maybe Int
                          , perfumes :: Maybe Int
                        } deriving (Show, Eq)

type Aunt = Compound

empty :: Compound
empty = Compound Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

toList :: Compound -> [Maybe Int]
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

findAunt :: [String] -> Compound -> Maybe Int
findAunt aunts cmp = findIndex (same . parse) aunts
  where
    cmpList = toList cmp
    same aunt = and $ catMaybes $ zipWith sameField (toList aunt) cmpList
    sameField f1 f2 = (==) <$> f1 <*> f2

parse :: String -> Compound
parse line = parse' empty $ drop 2 $ splitOn " " line
  where
    parse' comp [] = comp
    parse' comp (field:n:xs) = parse' updated xs
      where updated = parse'' comp (init field) $ Just $ read (head $ splitOn "," n)

    parse'' comp "cats" n = comp { cats = n }
    parse'' comp "children" n = comp { children = n }
    parse'' comp "samoyeds" n = comp { samoyeds = n }
    parse'' comp "pomeranians" n = comp { pomeranians = n }
    parse'' comp "akitas" n      = comp { akitas = n }
    parse'' comp "vizslas" n     = comp { vizslas = n }
    parse'' comp "goldfish" n    = comp { goldfish = n }
    parse'' comp "trees" n       = comp { trees = n }
    parse'' comp "cars" n        = comp { cars = n }
    parse'' comp "perfumes" n    = comp { perfumes = n }
    
