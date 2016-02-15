module Day22 where

data Spell = Spell { name::String, cost::Int, damage::Int, heal::Int, timer::Int }

data Player = Player { hp::Int, armor::Int, dmg::Int }

type Score = Int
type Cost = Int

spells :: [Spell]
spells = [ Spell "MagicMisile" 53 4 0 0
         , Spell "Drain"       73 2 2 0
      ]

play :: Player -> Player -> [Spell] -> (Cost, Score)
play p1 p2 spells = (0, 0)
