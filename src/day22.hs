module Day22 where

import Data.List
import Data.Maybe

type Mana = Int
type Point = Int

data Player = Player { 
  hp::Point,
  armor::Point,
  mana::Mana,
  spent::Mana
} deriving (Eq, Show)

data Boss = Boss { bp::Point, damage::Point } deriving (Eq, Show) 

type Score = Int
type Cost = Int

type PlayerFn = (Player, Boss) -> (Player, Boss)

data Spell = Spell {
  name::String,
  cost::Mana,
  timer::Int,
  everyTurn::PlayerFn,
  begin::PlayerFn,
  ends::PlayerFn
}

spells :: [Spell]
spells = [Spell "MagicMisile" 53 1 id magicMisile id
        , Spell "Drain"       73 1 id drain       id
        , Spell "Shield"     113 6 id shieldBegin shieldEnds
        , Spell "Poison"     173 6 poison id id
        , Spell "Recharge"   229 5 recharge id id
      ]
  where 
    -- Magic Missile costs 53 mana. It instantly does 4 damage 
    magicMisile (me, boss) = (me, damage boss 4)

    -- Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points
    drain (me, boss) = (heal me 2, damage boss 2)

    -- Shield costs 113 mana. It starts an effect that lasts for 6 turns. While it is active, your armor is increased by 7
    shieldBegin (me, boss) = (armorUp me 7, boss) 
    shieldEnds  (me, boss) = (armorUp me (-7), boss)

    -- Poison costs 173 mana. It starts an effect that lasts for 6 turns. At the start of each turn while it is active, it deals the boss 3 damage.
    poison (me, boss) = (me, damage boss 3)

    -- Recharge costs 229 mana. It starts an effect that lasts for 5 turns. At the start of each turn while it is active, it gives you 101 new mana
    recharge (me, boss) = (manaUp me 101, boss)

    manaUp pl n = pl {mana=mana pl + n}
    armorUp pl n = pl {armor=armor pl + n}
    heal pl n = pl {hp=hp pl + n}

    damage boss n = boss { bp = bp boss - n }

spell :: String -> Spell
spell n = fromJust $ find ((==) n . name) spells

play :: Player -> Player -> [Spell] -> (Cost, Score)
play p1 p2 spells = (0, 0)

cast :: (Player, Boss) -> Spell -> (Player, Boss)
cast players@(p1, p2) spell = begin spell $ (takeMana p1, p2)
  where
    takeMana pl = pl {mana=mana pl - cost spell, spent=spent pl + cost spell}
