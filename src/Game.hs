module Game where

import Control.Monad.State
import Data.List as L

type Card   = (Int, Int)

data Player = TAG | TPG | LAG | LPG | Manjak | Normal deriving (Show)

data Stage  = Lot | Preflop | Flop | Turn | River | Showdown deriving (Show)

data Status = Idle | Fold | Check | Bet | Raise | AllIn  deriving (Show)

type History = [(Status, Int)]

data Struc = Struc
  { hero    :: [Card]  -- hero pocket cards
  , villain :: [Card]  -- bot pocket cards
  , board   :: [Card]  -- common cards
  , order   :: Bool    -- True - hero acts ; False - bot acts    
  , cond    :: Status  -- флопнули чекнули кольнули срейзили запушили
  , gstage  :: Stage   -- на какой стадии находимся
  , history :: History
  , htype   :: Player
  , bet     :: Int    -- текущая ставка
  , pot     :: Int
  , hstack  :: Int    -- стек героя
  , vstack  :: Int    -- стек бота
  , hand    :: Int    -- for information only - not used in algorithms
  } deriving (Show)

-----------------------------

gameInit :: Struc
gameInit = Struc
  { gstage  = Lot
  , hero    = []
  , villain = []
  , order   = False              
  , cond    = Idle
  , history = []
  , htype   = Normal
  , board   = []
  , bet     = 1
  , pot     = 0
  , vstack  = 200
  , hstack  = 200
  , hand    = 1
  }

mysuit :: [(Int, String)]  
mysuit =
  [(1, "♠"), (2, "♣"), (3, "♦"), (4, "♥")]

myrank :: [(Int, String)]
myrank = 
  [ (2,  "2"),  (3,  "3"),  (4,  "4"),  (5,  "5"),  (6,  "6"),  (7, "7"),  (8, "8"),  (9, "9")
  , (10, "T"),  (11, "J"),  (12, "Q"),  (13, "K"),  (14, "A")
  ]                                       

--------------------------------------

cmpCard :: Card -> Card -> Ordering
cmpCard (_, r1) (_, r2) = compare r1 r2

createPocketsBoard :: [Card] -> [Card] -> [Card] -> Struc -> Struc
createPocketsBoard xs ys zs = 
  execState $
    get >>= \y -> put (y {board = xs, hero = ys, villain= zs}) >>= return

betToPotFmStack :: Int -> Struc -> Struc
betToPotFmStack stake =
  execState $
    get >>= \s ->
       case stake of
         (-1)  ->
                 if order s
                 then put (s { pot    = 0
                             , vstack = (pot s) + (vstack s)
                             , bet    = 1
                             }
                          ) >>= return
                 else put (s { pot    = 0
                             , hstack = (pot s) + (hstack s)
                             , bet    = 1
                             }
                          ) >>= return
         0     ->
                 put (s { order = not (order s) }) >>= return
         1     ->
                 undefined
         999   ->
                 undefined
         n     ->
                 if order s
                 then put (s { pot    = n + pot s
                             , hstack = (hstack s) - n
                             , bet    = n + bet s
                             }) >>= return
                 else put (s { pot    = n + pot s
                             , vstack = (vstack s) - n
                             , bet    = n + bet s
                             }) >>= return
                 
doAnte :: Struc -> Struc
doAnte =
  execState $
    get >>= \s ->
        if   order s
        then put (s { pot     = 3
                    , bet     = 1
                    , hstack  = (hstack s) - 1
                    , vstack  = (vstack s) - 2
                    }) >>= return
        else put (s { pot     = 3
                    , bet     = 1
                    , hstack  = (hstack s) - 2
                    , vstack  = (vstack s) - 1
                    }) >>= return


countSuits :: [Card] -> (Int, Int, Int, Int)
countSuits cs =
  foldr
    (\(suit, _) (fs, fc, fd, fh) ->
           case suit of
             1 -> (fs + 1, fc, fd, fh)
             2 -> (fs, fc + 1, fd, fh)
             3 -> (fs, fc, fd + 1, fh)
             4 -> (fs, fc, fd, fh + 1))
    (0, 0, 0, 0)
    cs 

countRanks :: [Card] -> [Int]
countRanks cs =
  foldr
    (\(_, rank) [f2, f3, f4, f5, f6, f7, f8, f9, ft, fj, fq, fk, fa] ->
           case rank of
             2 ->  [f2 + 1, f3, f4, f5, f6, f7, f8, f9, ft, fj, fq, fk, fa]
             3 ->  [f2, f3 + 1, f4, f5, f6, f7, f8, f9, ft, fj, fq, fk, fa]
             4 ->  [f2, f3, f4 + 1, f5, f6, f7, f8, f9, ft, fj, fq, fk, fa]
             5 ->  [f2, f3, f4, f5 + 1, f6, f7, f8, f9, ft, fj, fq, fk, fa]
             6 ->  [f2, f3, f4, f5, f6 + 1, f7, f8, f9, ft, fj, fq, fk, fa]
             7 ->  [f2, f3, f4, f5, f6, f7 + 1, f8, f9, ft, fj, fq, fk, fa]
             8 ->  [f2, f3, f4, f5, f6, f7, f8 + 1, f9, ft, fj, fq, fk, fa]
             9 ->  [f2, f3, f4, f5, f6, f7, f8, f9 + 1, ft, fj, fq, fk, fa]
             10 -> [f2, f3, f4, f5, f6, f7, f8, f9, ft + 1, fj, fq, fk, fa]
             11 -> [f2, f3, f4, f5, f6, f7, f8, f9, ft, fj + 1, fq, fk, fa]
             12 -> [f2, f3, f4, f5, f6, f7, f8, f9, ft, fj, fq + 1, fk, fa]
             13 -> [f2, f3, f4, f5, f6, f7, f8, f9, ft, fj, fq, fk + 1, fa]
             14 -> [f2, f3, f4, f5, f6, f7, f8, f9, ft, fj, fq, fk, fa + 1]) 
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    cs 
