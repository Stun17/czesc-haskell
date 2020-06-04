module Shows where

import Game 

import Data.Text as T 
import Data.Text.IO as TIO
import Data.List as L
import Text.Printf as P
-- import Control.Concurrent.Thread.Delay (delay)

-- all functions must return IO a

{-|
    for output of the single card of hero, villain or board
-}
putCard :: Card -> IO ()
putCard (x, y) =
  TIO.putStr $ T.pack $
  case lookup x mysuit of
    Just s ->
      case lookup y myrank of
        Just r -> r ++ s ++ " "
        Nothing -> " "
    Nothing ->  " "

{-|
    for output of cards with straight 
-}
putStraight :: [Card] -> IO ()
putStraight cs = 
  let
    xs = countRanks cs
  in    
    if Prelude.all (>0) $ (Prelude.last xs) : (Prelude.take 4 xs)
    then Prelude.putStr  "straight A2345" else    
    if Prelude.all (>0) $ Prelude.take 5 xs
    then Prelude.putStr  "straight 23456" else
    if Prelude.all (>0) $ Prelude.drop 1 $ Prelude.take  6 xs
    then Prelude.putStr  "straight 34567" else
    if Prelude.all (>0) $ Prelude.drop 2 $ Prelude.take  7 xs
    then Prelude.putStr  "straight 45678" else
    if Prelude.all (>0) $ Prelude.drop 3 $ Prelude.take  8 xs
    then Prelude.putStr  "straight 56789" else
    if Prelude.all (>0) $ Prelude.drop 4 $ Prelude.take  9 xs
    then Prelude.putStr  "straight 6789T" else
    if Prelude.all (>0) $ Prelude.drop 5 $ Prelude.take 10 xs
    then Prelude.putStr  "straight 789TJ" else
    if Prelude.all (>0) $ Prelude.drop 6 $ Prelude.take 11 xs
    then Prelude.putStr  "straight 89TJQ" else
    if Prelude.all (>0) $ Prelude.drop 7 $ Prelude.take 12 xs
    then Prelude.putStr  "straight 9TJQK" else
    if Prelude.all (>0) $ Prelude.drop 8 $ xs
    then Prelude.putStr  "straight TJQKA" else Prelude.putStr "no straight   "

{-|
   for output of cards with flush 
-}
putFlush :: [Card] -> IO ()
putFlush cs =
  let
    (fs, fc, fd, fh) = countSuits cs
  in
    if fs > 4
    then Prelude.putStr $ "flush on Spades " ++ selSuit cs 1
    else if fc > 4
         then Prelude.putStr $ "flush on Clubs " ++ selSuit cs 2
         else if fd > 4
              then Prelude.putStr $ "flush on Diamonds " ++ selSuit cs 3
              else if fh > 4
                   then Prelude.putStr $ "flush on Hearts " ++ selSuit cs 4
                   else Prelude.putStr "no flush "
  where
    selSuit :: [Card] -> Int -> String
    selSuit cs suit =
          dispRanks $ L.map snd $ L.reverse $ L.sort $
          L.filter (\(x, _) -> x == suit) cs
    dispRanks :: [Int] -> String
    dispRanks ys =
          case sequence $ fmap (\x -> lookup x myrank) ys of
            Just rs -> 
              Prelude.take 5 $ Prelude.concat rs
            Nothing ->
              ""

              
putCare :: [Card] -> IO ()
putCare = undefined

putFull :: [Card] -> IO ()
putFull = undefined

putSet :: [Card] -> IO ()
putSet = undefined

putTwoPairs :: [Card] -> IO ()
putTwoPairs = undefined

putPair :: [Card] -> IO ()
putPair = undefined

              
putGame :: Struc -> IO ()
putGame s =
    P.printf "%s%-10s"   "villain : " (show $ vstack s) >> 
    P.printf "\n%s%-10s" "hero    : " (show $ hstack s) >> mapM_ putCard (hero s) >>
    P.printf "\n%s%s%s%s\n" "pot/bet : " (show $ pot s) "/" (show $ bet s) >>
    case (gstage s) of
      Preflop ->
        Prelude.putStrLn ""
      _ ->
        P.printf "%-20s" "board   : " >> mapM_ putCard (L.take 
                           (case gstage s of
                              Flop     -> 3
                              Turn     -> 4
                              River    -> 5
                              _        -> 0)
                           (board s)) >> Prelude.putStrLn ""


putMenu :: IO ()
putMenu =
  Prelude.putStrLn "Texas Holdem Heads-Up NL200 1$/2$" >>
  Prelude.putStrLn "  fold:-1  |  check:0  |  call:1  |  bet:2  |  raise:n  |  all-in:999" 

putDeal :: IO ()
putDeal =
  Prelude.putStr "continue ? (Enter) " >> Prelude.getLine >>
  Prelude.putStr "wait ... "    >> -- delay 1000000 >>
  Prelude.putStr "dealing ... " >> -- delay 5000000 >>
  Prelude.putStr "done. "
