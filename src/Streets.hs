module Streets where

import Game 
import Decisions 
import Shuffle
import Reads
import Shows
import Data.List as L

lfun :: Struc -> IO Struc
lfun s0 =
  getSeed 265535 >>= \t ->
  let
    zs = makeLstTup t
    hs = L.take 1 zs
    vs = L.take 1 $ L.drop 1 zs
    s1 = createPocketsBoard [] hs vs s0
  in
    putStrLn " --- LOT ---" >>    
    putStr "villain   : " >> mapM_ putCard (villain s1) >> putStrLn "" >>
    putStr "hero      : " >> mapM_ putCard (hero    s1) >> putStrLn "" >>
    case cmpCard (L.head hs) (L.head vs) of    
        LT ->
          let s2 = s1 { order = False, gstage = Preflop }
          in  putStrLn "villain deals on SB, hero acts on BB" >> return (doAnte s2)
        _  ->
          let s2 = s1 { order = True, gstage = Preflop }          
          in  putStrLn "hero deals on SB, villain acts on BB" >> return (doAnte s2)

pfun :: Struc -> IO Struc
pfun s =
    makeDeck (s { gstage = Preflop }) >>= gfun Preflop 
    where
      makeDeck s =
        getSeed 434567 >>= \t ->
        let
          zs = makeLstTup t
          hs = L.take 2 zs
          vs = L.take 2 $ L.drop 2 zs
          fs = L.drop 4 zs
        in return $ createPocketsBoard fs hs vs s  

gfun :: Stage -> Struc -> IO Struc
gfun g s =
  let s1 = s { gstage = g }
  in  putStrLn ("hand " ++ (show $ hand s1) ++ " --- " ++ (show $ gstage s1)) >>
      putGame s1 >> getStavka s1 >>= \s2 -> putGame s2 >> return s2

sdfun :: Struc -> IO Struc
sdfun s =
  let s1 = s { gstage = Showdown }
  in  putStrLn (" hand " ++ (show $ hand s1) ++ " ---  Showdown") >>
      putStr "villain   : " >> mapM_ putCard (villain s) >> putStrLn "" >>
      putStr "hero      : " >> mapM_ putCard (hero s) >> putStrLn "" >>
      putStr "board     : " >> mapM_ putCard (L.take 5 (board s)) >> putStrLn "" >>
      return s                      
