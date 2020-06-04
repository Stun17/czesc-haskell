module Reads where

import Game
import Data.Time.Clock.POSIX (getPOSIXTime)

-- all functions must return IO a

getStavka :: Struc -> IO Struc
getStavka s =
  if order s
  then
    putStr "hero    : " >> readLn >>= \n ->
    if (n > 2) && (n < bet s)
    then putStrLn "you should stake no less then current bet! try again" >> getStavka s 
    else if (n /= 999) && (n > hstack s)
         then putStrLn "you havent got so much chips! try again" >> getStavka s 
         else if n < 0
              then return $ betToPotFmStack (-1) s
              else return $ betToPotFmStack n s
  else putStrLn "villain : " >> return s

getSeed :: Integer -> IO Integer
getSeed x =
  getPOSIXTime >>= \t -> return $ mod ((toInteger . floor) t) x
       
