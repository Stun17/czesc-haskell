module Main where

import Game
import Shows
import Reads
import Streets

main =
  putMenu >> return gameInit >>= \s0 ->
  putDeal >> lfun         s0 >>= \s1 ->
  putDeal >> pfun         s1 >>= \s2 ->
  putDeal >> gfun Flop    s2 >>= \s3 ->
  putDeal >> gfun Turn    s3 >>= \s4 ->
  putDeal >> gfun River   s4 >>= \s5 ->
             sdfun        s5 >>= print 
