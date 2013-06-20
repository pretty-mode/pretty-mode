#!/usr/bin/env runhaskell

import Control.Exception
import Data.List

f::Integer
f=5

g::(Integer)
g=6

h::Num a => a -> a
h = (* 2)

j::Fractional a=>Num a=>a->a
j = (/ 2)

main :: IO ()
main = let a = 1
           b = 2
           c = [1..10]
           d = [11..20]
           r = assert ( 2 >= 1 )
             $ assert ( 2>=1 )
             $ assert ( (1+1)>=(0+1) )
             $ assert ( b>=a )
             $ assert ( 1 <= 2 )
             $ assert ( 1<=2 )
             $ assert ( (1+0)<=(1+1) )
             $ assert ( a<=b )
             $ assert ( 1 /= 2 )
             $ assert ( 1/=2 )
             $ assert ( (0+1)/=(1+1) )
             $ assert ( a/=b )
             $ assert ( 1 == 1 )
             $ assert ( 1==1 )
             $ assert ( (1+1)==(1+1) )
             $ assert ( a==a )
             $ assert ( not False )
             $ assert (not(False) )
             $ assert ( and [True, True] )
             $ assert (and[True, True] )
             $ assert (and([True, True]) )
             $ assert ( or [False, True] )
             $ assert (or[False, True] )
             $ assert (or([False, True]) )
             $ assert ( 1 `elem` [1 .. 10] )
             $ assert ( 1`elem`[1..10] )
             $ assert ( [4, 5] == [1 .. 5] `intersect` [4 .. 10] )
             $ assert ( [4, 5]==[1..5]`intersect`[4..10] )
             $ assert ( [1 .. 10] == [1 .. 5] `union` [4 .. 10] )
             $ assert ( [1 .. 10]==[1..5]`union`[4..10] )
             $ assert ( [2, 3] == [1 .. 3] \\ [1] )
             $ assert ( [2, 3]==[1..3]\\[1] )
             $ assert ( [2, 3] == (\\) [1..3] [1] )
             $ assert ( 1 == [1 .. 10] !! 0 )
             $ assert ( 1==[1 .. 10]!!0 )
             $ assert ( 2==[1 .. 10]!!a )
             $ assert ( 1 == (!!) [1 .. 10] 0 )
             $ assert ( 1 == ((* 2) . (/ 2)) 1 )
             $ assert ( 1 == ((* 2).(/ 2)) 1 )
             $ assert ( 1 == (j.h) 1 )
             $ assert ( 6 == product [1 .. 3] )
             $ assert ( 6==product[1 .. 3] )
             $ assert ( 6==product[1 .. 3] )
             $ assert ( 6==product([1 .. 3]) )
             $ assert ( 6==sum[1 .. 3] )
             $ assert ( 6==sum[1 .. 3] )
             $ assert ( 6==sum([1 .. 3]) )
             $ assert ( 2.0 == sqrt 4)
             $ assert ( 2.0==sqrt(4))
             $ assert ( [1..20] == [1..10] ++ [11..20] )
             $ assert ( [1..20]==[1..10]++[11..20] )
             $ assert ( [1..20]==([1..10])++([11..20]))
             $ assert ( [1..20]==c++d)
             $ assert ( head [True, undefined ] )
             $ assert ( head [True,undefined] )
             $ assert ( head [True,(undefined)] )
             $ assert ( head [True,True&&undefined&&True] )
             $ assert ( head [1,0+undefined-5] > 0)
             $ assert ( head [1,1*undefined/5] > 0)
             $ assert True "success"
       in print ("Finished with " ++ r)
