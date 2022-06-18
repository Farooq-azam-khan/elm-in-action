module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)



{-
   expectation : Expectation
   expectation =
       -- a -> a -> Expectation
       Expect.equal (1 + 1) 2
-}


suite : Test
suite =
    test "one plus one equals two" (\_ -> Expect.equal 2 (1 + 1))
