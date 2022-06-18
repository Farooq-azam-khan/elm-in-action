module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Json.Decode as JD exposing (decodeString)
import PhotoGroove
import Test exposing (..)


decoderTest : Test
decoderTest =
    test "title defaults to (untitle)"
        (\_ ->
            """{"url":"fruits.com","size":5}"""
                |> decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitle)" })
        )



{-
   expectation : Expectation
   expectation =
       -- a -> a -> Expectation
       Expect.equal (1 + 1) 2
-}
{-
   suite : Test
   suite =
       test "one plus one equals two" (\_ -> Expect.equal 2 (1 + 1))
-}
