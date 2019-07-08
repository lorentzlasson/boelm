module GridTest exposing (suite)

import Expect
import Grid
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Grid"
        [ test "addCoords"
            (\_ ->
                let
                    a =
                        ( 1, 10 )

                    b =
                        ( 11, 50 )
                in
                Expect.equal (Grid.addCoords a b) ( 12, 60 )
            )
        , test "set"
            (\_ ->
                let
                    grid =
                        [ [ "a", "b" ]
                        , [ "c", "d" ]
                        ]

                    coord =
                        ( 0, 1 )

                    newElement =
                        "foo"
                in
                Expect.equal (Grid.set grid coord newElement) [ [ "a", "b" ], [ "foo", "d" ] ]
            )
        ]
