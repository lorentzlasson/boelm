module MainTest exposing (suite)

import Expect
import Main exposing (Tile)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Main"
        [ test "neighbours"
            (\_ ->
                let
                    grid =
                        [ [ "a", "b", "c" ]
                        , [ "d", "e", "f" ]
                        , [ "g", "h", "i" ]
                        ]

                    coord =
                        ( 2, 2 )
                in
                Expect.equal (Main.neighbours grid coord) [ "e", "h", "f" ]
            )
        , test "clickTile"
            (\_ ->
                let
                    grid =
                        [ [ ( "a", False ), ( "b", False ), ( "c", False ) ]
                        , [ ( "d", False ), ( "e", False ), ( "f", True ) ]
                        , [ ( "g", False ), ( "h", False ), ( "i", True ) ]
                        ]
                            |> Main.initTileGrid

                    coord =
                        ( 0, 0 )
                in
                Expect.equal (Main.clickTile coord grid)
                    [ [ Tile True "a" False, Tile True "b" False, Tile False "c" False ]
                    , [ Tile True "d" False, Tile True "e" False, Tile False "f" True ]
                    , [ Tile True "g" False, Tile True "h" False, Tile False "i" True ]
                    ]
            )
        ]
