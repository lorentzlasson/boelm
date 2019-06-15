module Grid exposing (Coord, Grid, addCoords, getCoord, getElem, getNeighbouringCoords, neighbourOffsets, set)

import List.Extra
import Maybe.Extra


type alias Grid a =
    List (List a)


type alias Coord =
    ( Int, Int )


set : Coord -> Grid a -> a -> Grid a
set ( x, y ) grid occupant =
    List.Extra.getAt y grid
        |> Maybe.map (List.Extra.setAt x occupant)
        |> Maybe.map (\r -> List.Extra.setAt y r grid)
        |> Maybe.withDefault grid


getNeighbouringCoords : Coord -> List Coord
getNeighbouringCoords coord =
    List.map (addCoords coord) neighbourOffsets


getElem : Grid a -> Coord -> Maybe a
getElem grid ( x, y ) =
    List.Extra.getAt y grid
        |> Maybe.andThen (List.Extra.getAt x)


getCoord : Grid a -> a -> Maybe Coord
getCoord grid a =
    let
        row =
            List.Extra.find (List.member a) grid

        y =
            List.Extra.findIndex (List.member a) grid

        x =
            Maybe.andThen (List.Extra.elemIndex a) row
    in
    Maybe.map2 Tuple.pair x y


neighbourOffsets : List ( Int, Int )
neighbourOffsets =
    [ ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( 1, 0 )
    , ( 1, 1 )
    , ( 0, 1 )
    , ( -1, 1 )
    , ( -1, 0 )
    ]


addCoords : Coord -> Coord -> Coord
addCoords ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )
