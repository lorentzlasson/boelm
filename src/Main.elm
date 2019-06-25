module Main exposing (init)

import Browser
import Css exposing (Style, backgroundColor, center, fontSize, height, hex, px, textAlign, vh, vw, width)
import Grid exposing (Coord, Grid)
import Html.Events exposing (onClick)
import Html.Styled exposing (Html, table, td, text, toUnstyled, tr)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import List.Extra
import Maybe.Extra
import Random
import Random.Char
import Random.String



-- CONSTANTS


boardSize : Int
boardSize =
    50


tileSize : Float
tileSize =
    (100 / toFloat boardSize) * 0.9


numberSize : Float
numberSize =
    tileSize * 7


difficulty : Int
difficulty =
    10



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view >> toUnstyled
        }



-- MODEL


type alias Model =
    { tileGrid : Grid Tile
    }


type alias Tile =
    { revealed : Bool
    , id : String
    , bomb : Bool
    }


type alias TileInit =
    ( String, Bool )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ [] ], getBoardInit )


getBoardInit : Cmd Msg
getBoardInit =
    Random.generate GotBoardInit (randomTileInitGrid difficulty boardSize)



-- UPDATE


type Msg
    = Click Coord
    | GotBoardInit (Grid TileInit)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click coord ->
            let
                grid =
                    model.tileGrid
            in
            clickTile coord grid
                |> updateTileGrid model
                |> pairFlipped Cmd.none

        GotBoardInit boolGrid ->
            ( initGridToModel boolGrid, Cmd.none )


updateTileGrid : Model -> Grid Tile -> Model
updateTileGrid model grid =
    { model | tileGrid = grid }



-- VIEW


view : Model -> Html Msg
view model =
    let
        grid =
            model.tileGrid
    in
    table []
        (List.map (viewTileRow grid) grid)


viewTileRow : Grid Tile -> List Tile -> Html Msg
viewTileRow grid tiles =
    tr []
        (List.map (viewTile grid) tiles)


viewTile : Grid Tile -> Tile -> Html Msg
viewTile grid tile =
    let
        coord =
            Grid.getCoord grid tile
                |> Maybe.withDefault ( 0, 0 )

        n =
            neighbouringBombsCount grid tile
    in
    td [ onClick (Click coord), css (styleTile tile) ]
        [ text (textTile tile.revealed tile.bomb n)
        ]


textTile : Bool -> Bool -> Int -> String
textTile revealed bomb n =
    if not revealed || bomb || n == 0 then
        ""

    else
        String.fromInt n


styleTile : Tile -> List Style
styleTile tile =
    if tile.revealed then
        if tile.bomb then
            styleTileBase ++ [ backgroundColor (hex "000000") ]

        else
            styleTileBase ++ [ backgroundColor (hex "FFFFFF"), textAlign center, fontSize (px numberSize) ]

    else
        styleTileBase ++ [ backgroundColor (hex "D9D9D9") ]


styleTileBase : List Style
styleTileBase =
    [ width (vw tileSize), height (vh tileSize) ]



-- UTIL
-- DOMAIN


neighbouringBombsCount : Grid Tile -> Tile -> Int
neighbouringBombsCount grid tile =
    Grid.getCoord grid tile
        |> Maybe.map (neighbouringCoordsBombCount grid)
        |> Maybe.withDefault 0


neighbours : Grid a -> Coord -> List a
neighbours grid =
    Grid.getNeighbouringCoords
        >> List.map (Grid.getElem grid)
        >> Maybe.Extra.values


neighbouringCoordsBombCount : Grid Tile -> Coord -> Int
neighbouringCoordsBombCount grid =
    neighbours grid >> List.Extra.count .bomb


randomTileInitGrid : Int -> Int -> Random.Generator (Grid TileInit)
randomTileInitGrid percent size =
    randomTileInit percent
        |> Random.list size
        |> Random.list size


randomTileInit : Int -> Random.Generator TileInit
randomTileInit percent =
    Random.map2
        Tuple.pair
        randomId
        (percentTrue percent)


initGridToModel : Grid TileInit -> Model
initGridToModel =
    initTileGrid >> Model


initTileGrid : Grid TileInit -> Grid Tile
initTileGrid =
    List.map (List.map (\( id, bomb ) -> Tile False id bomb))


clickTile : Coord -> Grid Tile -> Grid Tile
clickTile coord grid =
    let
        tile =
            Grid.getElem grid coord
                |> Maybe.withDefault (Tile True "DEFAULT" False)
    in
    if tile.revealed then
        grid

    else if neighbouringBombsCount grid tile == 0 then
        clickTiles
            (Grid.set
                grid
                coord
                { tile | revealed = True }
            )
            (Grid.getNeighbouringCoords coord)

    else
        Grid.set
            grid
            coord
            { tile | revealed = True }


clickTiles : Grid Tile -> List Coord -> Grid Tile
clickTiles grid coords =
    List.foldr clickTile grid coords



-- GENERIC


percentTrue : Int -> Random.Generator Bool
percentTrue percent =
    Random.map
        (\x -> x <= percent)
        (Random.int 0 100)


randomId : Random.Generator String
randomId =
    Random.String.string 10 Random.Char.english


pairFlipped : a -> b -> ( b, a )
pairFlipped a b =
    ( b, a )
