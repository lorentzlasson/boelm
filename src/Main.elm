module Main exposing (Tile, clickTile, init, initTileGrid, neighbours)

import Browser
import Css exposing (Style, backgroundColor, center, fontSize, height, hex, px, textAlign, width)
import Grid exposing (Coord, Grid)
import Html.Styled exposing (Html, div, table, td, text, toUnstyled, tr)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Json.Decode
import List.Extra
import Maybe.Extra
import Random
import Random.Char
import Random.String



-- CONSTANTS


tileSize : Float
tileSize =
    50


numberSize : Float
numberSize =
    tileSize / 2


difficulty : Int
difficulty =
    10



-- MAIN


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view >> toUnstyled
        }



-- MODEL


type alias Model =
    { gameState : GameState
    , tileGrid : Grid Tile
    }


type alias Tile =
    { tileState : TileState
    , id : String
    , bomb : Bool
    }


type TileState
    = Default
    | Flagged
    | Revealed


type GameState
    = Playing
    | Lost
    | Won


type alias TileInit =
    ( String, Bool )


init : Int -> ( Model, Cmd Msg )
init boardSize =
    ( Model Playing [ [] ], getBoardInit boardSize )


getBoardInit : Int -> Cmd Msg
getBoardInit boardSize =
    Random.generate GotBoardInit (randomTileInitGrid difficulty boardSize)



-- UPDATE


type Msg
    = Click Coord
    | RightClick Coord
    | GotBoardInit (Grid TileInit)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click coord ->
            let
                grid =
                    model.tileGrid
            in
            if gameIsOver model.gameState then
                ( model, Cmd.none )

            else
                clickTile coord grid
                    |> updateTileGrid model
                    |> updateGameState
                    |> pairFlipped Cmd.none

        RightClick coord ->
            let
                grid =
                    model.tileGrid
            in
            if gameIsOver model.gameState then
                ( model, Cmd.none )

            else
                rightClickTile coord grid
                    |> updateTileGrid model
                    |> pairFlipped Cmd.none

        GotBoardInit boolGrid ->
            ( initGridToModel boolGrid, Cmd.none )


updateTileGrid : Model -> Grid Tile -> Model
updateTileGrid model grid =
    { model | tileGrid = grid }


updateGameState : Model -> Model
updateGameState model =
    { model | gameState = gameState model.tileGrid }



-- VIEW


view : Model -> Html Msg
view model =
    let
        grid =
            model.tileGrid
    in
    div []
        [ table []
            (List.map (viewTileRow grid) grid)
        , viewGameState model.gameState
        ]


viewGameState : GameState -> Html Msg
viewGameState state =
    div [ css [ fontSize (px (numberSize * 5)) ] ]
        [ text (viewGameStateText state)
        ]


viewGameStateText : GameState -> String
viewGameStateText state =
    case state of
        Playing ->
            ""

        Won ->
            "🎉"

        Lost ->
            "💥"


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
    in
    td
        [ onClick
            (Click coord)
        , onRightClick
            (RightClick coord)
        , css (styleTile tile)
        ]
        [ text (textTile grid tile)
        ]


textTile : Grid Tile -> Tile -> String
textTile grid tile =
    let
        n =
            neighbouringBombsCount grid tile
    in
    case tile.tileState of
        Revealed ->
            if n == 0 then
                ""

            else
                String.fromInt n

        _ ->
            ""


styleTile : Tile -> List Style
styleTile tile =
    case tile.tileState of
        Default ->
            styleTileBase ++ [ backgroundColor (hex "D9D9D9") ]

        Flagged ->
            styleTileBase ++ [ backgroundColor (hex "F5BF42") ]

        Revealed ->
            if tile.bomb then
                styleTileBase ++ [ backgroundColor (hex "000000") ]

            else
                styleTileBase ++ [ backgroundColor (hex "FFFFFF"), textAlign center, fontSize (px numberSize) ]


styleTileBase : List Style
styleTileBase =
    [ width (px tileSize), height (px tileSize) ]



-- UTIL
-- DOMAIN


isRevealed : Tile -> Bool
isRevealed tile =
    case tile.tileState of
        Revealed ->
            True

        _ ->
            False


isFlagged : Tile -> Bool
isFlagged tile =
    case tile.tileState of
        Flagged ->
            True

        _ ->
            False


gameState : Grid Tile -> GameState
gameState grid =
    if hasWon grid then
        Won

    else if hasLost grid then
        Lost

    else
        Playing


hasWon : Grid Tile -> Bool
hasWon =
    List.concat
        >> List.partition .bomb
        >> bombsHiddenRestRevealed


hasLost : Grid Tile -> Bool
hasLost =
    List.concat
        >> List.filter .bomb
        >> List.any isRevealed


gameIsOver : GameState -> Bool
gameIsOver state =
    case state of
        Playing ->
            False

        _ ->
            True


bombsHiddenRestRevealed : ( List Tile, List Tile ) -> Bool
bombsHiddenRestRevealed ( bombs, rest ) =
    List.all (isRevealed >> not) bombs
        && List.all isRevealed rest


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
    initTileGrid >> Model Playing


initTileGrid : Grid TileInit -> Grid Tile
initTileGrid =
    List.map (List.map (\( id, bomb ) -> Tile Default id bomb))


clickTile : Coord -> Grid Tile -> Grid Tile
clickTile coord grid =
    let
        tile =
            Grid.getElem grid coord
                |> Maybe.withDefault (Tile Revealed "DEFAULT" False)
    in
    if isRevealed tile || isFlagged tile then
        grid

    else if neighbouringBombsCount grid tile == 0 then
        clickTiles
            (Grid.set
                grid
                coord
                { tile | tileState = Revealed }
            )
            (Grid.getNeighbouringCoords coord)

    else
        Grid.set
            grid
            coord
            { tile | tileState = Revealed }


clickTiles : Grid Tile -> List Coord -> Grid Tile
clickTiles grid coords =
    List.foldr clickTile grid coords


rightClickTile : Coord -> Grid Tile -> Grid Tile
rightClickTile coord grid =
    let
        tile =
            Grid.getElem grid coord
                |> Maybe.withDefault (Tile Flagged "DEFAULT" False)
    in
    if isRevealed tile then
        grid

    else
        Grid.set
            grid
            coord
            { tile | tileState = toggledFlagState tile }


toggledFlagState : Tile -> TileState
toggledFlagState tile =
    case tile.tileState of
        Default ->
            Flagged

        Flagged ->
            Default

        a ->
            a



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


onRightClick : msg -> Html.Styled.Attribute msg
onRightClick msg =
    Html.Styled.Events.custom "contextmenu"
        (Json.Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
