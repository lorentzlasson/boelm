module Main exposing (Tile, clickTile, init, initTileGrid, neighbours)

import Browser
import Browser.Navigation
import Css exposing (Style, backgroundColor, center, fontSize, height, hex, px, textAlign, width)
import Grid exposing (Coord, Grid)
import Html exposing (Html)
import Html.Events exposing (onClick)
import Html.Styled exposing (div, table, td, text, toUnstyled, tr)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import List.Extra
import Maybe.Extra
import Random
import Random.Char
import Random.String
import Url
import Url.Parser.Query



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


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = \_ -> NoOp
        , onUrlRequest = \_ -> NoOp
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
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


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model Playing [ [] ]
    , getBoardInit
        Maybe.withDefault
        10
        (sizeFromUrl url)
    )



--  Url.fromString "http://localhost:8000/src/Main.elm?foo=bar" |> Maybe.andThen (Url.Parser.parse (Url.Parser.query (Url.Parser.Query.string "foo")))


sizeFromUrl : Maybe String -> Maybe Int
sizeFromUrl { query } =
    Url.Parser.Query.int


getBoardInit : Int -> Cmd Msg
getBoardInit boardSize =
    Random.generate GotBoardInit (randomTileInitGrid difficulty boardSize)



-- UPDATE


type Msg
    = Click Coord
    | GotBoardInit (Grid TileInit)
    | NoOp


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

        GotBoardInit boolGrid ->
            ( initGridToModel boolGrid, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateTileGrid : Model -> Grid Tile -> Model
updateTileGrid model grid =
    { model | tileGrid = grid }


updateGameState : Model -> Model
updateGameState model =
    { model | gameState = gameState model.tileGrid }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "boelm"
        [ viewDiv model ]


viewDiv : Model -> Html.Html Msg
viewDiv model =
    let
        grid =
            model.tileGrid
    in
    toUnstyled
        (div []
            [ table []
                (List.map (viewTileRow grid) grid)
            , text (viewGameState model.gameState)
            ]
        )


viewGameState : GameState -> String
viewGameState state =
    case state of
        Playing ->
            ""

        Won ->
            repeatRow "🎉"

        Lost ->
            repeatRow "💥"


repeatRow : String -> String
repeatRow =
    List.repeat 50 >> String.concat


viewTileRow : Grid Tile -> List Tile -> Html.Styled.Html Msg
viewTileRow grid tiles =
    tr []
        (List.map (viewTile grid) tiles)


viewTile : Grid Tile -> Tile -> Html.Styled.Html Msg
viewTile grid tile =
    let
        coord =
            Grid.getCoord grid tile
                |> Maybe.withDefault ( 0, 0 )
    in
    td [ onClick (Click coord), css (styleTile tile) ]
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
    if isRevealed tile then
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
