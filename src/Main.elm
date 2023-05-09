module Main exposing (main, view)

import Browser
import Browser.Dom as Dom
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Task


type alias Cell =
    { i : Int
    , j : Int
    , text : String
    }


type alias Grid =
    { rows : Int
    , columns : Int
    , cellSize : Int
    }


type alias Model =
    { grid : Grid }


type Msg
    = Resize Dom.Viewport


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = { rows = 0, columns = 0, cellSize = 0 } }
    , Task.perform Resize Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize viewport ->
            ( { model
                | grid =
                    fitGrid
                        (truncate viewport.viewport.width)
                        (truncate viewport.viewport.height)
                        26
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "muspace"
    , body =
        [ Html.div
            [ Attributes.css [ gridStyle model.grid ] ]
            (List.map renderCell (generateSpace model.grid))
            |> Html.toUnstyled
        ]
    }


gridStyle : Grid -> Css.Style
gridStyle grid =
    Css.batch
        [ Css.property "--columns" (String.fromInt grid.columns)
        , Css.property "--rows" (String.fromInt grid.rows)
        , Css.property "--cell-size" (String.fromInt grid.cellSize ++ "px")
        , Css.property "display" "grid"
        , Css.property "grid-template-columns" "repeat(var(--columns), var(--cell-size))"
        , Css.property "grid-template-rows" "repeat(var(--rows), var(--cell-size))"
        , Css.property "row-gap" "1px"
        , Css.property "column-gap" "1px"
        , Css.fontFamily Css.monospace
        , Css.fontSize (Css.px (toFloat grid.cellSize / 2))
        , Css.textAlign Css.center
        , Css.lineHeight (Css.px (toFloat grid.cellSize))
        , Css.cursor Css.default
        , Css.property "user-select" "none"
        ]


cellStyle : Css.Style
cellStyle =
    Css.batch
        [ Css.border3 (Css.px 1) Css.solid (Css.hex "#D2D2D2")
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.hover [ Css.backgroundColor (Css.hex "#D5D5D5") ]
        ]


renderCell : Cell -> Html.Html msg
renderCell cell =
    Html.div
        [ Attributes.css [ cellStyle ] ]
        [ Html.span [] [ Html.text cell.text ] ]


fitGrid : Int -> Int -> Int -> Grid
fitGrid width height cellSize =
    -- 1px grid gap compensation
    { rows = (height - (height // cellSize)) // cellSize
    , columns = (width - (width // cellSize)) // cellSize
    , cellSize = cellSize
    }


generateSpace : Grid -> List Cell
generateSpace grid =
    let
        cell i j =
            Cell i j (note i j)
    in
    List.range 0 ((grid.rows * grid.columns) - 1)
        |> List.map (\x -> cell (x // grid.columns) (modBy grid.columns x))


note : Int -> Int -> String
note i j =
    let
        notes =
            [ "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b" ]

        -- i thirds and j fifths from top-left corner
        steps =
            modBy 12 ((i * (12 - 4)) + (j * 7))
    in
    case List.head (List.drop steps notes) of
        Just x ->
            x

        Nothing ->
            "?"
