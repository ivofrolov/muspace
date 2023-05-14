module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode as Decode
import Task


defaultGridCellSize =
    26


defaultGridGap =
    1


type alias Vector2 =
    { i : Int, j : Int }


type alias Cell =
    { i : Int
    , j : Int
    , text : String
    , highlighted : Bool
    }


type alias Grid =
    { columns : Int
    , rows : Int
    , cellSize : Int
    , gap : Int
    }


type alias Scale =
    List Vector2


type alias Model =
    { grid : Grid, hoverRoot : Vector2, hoverChord : Scale }


type Msg
    = Resize Dom.Viewport
    | SetRoot Vector2


majorChord : Scale
majorChord =
    [ Vector2 1 0, Vector2 0 1 ]


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid =
            { rows = 0
            , columns = 0
            , cellSize = defaultGridCellSize
            , gap = defaultGridGap
            }
      , hoverRoot = { i = 0, j = 0 }
      , hoverChord = majorChord
      }
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
                        defaultGridCellSize
                        defaultGridGap
              }
            , Cmd.none
            )

        SetRoot root ->
            ( { model | hoverRoot = root }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "muspace"
    , body =
        [ Html.div
            [ Attributes.css [ gridStyle model.grid ]
            , onMouseOverCell SetRoot
            ]
            (List.map renderCell (generateSpace model.grid (buildChord model.hoverChord model.hoverRoot)))
            |> Html.toUnstyled
        ]
    }


gridStyle : Grid -> Css.Style
gridStyle grid =
    Css.batch
        [ Css.property "--columns" (String.fromInt grid.columns)
        , Css.property "--rows" (String.fromInt grid.rows)
        , Css.property "--cell-size" (String.fromInt grid.cellSize ++ "px")
        , Css.property "--gap" (String.fromInt grid.gap ++ "px")
        , Css.property "display" "grid"
        , Css.property "grid-template-columns" "repeat(var(--columns), var(--cell-size))"
        , Css.property "grid-template-rows" "repeat(var(--rows), var(--cell-size))"
        , Css.property "row-gap" "var(--gap)"
        , Css.property "column-gap" "var(--gap)"
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
        [ Css.property "border-width" "var(--gap)"
        , Css.borderStyle Css.solid
        , Css.borderColor (Css.hex "#D2D2D2")
        , Css.width (Css.pct 100)
        , Css.height (Css.pct 100)
        , Css.hover [ Css.backgroundColor (Css.hex "#D5D5D5") ]
        ]


highlightedCellStyle : Css.Style
highlightedCellStyle =
    Css.batch
        [ cellStyle
        , Css.backgroundColor (Css.hex "#FF644E")
        , Css.hover [ Css.backgroundColor (Css.hex "#FF644E") ]
        ]


dataset : String -> Decode.Decoder String
dataset field =
    Decode.at [ "target", "dataset", field ] Decode.string


onMouseOverCell : (Vector2 -> msg) -> Html.Attribute msg
onMouseOverCell tagger =
    let
        toInt =
            Decode.map (String.toInt >> Maybe.withDefault 0)

        position =
            Decode.map2 Vector2 (dataset "i" |> toInt) (dataset "j" |> toInt)
    in
    Events.on "mouseover" (Decode.map tagger position)


renderCell : Cell -> Html.Html msg
renderCell cell =
    Html.div
        [ Attributes.attribute "data-i" (String.fromInt cell.i)
        , Attributes.attribute "data-j" (String.fromInt cell.j)
        , Attributes.css
            [ if cell.highlighted then
                highlightedCellStyle

              else
                cellStyle
            ]
        ]
        [ Html.span [] [ Html.text cell.text ] ]


fitGrid : Int -> Int -> Int -> Int -> Grid
fitGrid width height cellSize gap =
    { columns = (width + gap) // (cellSize + gap)
    , rows = (height + gap) // (cellSize + gap)
    , cellSize = cellSize
    , gap = gap
    }


generateSpace : Grid -> Scale -> List Cell
generateSpace grid scale =
    let
        highlighted x =
            List.member x scale

        cell i j =
            { i = i, j = j, text = note i j, highlighted = highlighted (Vector2 i j) }
    in
    List.range 0 ((grid.rows * grid.columns) - 1)
        |> List.map (\x -> cell (modBy grid.columns x) (x // grid.columns))


note : Int -> Int -> String
note i j =
    let
        notes =
            [ "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b" ]

        -- i fifths and j thirds from the top-left corner
        steps =
            modBy 12 ((i * 7) + (j * (12 - 4)))
    in
    case List.head (List.drop steps notes) of
        Just x ->
            x

        Nothing ->
            "?"


buildChord : Scale -> Vector2 -> Scale
buildChord degrees root =
    let
        add x y =
            -- flip j coordinate, because (0, 0) is in the top-left corner
            Vector2 (x.i + y.i) (x.j - y.j)
    in
    root :: List.map (add root) degrees
