module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Css
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode as Decode
import Set
import Task


type alias Vector2 =
    ( Int, Int )


type alias Scale =
    Set.Set Vector2


type alias Cell =
    { x : Int
    , y : Int
    , text : String
    , highlighted : Bool
    }


type alias Grid =
    { columns : Int
    , rows : Int
    , cellSize : Int
    , gap : Int
    }


type alias Model =
    { grid : Grid
    , selectedNotes : Scale
    }


type Msg
    = Resize Dom.Viewport
    | ToggleNote Vector2


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { grid = { columns = 0, rows = 0, cellSize = 26, gap = 1 }
      , selectedNotes = Set.empty
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
                        model.grid
              }
            , Cmd.none
            )

        ToggleNote position ->
            ( { model
                | selectedNotes =
                    symmetricDifferenceSets
                        (Set.singleton position)
                        model.selectedNotes
              }
            , Cmd.none
            )


view : Model -> Browser.Document Msg
view model =
    { title = "muspace"
    , body =
        [ Html.div []
            [ Html.div
                [ Attributes.css [ gridStyle model.grid ]
                , onCellClick ToggleNote
                ]
                (List.map renderCell (generateCells model))
            ]
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


decodeDataset : String -> Decode.Decoder String
decodeDataset field =
    Decode.at [ "target", "dataset", field ] Decode.string


decodeCellPosition : Decode.Decoder Vector2
decodeCellPosition =
    let
        toInt =
            Decode.map (String.toInt >> Maybe.withDefault 0)
    in
    Decode.map2 Tuple.pair (decodeDataset "x" |> toInt) (decodeDataset "y" |> toInt)


onCellClick : (Vector2 -> msg) -> Html.Attribute msg
onCellClick tagger =
    Events.on "click" (Decode.map tagger decodeCellPosition)


renderCell : Cell -> Html.Html msg
renderCell cell =
    Html.div
        [ Attributes.attribute "data-x" (String.fromInt cell.x)
        , Attributes.attribute "data-y" (String.fromInt cell.y)
        , Attributes.css
            [ if cell.highlighted then
                highlightedCellStyle

              else
                cellStyle
            ]
        ]
        [ Html.text cell.text ]


generateCells : Model -> List Cell
generateCells model =
    let
        cell x y =
            { x = x
            , y = y
            , text = noteNameAt x y
            , highlighted = Set.member ( x, y ) model.selectedNotes
            }
    in
    List.range 0 ((model.grid.rows * model.grid.columns) - 1)
        |> List.map (\x -> cell (modBy model.grid.columns x) (x // model.grid.columns))


fitGrid : Int -> Int -> Grid -> Grid
fitGrid width height grid =
    { grid
        | columns = (width + grid.gap) // (grid.cellSize + grid.gap)
        , rows = (height + grid.gap) // (grid.cellSize + grid.gap)
    }


noteNameAt : Int -> Int -> String
noteNameAt x y =
    let
        notes =
            [ "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b" ]

        -- x fifths up and y thirds down because we start at the top-left corner
        steps =
            modBy 12 ((x * 7) + (y * (12 - 4)))
    in
    List.head (List.drop steps notes) |> Maybe.withDefault "?"


degreeToNote : Vector2 -> Vector2 -> Vector2
degreeToNote root degree =
    -- flip y coordinate, because (0, 0) is in the top-left corner
    addVectors root (transformVector ( 1, -1 ) degree)


noteToDegree : Vector2 -> Vector2 -> Vector2
noteToDegree root note =
    -- flip y coordinate, because (0, 0) is in the top-left corner
    subtractVectors note root |> transformVector ( 1, -1 )


buildChord : Vector2 -> Scale -> Scale
buildChord root degrees =
    Set.union (Set.singleton root) (Set.map (degreeToNote root) degrees)


addVectors : Vector2 -> Vector2 -> Vector2
addVectors ( ax, ay ) ( bx, by ) =
    ( ax + bx, ay + by )


subtractVectors : Vector2 -> Vector2 -> Vector2
subtractVectors ( ax, ay ) ( bx, by ) =
    ( ax - bx, ay - by )


transformVector : Vector2 -> Vector2 -> Vector2
transformVector ( tx, ty ) ( ax, ay ) =
    ( ax * tx, ay * ty )


symmetricDifferenceSets : Set.Set comparable -> Set.Set comparable -> Set.Set comparable
symmetricDifferenceSets set other =
    Set.foldl
        (\x xs ->
            if Set.member x xs then
                Set.remove x xs

            else
                Set.insert x xs
        )
        other
        set
