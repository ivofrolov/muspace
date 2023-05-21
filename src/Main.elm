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


type Mode
    = Pick
    | Place


type alias Model =
    { grid : Grid
    , mode : Mode
    , picked : Scale
    , placed : Scale
    }


type Msg
    = Resize Dom.Viewport
    | PlaceScale Vector2
    | SetMode Mode


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
      , mode = Pick
      , picked = Set.fromList [ ( 1, 0 ), ( 0, 1 ) ]
      , placed = Set.empty
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

        PlaceScale position ->
            case model.mode of
                Pick ->
                    ( { model
                        | picked =
                            symmetricDifferenceSets
                                (Set.singleton (noteToDegree (pickRoot model.grid) position))
                                model.picked
                      }
                    , Cmd.none
                    )

                Place ->
                    ( { model
                        | placed =
                            symmetricDifferenceSets
                                (buildChord position model.picked)
                                model.placed
                      }
                    , Cmd.none
                    )

        SetMode mode ->
            ( { model | mode = mode }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "muspace"
    , body =
        [ Html.div []
            [ Html.div
                [ Attributes.css [ gridStyle model.grid ]
                , onCellClick PlaceScale
                ]
                (List.map renderCell (generateCells model))
            , Html.div
                [ Attributes.css
                    [ Css.position Css.fixed
                    , Css.bottom (Css.em 2)
                    , Css.left (Css.pct 50)
                    , Css.transform (Css.translateX (Css.pct -50))
                    ]
                ]
                [ Html.button
                    [ Attributes.css [ buttonStyle (model.mode == Pick) ]
                    , Events.onClick (SetMode Pick)
                    ]
                    [ Html.text "Pick" ]
                , Html.button
                    [ Attributes.css [ buttonStyle (model.mode == Place) ]
                    , Events.onClick (SetMode Place)
                    ]
                    [ Html.text "Place" ]
                ]
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


buttonStyle : Bool -> Css.Style
buttonStyle active =
    let
        borderStyle =
            if active then
                Css.inset

            else
                Css.outset
    in
    Css.batch
        [ Css.fontFamily Css.monospace
        , Css.border3 (Css.px 1) borderStyle (Css.hex "#D2D2D2")
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
        highlightedNotes =
            case model.mode of
                Pick ->
                    buildChord (pickRoot model.grid) model.picked

                Place ->
                    model.placed

        cell x y =
            { x = x
            , y = y
            , text = noteNameAt x y
            , highlighted = Set.member ( x, y ) highlightedNotes
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


pickRoot : Grid -> Vector2
pickRoot grid =
    ( grid.columns // 2, grid.rows // 2 )


noteNameAt : Int -> Int -> String
noteNameAt x y =
    let
        notes =
            [ "c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b" ]

        -- x fifths and y thirds from the top-left corner
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
