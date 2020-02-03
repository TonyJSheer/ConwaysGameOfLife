module ConwaysGameOfLife exposing (..)

import Html exposing (Html)
import Html.Events exposing (..)
import Html.Attributes exposing (placeholder, value)
import Browser
import Random exposing (Generator)
import Time

import CellGrid as CG exposing (CellGrid)
import CellGrid.Render as CGR exposing (asHtml, CellStyle)
import Color

import Element as EL exposing (Element, el, text, fill, padding)
import Element.Font as Font


-- MAIN

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Helpers


maybeTrue : Generator Bool
maybeTrue = Random.map (\n -> n <= truthPercent) (Random.int 1 100)

newList : Generator (List Bool)
newList =
    let
        listLen = gridWidth * gridHeight
    in
        Random.list listLen maybeTrue


-- MODEL


init : () -> (Model, Cmd Msg)
init _ =
  ( initialModel
  , Random.generate NewGrid newList
  )

cellWidth: Int
cellWidth = 12

cellHeight: Int
cellHeight = cellWidth

type alias GridData =
    { grid: CellGrid Bool
    , style: CellStyle Bool
    }
gridData : GridData
gridData =
    { grid = CG.initialize myDimensions (\ i j -> False)
    , style = myStyle
    }

myStyle : CellStyle Bool
myStyle =
    { toColor = \z -> Color.rgb (colourFromBool z) 0 0
    , cellWidth = toFloat cellWidth
    , cellHeight = toFloat cellHeight
    , gridLineColor = Color.rgb 180 0 0
    , gridLineWidth = 0.5
    }

type alias PageObjects =
    { started : Bool
    , timeInputText : String
    , timeInputValid : Bool
    , timeFloat : Float
    }


type alias Model =
    { gridData: GridData
    , pageObjects: PageObjects
    }
initialModel : Model
initialModel =
    { gridData = gridData
    , pageObjects =
        { started = False
        , timeInputText = "250"
        , timeInputValid = True
        , timeFloat = 250
        }
    }


gridWidth : Int
gridWidth = 40  -- gridWidth?

gridHeight : Int
gridHeight = gridWidth

truthPercent : Int
truthPercent = 33


colourFromBool : Bool -> Float
colourFromBool truthyness =
    if truthyness then 200 else 0


display : {width : Int, height : Int}
display = {width = cellWidth * gridWidth, height = cellHeight * gridHeight}

myDimensions = CG.Dimensions gridWidth gridHeight


-- VIEW


gridView : Model -> Html Msg
gridView model = asHtml display model.gridData.style model.gridData.grid
    |> Html.map GridMsg


view : Model -> Html Msg
view model =
    EL.layout [] <|
    EL.column [ EL.width fill, EL.height fill, EL.alignTop, padding 50]
        [ el [EL.centerX, padding 25, Font.size 50] ( text "Conway's Game of Life")
        , EL.row [EL.centerX] [gridView model |> EL.html]
        , EL.row [EL.centerX, padding 20]
            [ el [] ( text "Time in ms: ")
            , Html.input [ placeholder "", value model.pageObjects.timeInputText, onInput UpdateTimeStep ] [] |> EL.html
            , Html.button [ onClick RandomiseGrid ] [ Html.text "New Grid" ] |> EL.html
            , Html.button [ onClick Start ] [ Html.text "Start" ] |> EL.html
            , Html.button [ onClick Stop ] [ Html.text "Stop" ] |> EL.html
            ]
        , EL.row [EL.centerX] [viewTimeStepError model |> EL.html]
        ]


-- type sig
viewTimeStepError : Model -> Html msg
viewTimeStepError model =
    let
        valid = model.pageObjects.timeInputValid
    in
        case valid of -- view can read off 'validity' from model
            False ->
                Html.div [ Html.Attributes.style "color" "red" ] [ Html.text "Invalid time entered" ]
            True ->
                Html.div [] []

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        time = model.pageObjects.timeFloat
        valid = model.pageObjects.timeInputValid
        updateRequired = valid && model.pageObjects.started
    in
        case updateRequired of
            True ->
                Time.every time UpdateGrid
            False ->
                Sub.none

-- Update

type Msg = RandomiseGrid
         | NewGrid (List Bool)
         | GridMsg CGR.Msg
         | UpdateTimeStep String
         | Start
         | Stop
         | UpdateGrid Time.Posix

type alias BoolGrid = CellGrid Bool

lifeRule : Int -> Int -> CellGrid Bool -> Bool
lifeRule x y grid =
    let
        position = { row = x, column = y}
        neighbours = CG.neighbors position grid
        trueValues = List.filter (\a -> a) neighbours
        truthCount = List.length trueValues
        cell = (CG.get position grid) |> Maybe.withDefault False
    in
        if truthCount == 3
        then True
        else if cell
        then truthCount == 2
        else False


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        pageObjects = model.pageObjects
    in
    case msg of
        GridMsg _ ->
            ( model, Cmd.none)

        RandomiseGrid ->
            ( model
            , Random.generate NewGrid newList
            )

        NewGrid my_list ->
            let
                new_grid : Maybe (CellGrid Bool)
                new_grid = CG.fromList myDimensions my_list
            in
                case new_grid of
                    Nothing ->
                        (model, Cmd.none)
                    Just grid ->
                        ( { model | gridData = { gridData | grid = grid} }
                        , Cmd.none
                        )
        UpdateTimeStep input ->
            let
                maybeFloat = String.toFloat input
            in
                case maybeFloat of
                    Just x ->
                        ( { model | pageObjects =
                            { pageObjects | timeInputText = input
                                          , timeInputValid = True
                                          , timeFloat = x
                                          } } , Cmd.none)
                    Nothing ->
                        ( { model | pageObjects =
                            { pageObjects | timeInputText = input
                                          , timeInputValid = False
                                          } } , Cmd.none)

        UpdateGrid _ ->
            let grid = model.gridData.grid
            in
                ( { model | gridData = { gridData | grid = CG.transform lifeRule grid} }
                , Cmd.none
                )

        Start ->
            ( { model | pageObjects = { pageObjects | started = True }} , Cmd.none)

        Stop ->
            ( { model | pageObjects = { pageObjects | started = False }} , Cmd.none)
