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

import Element as EL exposing (Element, el, text, fill, width, padding)
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


maybe_true : Generator Bool
maybe_true = Random.map (\n -> n <= truth_percent) (Random.int 1 100)

new_list : Generator (List Bool)
new_list =
    let
        list_len = width * height
    in
        Random.list list_len maybe_true


-- MODEL


init : () -> (Model, Cmd Msg)
init _ =
  ( initModel
  , Random.generate NewGrid new_list
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
    { grid = CG.initialize my_dimensions (\ i j -> False)
    , style = my_style
    }

my_style : CellStyle Bool
my_style =
    { toColor = \z -> Color.rgb (colour_from_bool z) 0 0
    , cellWidth = toFloat cellWidth
    , cellHeight = toFloat cellHeight
    , gridLineColor = Color.rgb 180 0 0
    , gridLineWidth = 0.5
    }

type alias PageObjects =
    { started : Bool
    , time_step : String
    }


type alias Model =
    { gridData: GridData
    , pageObjects: PageObjects
    }
initModel : Model
initModel =
    { gridData = gridData
    , pageObjects =
        { started = False
        , time_step = "250"
        }
    }


width : Int
width = 40

height : Int
height = width


truth_percent : Int
truth_percent = 33


colour_from_bool : Bool -> Float
colour_from_bool truthyness =
    if truthyness then 200 else 0


display : {width : Int, height : Int}
display = {width = cellWidth * width, height = cellHeight * height}

my_dimensions = CG.Dimensions width height


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
            , Html.input [ placeholder "", value model.pageObjects.time_step, onInput UpdateTimeStep ] [] |> EL.html
            , Html.button [ onClick RandomiseGrid ] [ Html.text "New Grid" ] |> EL.html
            , Html.button [ onClick Start ] [ Html.text "Start" ] |> EL.html
            , Html.button [ onClick Stop ] [ Html.text "Stop" ] |> EL.html
            ]
        , EL.row [EL.centerX] [viewTimeStepValidation model |> EL.html]
        ]



viewTimeStepValidation model =
    let
        input = model.pageObjects.time_step
        timeStep = String.toInt input
    in
        case timeStep of
            Nothing ->
                Html.div [ Html.Attributes.style "color" "red" ] [ Html.text "Invalid time entered" ]
            Just x ->
                Html.div [] []

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        time = String.toFloat model.pageObjects.time_step
    in
    case time of
        Nothing ->
            Sub.none
        Just x ->
            case model.pageObjects.started of
                True ->
                    Time.every x Step
                False ->
                    Sub.none

-- Update

type Msg = RandomiseGrid
         | NewGrid (List Bool)
         | GridMsg CGR.Msg
         | UpdateTimeStep String
         | Start
         | Stop
         | Step Time.Posix

type alias BoolGrid = CellGrid Bool

life_rule : Int -> Int -> CellGrid Bool -> Bool
life_rule x y grid =
    let
        position = { row = x, column = y}
        neighbours = CG.neighbors position grid
        true_values = List.filter (\a -> a) neighbours
        truth_count = List.length true_values
        cell : Maybe Bool
        cell = (CG.get position grid)
        cell2 = case cell of
            Nothing -> False
            Just contents -> contents
    in
        if truth_count == 3
        then True
        else if cell2
        then truth_count == 2
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
            , Random.generate NewGrid new_list
            )

        NewGrid my_list ->
            let
                new_grid : Maybe (CellGrid Bool)
                new_grid = CG.fromList my_dimensions my_list
            in
                case new_grid of
                    Nothing ->
                        (model, Cmd.none)
                    Just grid ->
                        ( { model | gridData = { gridData | grid = grid} }
                        , Cmd.none
                        )
        UpdateTimeStep input ->
            ( { model | pageObjects = { pageObjects | time_step = input } } , Cmd.none)

        Step posix ->
            let grid = model.gridData.grid
            in
                ( { model | gridData = { gridData | grid = CG.transform life_rule grid} }
                , Cmd.none
                )

        Start ->
            ( { model | pageObjects = { pageObjects | started = True }} , Cmd.none)

        Stop ->
            ( { model | pageObjects = { pageObjects | started = False }} , Cmd.none)
