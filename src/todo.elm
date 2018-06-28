module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Debug exposing (log)
import Json.Decode as Json
import Dict exposing (Dict)
import Mouse exposing (..)


-- Styles


headerStyle : List ( String, String )
headerStyle =
    [ ( "color", "#4d4d4d" )
    , ( "min-width", "230px" )
    , ( "max-width", "550px" )
    , ( "margin", "0 auto" )
    , ( "font-family", "arial" )
    ]


textBoxStyle : List ( String, String )
textBoxStyle =
    [ ( "padding", "10px 12px" )
    , ( "border", "1px solid rgba(0, 0, 0, 0.17)" )
    , ( "border-radius", "4px" )
    , ( "background-color", "white" )
    , ( "display", "inline-block" )
    , ( "margin", "0 5px" )
    , ( "font-size", "20px" )
    , ( "width", "405px" )
    ]


searchTextBoxStyle : List ( String, String )
searchTextBoxStyle =
    [ ( "padding", "10px 12px" )
    , ( "border", "1px solid rgba(0, 0, 0, 0.17)" )
    , ( "border-radius", "4px" )
    , ( "background-color", "white" )
    , ( "display", "inline-block" )
    , ( "margin", "0 5px" )
    , ( "font-size", "14px" )
    , ( "width", "225px" )
    ]


btnStyle : List ( String, String )
btnStyle =
    [ ( "display", "inline-block" )
    , ( "padding", "6px 12px" )
    , ( "margin-bottom", "0" )
    , ( "font-size", "20px" )
    , ( "font-weight", "400" )
    , ( "line-height", "1.42857143" )
    , ( "text-align", "center" )
    , ( "white-space", "nowrap" )
    , ( "cursor", "pointer" )
    , ( "user-select", "none" )
    , ( "border", "1px solid transparent" )
    , ( "border-radius", "4px" )
    , ( "color", "#fff" )
    , ( "background-color", "#286090" )
    , ( "border-color", "#204d74" )
    ]


toListUlStyle : List ( String, String )
toListUlStyle =
    [ ( "margin", "0" )
    , ( "padding", "0" )
    , ( "list-style", "none" )
    , ( "float", "left" )
    , ( "margin-top", "15px" )
    ]


toListUlLiStyle : List ( String, String )
toListUlLiStyle =
    [ ( "position", "relative" )
    , ( "font-size", "20px" )
    , ( "border-bottom", "1px solid #ededed" )
    , ( "float", "left" )
    , ( "padding", "10px 15px 10px 15px" )
    ]


toListUlLiLabelStyle : List ( String, String )
toListUlLiLabelStyle =
    [ ( "white-space", "pre-line" )
    , ( "word-break", "break-all" )
    , ( "line-height", "1.2" )
    , ( "transition", "color 0.4s" )
    , ( "width", "458px" )
    , ( "float", "left" )
    ]


deleteBtnStyle : List ( String, String )
deleteBtnStyle =
    [ ( "background", "0px center" )
    , ( "border", "0" )
    , ( "float", "left" )
    , ( "font-size", "30px" )
    , ( "color", "#cc9a9a" )
    , ( "transition", "color 0.2s ease-out" )
    , ( "outline", "0" )
    , ( "cursor", "pointer" )
    ]


btnSortStyle : List ( String, String )
btnSortStyle =
    [ ( "display", "inline-block" )
    , ( "padding", "6px 12px" )
    , ( "margin-bottom", "0" )
    , ( "font-size", "16px" )
    , ( "font-weight", "400" )
    , ( "line-height", "1.42857143" )
    , ( "text-align", "center" )
    , ( "white-space", "nowrap" )
    , ( "cursor", "pointer" )
    , ( "user-select", "none" )
    , ( "border", "1px solid rgba(0, 0, 0, 0.17)" )
    , ( "border-radius", "4px" )
    , ( "float", "right" )
    , ( "background-color", "#fff" )
    , ( "border-color", "1px solid rgba(0, 0, 0, 0.17)" )
    ]


btnDeleteAllStyle : List ( String, String )
btnDeleteAllStyle =
    [ ( "display", "inline-block" )
    , ( "padding", "6px 12px" )
    , ( "margin-bottom", "0" )
    , ( "font-size", "16px" )
    , ( "font-weight", "400" )
    , ( "line-height", "1.42857143" )
    , ( "text-align", "center" )
    , ( "white-space", "nowrap" )
    , ( "cursor", "pointer" )
    , ( "user-select", "none" )
    , ( "border", "1px solid rgb(204, 154, 154)" )
    , ( "border-radius", "4px" )
    , ( "float", "right" )
    , ( "background-color", "rgb(204, 154, 154)" )
    , ( "color", "#fff" )
    , ( "margin-left", "10px" )
    ]


divActionStyle : List ( String, String )
divActionStyle =
    [ ( "float", "left" )
    , ( "width", "100%" )
    ]


divWrapStyle : List ( String, String )
divWrapStyle =
    [ ( "float", "left" )
    , ( "border", "1px solid rgb(237, 237, 237)" )
    , ( "padding", "10px 15px" )
    , ( "border-radius", "4px" )
    , ( "width", "100%" )
    ]



---main program


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias ToDoItem =
    { task : String
    , taskId : Int
    , isExists : Bool
    }


type alias Model =
    { toDoList : List ToDoItem
    , searchResult : List ToDoItem
    , searchTerm : String
    , id : Int
    , task : String
    , sortOrder : Int
    }


init : ( Model, Cmd Msg )
init =
    { toDoList = []
    , searchResult = []
    , searchTerm = ""
    , id = 1
    , task = ""
    , sortOrder = -1
    }
        ! []



-- UPDATE


type Msg
    = Add
    | Delete Int
    | Search String
    | Sort
    | UpdateField String
    | DeleteAll


type SortType
    = Asc
    | Desc


newEntry : String -> Int -> ToDoItem
newEntry taskName taskId =
    { task = taskName
    , taskId = taskId
    , isExists = True
    }


toLiRow : ToDoItem -> Html Msg
toLiRow item =
    li
        [ style toListUlLiStyle ]
        [ div []
            [ label [ style toListUlLiLabelStyle ] [ text item.task ]
            , button [ style deleteBtnStyle, onClick (Delete item.taskId) ] [ text "×" ]
            ]
        ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)


viewInput : Model -> Html Msg
viewInput model =
    header
        [ style headerStyle ]
        [ h1 [ style [ ( "text-align", "center" ), ( "font-weight", "normal" ) ] ] [ text "Create ToDo List" ]
        , input
            [ style textBoxStyle
            , placeholder "Enter task name"
            , value model.task
            , onInput UpdateField
            , onEnter Add
            ]
            []
        , button [ style btnStyle, onClick Add, title "Delete Task" ]
            [ text "Add Task" ]
        , br [] []
        , br [] []
        , div
            [ style
                (if (List.length model.toDoList) > 0 then
                    divWrapStyle
                 else
                    [ ( "display", "none" ) ]
                )
            ]
            [ div
                [ style divActionStyle ]
                [ input
                    [ style searchTextBoxStyle
                    , placeholder "Search"
                    , onInput Search
                    ]
                    []
                , button [ style btnDeleteAllStyle, onClick DeleteAll, title "Delete All" ]
                    [ text "Delete All" ]
                , button [ style btnSortStyle, onClick Sort, title "Sort" ]
                    [ text
                        ("Sort"
                            ++ (if model.sortOrder == 1 then
                                    "↓"
                                else if model.sortOrder == 0 then
                                    "↑"
                                else
                                    ""
                               )
                        )
                    ]
                ]
            , ul [ style toListUlStyle ] <|
                (List.map toLiRow
                    (if String.isEmpty model.searchTerm then
                        model.toDoList
                     else
                        model.searchResult
                    )
                )
            ]
        ]



---Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateField value ->
            { model | task = value } ! []

        Add ->
            { model
                | id = model.id + 1
                , task = ""
                , toDoList =
                    model.toDoList ++ [ newEntry model.task model.id ]
            }
                ! []

        Delete id ->
            { model
                | toDoList =
                    List.filter (\i -> i.taskId /= id) model.toDoList
            }
                ! []

        DeleteAll ->
            { model
                | toDoList = []
            }
                ! []

        Search searchKeyword ->
            { model
                | searchTerm = searchKeyword
                , searchResult =
                    List.filter
                        (\i ->
                            String.contains (String.toLower searchKeyword) (String.toLower i.task)
                        )
                        model.toDoList
            }
                ! []

        Sort ->
            case model.sortOrder of
                1 ->
                    { model
                        | sortOrder = 0
                        , toDoList =
                            List.sortBy .task model.toDoList
                    }
                        ! []

                0 ->
                    { model
                        | sortOrder = 1
                        , toDoList =
                            List.reverse (List.sortBy .task model.toDoList)
                    }
                        ! []

                _ ->
                    { model
                        | sortOrder = 0
                        , toDoList =
                            List.sortBy .task model.toDoList
                    }
                        ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---View


view : Model -> Html Msg
view model =
    div [ class "todomvc-wrapper" ]
        [ section
            [ class "todoapp" ]
            [ viewInput model
            ]
        ]
