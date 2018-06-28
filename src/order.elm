module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import Json.Decode exposing (Decoder, int, string)
import Dict exposing (Dict)
import Mouse exposing (..)


-- dropdown Styles


mainContainer : List ( String, String )
mainContainer =
    [ ( "height", "100%" )
    , ( "background-color", "#fafafa" )
    , ( "padding", "16px" )
    ]



-- styles for dropdown container


dropdownContainer : List ( String, String )
dropdownContainer =
    [ ( "position", "relative" )
    , ( "margin", "16px" )
    , ( "width", "216px" )
    , ( "display", "inline-block" )
    , ( "fontFamily", "sans-serif" )
    , ( "fontSize", "16px" )
    ]



-- styles for main input field


dropdownInput : List ( String, String )
dropdownInput =
    [ ( "padding", "6px 12px 8px 15px" )
    , ( "margin", "0" )
    , ( "border", "1px solid rgba(0,0,0,.17)" )
    , ( "border-radius", "4px" )
    , ( "background-color", "white" )
    , ( "display", "flex" )
    , ( "alignItems", "center" )
    ]



-- disabled style


dropdownDisabled : List ( String, String )
dropdownDisabled =
    [ ( "color", "rgba(0,0,0,.54" ) ]



-- styles for the text of selected item


dropdownText : List ( String, String )
dropdownText =
    [ ( "flex", "1 0 auto" ) ]



-- styles for list container


dropdownList : List ( String, String )
dropdownList =
    [ ( "position", "absolute" )
    , ( "top", "36px" )
    , ( "border-radius", "4px" )
    , ( "box-shadow", "0 1px 2px rgba(0,0,0,.24)" )
    , ( "padding", "4px 8px" )
    , ( "margin", "0" )
    , ( "width", "200px" )
    , ( "background-color", "white" )
    ]



-- styles for list items


dropdownListItem : List ( String, String )
dropdownListItem =
    [ ( "display", "block" )
    , ( "padding", "8px 8px" )
    ]


textBoxStyle : List ( String, String )
textBoxStyle =
    [ ( "padding", "8px 12px" )
    , ( "border", "1px solid rgba(0, 0, 0, 0.17)" )
    , ( "border-radius", "4px" )
    , ( "background-color", "white" )
    , ( "display", "inline-block" )
    , ( "margin", "0 5px" )
    ]


btnStyle : List ( String, String )
btnStyle =
    [ ( "display", "inline-block" )
    , ( "padding", "6px 12px" )
    , ( "margin-bottom", "0" )
    , ( "font-size", "14px" )
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


tableStyle : List ( String, String )
tableStyle =
    [ ( "width", "100%" )
    , ( "margin-bottom", "1rem" )
    , ( "background-color", "transparent" )
    , ( "border-collapse", "collapse" )
    ]


tdStyle : List ( String, String )
tdStyle =
    [ ( "vertical-align", "middle" )
    , ( "padding", ".75rem" )
    , ( "border-top", "1px solid #dee2e6" )
    , ( "text-align", "center" )
    ]


boldStyle : List ( String, String )
boldStyle =
    [ ( "font-weight", "bold" )
    ]


thStyle : List ( String, String )
thStyle =
    [ ( "border-bottom", "2px solid #dee2e6" )
    , ( "vertical-align", "middle" )
    , ( "padding", ".75rem" )
    , ( "border-top", "1px solid #dee2e6" )
    ]



---main program


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


items : List String
items =
    Dict.keys itemsDictionary


itemsDictionary : Dict String Int
itemsDictionary =
    Dict.fromList [ ( "Coffee", 120 ), ( "Espresso", 150 ), ( "Laate", 80 ), ( "Cold", 50 ) ]


type alias OrderItem =
    { orderNumber : Int
    , itemName : String
    , quantity : Int
    , itemPrice : Int
    , total : Int
    }


type alias Model =
    { order : List OrderItem
    , id : Int
    , itemName : String
    , qty : Int
    , price : Int
    , isOpen : Bool
    , isTextBoxDisabled : Bool
    }


init : ( Model, Cmd Msg )
init =
    { order = []
    , id = 1
    , itemName = "--Select Item--"
    , qty = 1
    , price = 0
    , isOpen = False
    , isTextBoxDisabled = True
    }
        ! []



-- UPDATE


type Msg
    = Add
    | UpdateQty String
    | SelectItem String
    | DropDownClicked
    | Blur


newEntry : String -> Int -> Int -> Int -> OrderItem
newEntry name orderNo qty price =
    { itemName = name
    , quantity = qty
    , itemPrice = price
    , orderNumber = orderNo
    , total = qty * price
    }


viewItem : String -> Html Msg
viewItem itemName =
    li
        [ style dropdownListItem
        , onClick <| SelectItem itemName
        ]
        [ text itemName ]


dropDown : Model -> Html Msg
dropDown model =
    let
        selectedText =
            model.itemName

        displayStyle =
            if model.isOpen then
                ( "display", "block" )
            else
                ( "display", "none" )
    in
        div [ style dropdownContainer ]
            [ p
                [ style dropdownInput
                , onClick DropDownClicked
                ]
                [ span [ style dropdownText ] [ text <| selectedText ]
                , span [] [ text " ▾" ]
                ]
            , ul
                [ style <| displayStyle :: dropdownList ]
                (List.map viewItem items)
            ]


toTableRow : OrderItem -> Html Msg
toTableRow item =
    tr []
        [ td [ style tdStyle ] [ text (toString item.orderNumber) ]
        , td [ style tdStyle ] [ text item.itemName ]
        , td [ style tdStyle ] [ text (toString item.quantity) ]
        , td [ style tdStyle ] [ text (toString item.itemPrice) ]
        , td [ style tdStyle ] [ text (toString item.total) ]
        ]


blankTableRow : Html Msg
blankTableRow =
    tr []
        [ td [ colspan 5, style tdStyle ] [ text "No items added" ]
        ]


viewInput : Model -> Html Msg
viewInput model =
    header
        [ class "header", style [ ( "font-family", "arial" ) ] ]
        [ h1 [] [ text "Make an order" ]
        , text "Select Item:"
        , dropDown model
        , text "Quantity:"
        , input
            [ style textBoxStyle
            , placeholder "Quantity"
            , value (toString model.qty)
            , disabled model.isTextBoxDisabled
            , onInput UpdateQty
            ]
            []
        , button [ style btnStyle, disabled model.isTextBoxDisabled, onClick Add ]
            [ text "Add Item" ]
        , br [] []
        , table [ style tableStyle ]
            ([ thead []
                [ th [ style thStyle ] [ text "Order Number" ]
                , th [ style thStyle ] [ text "Item Name" ]
                , th [ style thStyle ] [ text "Qunatity" ]
                , th [ style thStyle ] [ text "Item Price" ]
                , th [ style thStyle ] [ text "Item Total" ]
                ]
             ]
                ++ List.map toTableRow model.order
                ++ [ tr []
                        [ td [ style (List.append tdStyle boldStyle) ] [ text "Total" ]
                        , td [ style tdStyle ] []
                        , td [ style tdStyle ] []
                        , td [ style tdStyle ] []
                        , td [ style (List.append tdStyle boldStyle) ] [ (toString (List.sum (List.map (\item -> item.total) model.order))) |> text ]
                        ]
                   ]
            )
        ]



---Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectItem item ->
            { model
                | itemName = item
                , price = Dict.get item itemsDictionary |> Maybe.withDefault 1
                , isOpen = False
                , isTextBoxDisabled =
                    case item of
                        "--Select Item--" ->
                            True

                        _ ->
                            False
            }
                ! []

        DropDownClicked ->
            { model | isOpen = not model.isOpen } ! []

        Blur ->
            { model | isOpen = False } ! []

        UpdateQty quantity ->
            { model
                | qty = (String.toInt quantity |> Result.withDefault 1)
                , price = model.price * model.qty
            }
                ! []

        Add ->
            { model
                | id = model.id + 1
                , itemName = ""
                , qty = 1
                , itemName = "--Select Item--"
                , price = 0
                , order =
                    if model.itemName == "--Select Item--" then
                        model.order
                    else
                        let
                            findedItem =
                                (List.head (List.filter (\i -> i.itemName == model.itemName) model.order)
                                    |> Maybe.withDefault
                                        { itemName = ""
                                        , itemPrice = 0
                                        , orderNumber = 0
                                        , quantity = 0
                                        , total = 0
                                        }
                                )
                        in
                            if findedItem.orderNumber > 0 then
                                let
                                    updateEntry order =
                                        if order.orderNumber == findedItem.orderNumber then
                                            { order
                                                | quantity = model.qty
                                                , total = model.qty * model.price
                                            }
                                        else
                                            order

                                    updatedOrders =
                                        { model | order = List.map updateEntry model.order }
                                in
                                    updatedOrders.order
                            else
                                model.order ++ [ newEntry model.itemName model.id model.qty model.price ]
            }
                ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isOpen then
        Mouse.clicks (always Blur)
    else
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
