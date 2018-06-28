module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Array exposing (..)
import Debug
import Time exposing (Time)
import Task
import Date exposing (Date)
import Json.Decode exposing (Decoder, int, string)
import Dict exposing (..)


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type alias Model =
    { list : String
    , serachTerm : String
    , findItemResult : String
    , leapYears : String
    , year : Int
    , enteredListToFindLargestNumber : String
    , largestNumber : Int
    , digits : String
    , digitsList : String
    , enteredListToGetSum : String
    , sumOfList : Int
    , enteredListToGetSumOrProduct : String
    , sumOrProductOfList : Int
    , enterdListForSorting : String
    , sortedList : List Int
    , combinedFlatternList : List Int
    , mapedAlterNateItemsList : List String
    , mergedSortedList : List Int
    , enterdRotationList : String
    , rotationTimes : Int
    , swapedList : List Int
    , fibonacciSeries : String
    , isNumberArmstrong : Bool
    , capitalOrCountry : String
    , binaryConversion : String
    }


model : Model
model =
    { list = toString listOfElements
    , serachTerm = ""
    , findItemResult = ""
    , leapYears = ""
    , year = 0
    , enteredListToFindLargestNumber = ""
    , largestNumber = 0
    , digits = ""
    , digitsList = ""
    , enteredListToGetSum = ""
    , sumOfList = 0
    , enteredListToGetSumOrProduct = ""
    , sumOrProductOfList = 0
    , enterdListForSorting = ""
    , sortedList = []
    , combinedFlatternList = []
    , mapedAlterNateItemsList = []
    , mergedSortedList = []
    , enterdRotationList = ""
    , rotationTimes = 0
    , swapedList = []
    , fibonacciSeries = ""
    , isNumberArmstrong = False
    , capitalOrCountry = ""
    , binaryConversion = ""
    }


listOfElements =
    [ "1", "2", "3", "4" ]


capitalCountries : Dict String String
capitalCountries =
    Dict.fromList
        [ ( "Afghanistan", "Kabul" )
        , ( "Albania", "Tirana" )
        , ( "Algeria", "Algiers" )
        , ( "Andorra", "Andorra la Vella" )
        , ( "Angola", "Luanda" )
        , ( "Antigua and Barbuda", "Saint John's" )
        , ( "Argentina", "Buenos Aires" )
        , ( "Armenia", "Yerevan" )
        , ( "Australia", "Canberra" )
        , ( "Austria", "Vienna" )
        , ( "Azerbaijan", "Baku" )
        ]



-- UPDATE


type Msg
    = SetYear String
    | SetListValues String
    | SetRuningListSumValues String
    | SetRuningListSumOrProductValues String
    | SetEnterdListForSorting String
    | SetRotatedList String
    | SetRotationTime String
    | GetLeapYears
    | FindLargestNumber
    | FindElementOccuranceInsideList String
    | TakeNumberAndReturnListOfDigits String
    | CalculateSumOrProductOfList Bool
    | CalculateSumOfList
    | SortList
    | CombinedList
    | MapAlterNateItems
    | MergeTwoSortedListIntoOne
    | RotateListKthTimes
    | PrintFibonacciSeries String
    | CheckArmstrongNumber String
    | FindCapitalOrCountry String
    | ConvertBaseToBinary String


convertCommaSeperatedStringToListOfInt : String -> List Int
convertCommaSeperatedStringToListOfInt values =
    let
        listOfString =
            String.split "," values

        listOfIntegers =
            List.map (\value -> (String.toInt value |> Result.withDefault 0)) listOfString
    in
        listOfIntegers


getElementAt : Array String -> Int -> String
getElementAt listOfArray position =
    Array.get position listOfArray
        |> Maybe.withDefault ""


swapElements : Int -> Int -> Array Int -> Array Int
swapElements start end arrayObj =
    if start == end then
        arrayObj
    else
        let
            oldValue =
                Array.get start arrayObj |> Maybe.withDefault 0

            newValue =
                Array.get (start + 1) arrayObj |> Maybe.withDefault 0
        in
            swapElements (start + 1) end (Array.set (start + 1) oldValue (Array.set start newValue arrayObj))


rotateAListForKthTimes : Int -> Int -> Array Int -> List Int
rotateAListForKthTimes start rotation listObj =
    if start == rotation then
        Array.toList listObj
    else
        rotateAListForKthTimes (start + 1) rotation (swapElements 0 ((Array.length listObj) - 1) listObj)


calculateMaxValue : Int -> Int -> Int
calculateMaxValue maxValue length =
    if maxValue > length then
        maxValue
    else
        length


getMaxLengthFromArrayOfString : Array String -> Int -> Int -> Int
getMaxLengthFromArrayOfString array position maxValue =
    if (position == (Array.length array) - 1) then
        calculateMaxValue maxValue (String.length (getElementAt array position))
    else
        let
            length =
                calculateMaxValue maxValue (String.length (getElementAt array (position + 1)))
        in
            getMaxLengthFromArrayOfString array (position + 1) length


getNumberOfLeapYears : Int -> Int -> Int -> String -> String
getNumberOfLeapYears year counter upto output =
    if counter == upto then
        output
    else if ((year % 4 == 0 && year % 100 /= 0) || (year % 400 == 0)) then
        let
            leapYaers =
                case output of
                    "" ->
                        (toString year)

                    _ ->
                        output ++ ", " ++ (toString year)
        in
            getNumberOfLeapYears (year + 1) (counter + 1) upto leapYaers
    else
        getNumberOfLeapYears (year + 1) counter upto output


getLargestNumberFromList : List Int -> Int
getLargestNumberFromList list =
    let
        {- element =
           List.sort list
               |> List.reverse
               |> List.head
               |> Maybe.withDefault 0
        -}
        element =
            List.maximum list
                |> Maybe.withDefault 0
    in
        element


mapAlterNateListElements : Int -> Array String -> Array String -> List String -> List String
mapAlterNateListElements counter listA listB outputList =
    let
        newList =
            (Array.get counter listB |> Maybe.withDefault "") :: outputList

        newList1 =
            (Array.get counter listA |> Maybe.withDefault "") :: newList
    in
        case counter of
            0 ->
                newList1

            _ ->
                mapAlterNateListElements (counter - 1) listA listB newList1


printFibonacciSeries : Int -> Int -> Int -> Int -> String -> String
printFibonacciSeries number1 number2 counter upTo series =
    let
        seriesNumber =
            number1 + number2
    in
        if upTo < 1 || counter == upTo then
            series
        else
            printFibonacciSeries number2 seriesNumber (counter + 1) upTo (series ++ " " ++ (toString seriesNumber))


printRactangle : Int -> String -> Html Msg
printRactangle maxLength value =
    div [ class "ractangle-block" ]
        [ if maxLength > (String.length value) then
            text <| "*" ++ (String.padRight maxLength '*' value) ++ "*" ++ "\n"
          else
            text <| "*" ++ (value ++ "\n") ++ "*"
        ]


checkIfNumberIsArmstrongNumber : Int -> Bool
checkIfNumberIsArmstrongNumber number =
    let
        sumOfDigits =
            String.toList (toString number)
                |> List.map
                    (\n ->
                        (String.toInt (String.fromChar n)
                            |> Result.withDefault 0
                        )
                            ^ (String.length (toString number))
                    )
                |> List.sum
    in
        if number == sumOfDigits then
            True
        else
            False


findCountryCapitalOrViseVersa : String -> List ( String, String ) -> String
findCountryCapitalOrViseVersa value =
    List.foldr
        (\( country, capital ) acc ->
            if (String.toLower country == String.toLower value) then
                "Country capital is: " ++ capital
            else if String.toLower capital == String.toLower value then
                "Capital country is: " ++ country
            else
                acc
        )
        ""


convertNumberToBinary : Int -> String -> String
convertNumberToBinary number binaryNumber =
    if (Basics.floor ((Basics.toFloat number) / 2)) == 0 then
        let
            remainder =
                (number % 2)
        in
            binaryNumber ++ (toString remainder)
    else
        let
            remainder =
                (number % 2)

            quotient =
                Basics.floor ((Basics.toFloat number) / 2)
        in
            convertNumberToBinary quotient (binaryNumber ++ (toString remainder))



---Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetYear value ->
            { model | year = (String.toInt value |> Result.withDefault 0) }

        SetListValues values ->
            { model | enteredListToFindLargestNumber = values }

        SetRuningListSumValues values ->
            { model | enteredListToGetSum = values }

        SetRuningListSumOrProductValues values ->
            { model | enteredListToGetSumOrProduct = values }

        SetEnterdListForSorting values ->
            { model | enterdListForSorting = values }

        SetRotatedList values ->
            { model | enterdRotationList = values }

        SetRotationTime value ->
            { model | rotationTimes = (String.toInt value |> Result.withDefault 0) }

        FindElementOccuranceInsideList value ->
            let
                isMember =
                    List.member value listOfElements
            in
                if isMember then
                    { model | findItemResult = value ++ " Elemnt Found!" }
                else
                    { model | findItemResult = value ++ " Elemnt Not Found!" }

        GetLeapYears ->
            { model | leapYears = toString (getNumberOfLeapYears model.year 0 20 "") }

        FindLargestNumber ->
            { model
                | largestNumber =
                    getLargestNumberFromList (convertCommaSeperatedStringToListOfInt model.enteredListToFindLargestNumber)
            }

        TakeNumberAndReturnListOfDigits value ->
            { model
                | digits = value
                , digitsList =
                    String.toList value
                        |> List.map (\char -> String.fromChar char)
                        |> List.map (\val -> (String.toInt val |> Result.withDefault 0))
                        |> toString
            }

        CalculateSumOfList ->
            { model | sumOfList = List.sum (convertCommaSeperatedStringToListOfInt model.enteredListToGetSum) }

        CalculateSumOrProductOfList isProcuct ->
            { model
                | sumOrProductOfList =
                    if isProcuct then
                        List.product (convertCommaSeperatedStringToListOfInt model.enteredListToGetSumOrProduct)
                    else
                        List.sum (convertCommaSeperatedStringToListOfInt model.enteredListToGetSumOrProduct)
            }

        SortList ->
            { model
                | sortedList =
                    convertCommaSeperatedStringToListOfInt model.enterdListForSorting
                        |> List.sort
            }

        CombinedList ->
            { model
                | combinedFlatternList =
                    List.concat [ [ 1 ], [ 2, 3, 4 ], [ 5 ], [ 6 ] ]
                        |> List.map (\val -> (String.toInt (toString val) |> Result.withDefault 0))
            }

        MapAlterNateItems ->
            { model
                | mapedAlterNateItemsList =
                    mapAlterNateListElements 2 (Array.fromList [ "a", "c", "e" ]) (Array.fromList [ "b", "d", "f" ]) []
            }

        MergeTwoSortedListIntoOne ->
            { model
                | mergedSortedList =
                    List.append [ 1, 4, 6 ] [ 2, 3, 5 ]
                        |> List.map (\val -> (String.toInt (toString val) |> Result.withDefault 0))
                        |> List.sort
            }

        RotateListKthTimes ->
            { model
                | swapedList =
                    rotateAListForKthTimes 0 model.rotationTimes (Array.fromList (convertCommaSeperatedStringToListOfInt model.enterdRotationList))
            }

        PrintFibonacciSeries value ->
            { model
                | fibonacciSeries =
                    printFibonacciSeries 0 1 0 (String.toInt value |> Result.withDefault 0) "0 1"
            }

        CheckArmstrongNumber value ->
            { model
                | isNumberArmstrong =
                    checkIfNumberIsArmstrongNumber (String.toInt value |> Result.withDefault 0)
            }

        FindCapitalOrCountry value ->
            { model
                | capitalOrCountry =
                    findCountryCapitalOrViseVersa value (Dict.toList capitalCountries)
            }

        ConvertBaseToBinary number ->
            { model
                | binaryConversion =
                    convertNumberToBinary (String.toInt number |> Result.withDefault 0) ""
                        |> String.reverse
            }



---View


view : Model -> Html Msg
view model =
    div []
        [ ---Write a function that checks whether an element occurs in a list.
          h2 [] [ text <| model.list ++ " Find the elements from the list" ]
        , br [] []
        , input
            [ placeholder "Find elemnet occurance inside List"
            , onInput FindElementOccuranceInsideList
            ]
            []
        , div [] [ text (model.findItemResult) ]

        ---Write a program that prints the next 20 leap years.
        , br [] []
        , h2 [] [ text <| "Start year " ++ toString model.year ]
        , input [ placeholder "Enter the year", onInput SetYear ] []
        , button [ onClick GetLeapYears ] [ text "GetLeapYears" ]
        , div [] [ text (model.leapYears) ]
        , br [] []

        --Write a program that asks the user for a number n and gives them the possibility
        --to choose between computing the sum and computing the product of 1,…,n.
        , h2 [] [ text <| "Enterd List is [" ++ model.enteredListToGetSumOrProduct ++ "]" ]
        , input [ placeholder "Enter comma seprated number list", onInput SetRuningListSumOrProductValues ] []
        , button [ onClick (CalculateSumOrProductOfList False) ] [ text "Calculate Sum Of List" ]
        , button [ onClick (CalculateSumOrProductOfList True) ] [ text "Calculate Product Of List" ]
        , div [] [ text (toString model.sumOrProductOfList) ]
        , br [] []

        ---Write a function that returns the largest element in a list.
        , h2 [] [ text <| "Find Largest Number From [" ++ model.enteredListToFindLargestNumber ++ "]" ]
        , input [ placeholder "Enter the year", onInput SetListValues ] []
        , button [ onClick FindLargestNumber ] [ text "Find Largest Number" ]
        , div [] [ text (toString model.largestNumber) ]
        , br [] []

        ---Write a function that takes a number and returns a list of its digits.
        , h2 [] [ text <| "Convert Entered to digits list. Entered number is: " ++ model.digits ]
        , input [ placeholder "Enter number", onInput TakeNumberAndReturnListOfDigits ] []
        , div [] [ text (toString model.digitsList) ]
        , br [] []

        ---Write a function that computes the running total of a list.
        , h2 [] [ text <| "Enterd List is [" ++ model.enteredListToGetSum ++ "]" ]
        , input [ placeholder "Enter comma seprated number list", onInput SetRuningListSumValues ] []
        , button [ onClick CalculateSumOfList ] [ text "Calculate Sum Of List" ]
        , div [] [ text (toString model.sumOfList) ]
        , br [] []

        --"Take a comma separated string of numbers in a text box.
        --On click of a button SORT , Make a list of numbers , Sort it , and print the Sorted List."
        , h2 [] [ text <| "Enterd List is [" ++ model.enteredListToGetSum ++ "]" ]
        , input [ placeholder "Enter comma seprated numbers", onInput SetEnterdListForSorting ] []
        , button [ onClick SortList ] [ text "Sort" ]
        , div [] [ text (toString model.sortedList) ]
        , br [] []

        --"Take a nested list and return a single flattened list with all values except nil/null.
        -- The challenge is to write a function that accepts an arbitrarily-deep nested list-like structure and returns a flattened structure.
        -- For Example
        -- input: [[1],[2,3,4],[5],[6]]
        -- output: [1,2,3,4,5,6]"
        , h2 [] [ text <| "List is [[1],[2,3,4],[5],[6]]" ]
        , button [ onClick CombinedList ] [ text "Combine List" ]
        , div [] [ text (toString model.combinedFlatternList) ]
        , br [] []

        -- Write a function that combines two lists by alternatingly taking elements, e.g. [a,b,c], [1,2,3] → [a,1,b,2,c,3].
        , h2 [] [ text <| "Map Two List Items [\"a\", \"c\", \"e\"]  [\"b\", \"d\", \"f\"]" ]
        , button [ onClick MapAlterNateItems ] [ text "Map Alternatingly" ]
        , div [] [ text (toString model.mapedAlterNateItemsList) ]
        , br [] []

        -- Write a function that merges two sorted lists into a new sorted list. [1,4,6],[2,3,5] → [1,2,3,4,5,6].
        -- You can do this quicker than concatenating them followed by a sort.
        , h2 [] [ text <| "Map Two Sorted List Into SOrted List [ 1, 4, 6 ] [ 2, 3, 5 ]" ]
        , button [ onClick MergeTwoSortedListIntoOne ] [ text "Merge Two Sorted List In to One" ]
        , div [] [ text (toString model.mergedSortedList) ]
        , br [] []

        -- Write a function that rotates a list by k elements. For example [1,2,3,4,5,6] rotated by two becomes [3,4,5,6,1,2].
        -- Try solving this without creating a copy of the list. How many swap or move operations do you need?
        , h2 [] [ text <| "Rotate List Kth Times, Entered List [" ++ model.enterdRotationList ++ "]   Rotation Times:" ++ (toString model.rotationTimes) ]
        , h2 [] [ text <| "Swaped operation needed is (List.length - 1)^Rotation" ]
        , input [ placeholder "Enter comma seprated number for Rotation", onInput SetRotatedList ] []
        , input [ placeholder "Enter Rotation", onInput SetRotationTime ] []
        , button [ onClick RotateListKthTimes ] [ text "Rotate" ]
        , div [] [ text (toString model.swapedList) ]
        , br [] []

        -- Write a function that computes the list of the first 100 Fibonacci numbers.
        , h2 [] [ text <| "Fibonacci numbers" ]
        , input [ placeholder "Enter Fibonacci number upto", onInput PrintFibonacciSeries ] []
        , div [] [ text (toString model.fibonacciSeries) ]
        , br [] []

        -- "Write a function that takes a list of strings an prints them, one per line, in a rectangular frame. For example the list [""Hello"", ""World"", ""in"", ""a"", ""frame""] gets printed as:
        -- **********
        -- * Hello  *
        -- * World *
        -- * in        *
        --   * a        *
        --   * frame *
        -- **********"
        , h2 [] [ text <| "Print Ractangle from the given string [\"Hello\", \"World\", \"in\", \"a\", \"frame\"]" ]
        , div [ class "ractangle-results" ]
            (List.map (printRactangle 5) [ "Hello", "World", "in", "a", "frame" ])
        , br [] []

        -- "An Armstrong number is a number that is the sum of its own digits each raised to the power of the number of digits.
        -- For example:
        -- 9 is an Armstrong number, because 9 = 9^1 = 9
        , h2 [] [ text <| "Check Number is Armstrong or Not" ]
        , input [ placeholder "Enter number", onInput CheckArmstrongNumber ] []
        , div [] [ text (toString model.isNumberArmstrong) ]
        , br [] []

        -- Keep a map(Dict) of Country:Capital. Make a text field to enter name of
        -- the country/capital and find its capital/country respectively and display it.
        , h2 [] [ text <| "Find country capital or capital/country of " ++ (toString capitalCountries) ]
        , input [ placeholder "Enter text", onInput FindCapitalOrCountry ] []
        , div [] [ text <| (toString model.capitalOrCountry) ]
        , br [] []

        -- "Convert a number, represented as a sequence of digits in one base, to any other base.
        -- Implement general base conversion. Given a number in base a, represented as a sequence of digits, convert it to base b."
        , h2 [] [ text <| "Enter number to find it's binary conversion" ]
        , input [ placeholder "Enter number", onInput ConvertBaseToBinary ] []
        , div [] [ text <| model.binaryConversion ]
        , br [] []
        ]
