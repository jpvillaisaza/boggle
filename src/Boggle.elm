module Boggle exposing (..)

import Dice exposing (Dice)

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Random
import Random.Extra
import Random.List
import Task
import Time exposing (Time)
import Window


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


internationalLetterDice : List (Dice Char)
internationalLetterDice =
    [ Dice ['E', 'T', 'U', 'K', 'N', 'O']
    , Dice ['E', 'V', 'G', 'T', 'I', 'N']
    , Dice ['D', 'E', 'C', 'A', 'M', 'P']
    , Dice ['I', 'E', 'L', 'R', 'U', 'W']
    , Dice ['E', 'H', 'I', 'F', 'S', 'E']
    , Dice ['R', 'E', 'C', 'A', 'L', 'S']
    , Dice ['E', 'N', 'T', 'D', 'O', 'S']
    , Dice ['O', 'F', 'X', 'R', 'I', 'A']
    , Dice ['N', 'A', 'V', 'E', 'D', 'Z']
    , Dice ['E', 'I', 'O', 'A', 'T', 'A']
    , Dice ['G', 'L', 'E', 'N', 'Y', 'U']
    , Dice ['B', 'M', 'A', 'Q', 'J', 'O']
    , Dice ['T', 'L', 'I', 'B', 'R', 'A']
    , Dice ['S', 'P', 'U', 'L', 'T', 'E']
    , Dice ['A', 'I', 'M', 'S', 'O', 'R']
    , Dice ['E', 'N', 'H', 'R', 'I', 'S']
    ]


spanishLetterDice : List (Dice Char)
spanishLetterDice =
    [ Dice ['Q', 'B', 'Z', 'J', 'X', 'L']
    , Dice ['T', 'O', 'U', 'O', 'T', 'O']
    , Dice ['O', 'V', 'C', 'R', 'G', 'R']
    , Dice ['A', 'A', 'A', 'F', 'S', 'R']
    , Dice ['A', 'U', 'M', 'E', 'E', 'O']
    , Dice ['E', 'H', 'L', 'R', 'D', 'O']
    , Dice ['N', 'H', 'D', 'T', 'H', 'O']
    , Dice ['L', 'H', 'N', 'R', 'O', 'D']
    , Dice ['A', 'D', 'A', 'I', 'S', 'R']
    , Dice ['U', 'I', 'F', 'A', 'S', 'R']
    , Dice ['T', 'E', 'L', 'P', 'C', 'I']
    , Dice ['S', 'S', 'N', 'S', 'E', 'U']
    , Dice ['R', 'I', 'Y', 'P', 'R', 'H']
    , Dice ['D', 'O', 'R', 'D', 'L', 'N']
    , Dice ['C', 'C', 'Ñ', 'N', 'S', 'T']
    , Dice ['T', 'T', 'O', 'T', 'E', 'M']
    , Dice ['S', 'C', 'T', 'I', 'E', 'P']
    , Dice ['E', 'A', 'N', 'D', 'N', 'N']
    , Dice ['M', 'N', 'N', 'E', 'A', 'G']
    , Dice ['U', 'O', 'T', 'O', 'Ñ', 'N']
    , Dice ['A', 'E', 'A', 'E', 'E', 'H']
    , Dice ['Y', 'I', 'F', 'P', 'S', 'R']
    , Dice ['E', 'E', 'E', 'E', 'M', 'A']
    , Dice ['I', 'T', 'A', 'T', 'I', 'E']
    , Dice ['E', 'T', 'I', 'L', 'A', 'C']
    ]


type State
    = Other
    | Over
    | Paused
    | Playing


type Language
    = English
    | Spanish


type alias Model =
    { dice : List (Dice Char)
    , language : Language
    , length : Int
    , letters : List Char
    , state : State
    , time : Time
    , windowSize : Window.Size
    }


init : (Model, Cmd Msg)
init =
    let
        model =
            { dice = spanishLetterDice
            , language = Spanish
            , length = 5
            , letters = List.repeat 25 ' '
            , state = Other
            , time = 3 * Time.minute
            , windowSize = Window.Size 0 0
            }
    in
        ( model
        , Cmd.batch
            [ Task.perform UpdateWindowSize Window.size
            , Random.generate New (Dice.gene 'A' model.dice)
            ]
        )


type Msg
    = New (List Char)
    | Pause
    | Shake
    | SwitchLanguageTo Language
    | Tick Time
    | UpdateWindowSize Window.Size


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        New c ->
            ( { model | letters = c, state = Playing, time = 3 * Time.minute }
            , Cmd.none
            )

        Pause ->
            ( { model | state = if model.state == Other then Playing else Other}
            , Cmd.none
            )

        Shake ->
            ( model
            , Random.generate New (Dice.gene 'A' model.dice)
            )

        SwitchLanguageTo language ->
            ( { model | language = language }
            , Cmd.none
            )

        Tick _ ->
            let
                t = model.time - Time.second
            in
                if t == 0 then
                    ({ model | time = t, state = Other }, Cmd.none)
                else
                    ({ model | time = t }, Cmd.none)

        UpdateWindowSize windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        sd =
            if model.state == Playing then
                [Time.every Time.second Tick]
            else
                []
    in
        Sub.batch (Window.resizes UpdateWindowSize :: sd)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
              [ viewTime model.time
              , viewButtons model
              , viewLetters model.length model.letters
              ]
        ]


viewButtons : Model -> Html Msg
viewButtons model =
    div []
        [ button [ class "button-primary", onClick Shake ] [ text "Shake" ]
        , button [ class "pause", onClick Pause ]
            [ text (if model.state == Other then "Unpause" else "Pause")
            ]
        ]


viewLanguage : Language -> Html Msg
viewLanguage language =
    let
        radio lang value msg =
            label []
                [ input
                      [ type_ "radio"
                      , name "language"
                      , onClick msg
                      , checked (lang == language)
                      ] []
                , text value
                ]
    in
        fieldset []
            [ radio English (toString English) (SwitchLanguageTo English)
            , radio Spanish (toString Spanish) (SwitchLanguageTo Spanish)
            ]


viewLetters : Int -> List Char -> Html msg
viewLetters length letters =
    viewLettersTable (List.Extra.groupsOf length letters)


viewLettersTable : List (List Char) -> Html msg
viewLettersTable letters =
    table [] [ viewLettersTableBody letters ]


viewLettersTableBody : List (List Char) -> Html msg
viewLettersTableBody letters =
    tbody [] (List.map viewLettersTableRow letters)


viewLettersTableRow : List Char -> Html msg
viewLettersTableRow letters =
    tr [] (List.map viewLettersTableCell letters)


viewLettersTableCell : Char -> Html msg
viewLettersTableCell letter =
    td [] [ h1 [ class "letter" ] [ text (String.fromChar letter) ] ]


viewTime : Time -> Html Msg
viewTime time =
    let
        minutes =
            toString (floor (Time.inMinutes time))

        seconds =
            toString (round (Time.inSeconds time) % 60)
                |> String.padLeft 2 '0'
    in
        h2 [] [ text (minutes ++ ":" ++ seconds) ]
