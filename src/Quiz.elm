module Quiz exposing (Answer, Model, Msg, init, update, view)

import Html exposing (Html)
import Html.Attributes as A
import Html.Events exposing (onCheck, onClick)


type alias Model =
    { revealed : Bool
    , multiple : Bool
    , options : List Option
    }


type alias Option =
    { label : String
    , correct : Bool
    , checked : Bool
    }


type alias Answer =
    ( Bool, String )


type Msg
    = ToggleCheck Int Bool
    | Reveal
    | Reset


init : List Answer -> Model
init answers =
    let
        options =
            List.map answerToOption answers

        multiple =
            options
                |> List.filter (\{ correct } -> correct)
                |> List.length
                |> (/=) 1
    in
    { revealed = False
    , multiple = multiple
    , options = options
    }


answerToOption : Answer -> Option
answerToOption ( correct, label ) =
    Option label correct False


update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleCheck toggleIndex newCheck ->
            { model
                | options =
                    List.indexedMap
                        (\index ({ checked } as option) ->
                            { option
                                | checked =
                                    if index == toggleIndex then
                                        newCheck

                                    else
                                        checked
                            }
                        )
                        model.options
                , revealed = not model.multiple
            }

        Reveal ->
            { model | revealed = True }

        Reset ->
            { model
                | revealed = False
                , options = List.map (\option -> { option | checked = False }) model.options
            }


view : Model -> Html Msg
view { revealed, multiple, options } =
    Html.fieldset
        []
        [ Html.ul
            [ A.style "padding-inline-start" "0"
            , A.style "list-style" "none"
            ]
            (List.indexedMap (viewOption multiple revealed) options)
        , case ( revealed, multiple ) of
            ( True, _ ) ->
                Html.button [ onClick Reset ] [ Html.text "Try again" ]

            ( False, True ) ->
                let
                    noSelection =
                        List.all (\{ checked } -> not checked) options
                in
                Html.button
                    [ onClick Reveal
                    , A.disabled noSelection
                    ]
                    [ Html.text "Check" ]

            _ ->
                Html.text ""
        ]


viewOption : Bool -> Bool -> Int -> Option -> Html Msg
viewOption multiple reveal index ({ label, correct, checked } as option) =
    Html.li []
        [ Html.label [ onCheck <| ToggleCheck index ]
            [ Html.input
                [ A.type_ "checkbox"
                , A.checked checked
                , A.disabled reveal
                ]
                []
            , Html.text (" " ++ label)
            ]
        , Html.text <|
            case ( reveal, multiple ) of
                ( True, True ) ->
                    multipleOptionComment option

                ( True, False ) ->
                    singleOptionComment option

                _ ->
                    ""
        ]


multipleOptionComment : Option -> String
multipleOptionComment { checked, correct } =
    case ( checked, correct ) of
        ( True, True ) ->
            " - ✅ Correct"

        ( True, False ) ->
            " - ❌ Incorrect"

        ( False, True ) ->
            " 👈 Missed"

        _ ->
            ""


singleOptionComment : Option -> String
singleOptionComment { checked, correct } =
    case ( checked, correct ) of
        ( True, True ) ->
            " - 🎉 Correct!"

        ( False, True ) ->
            " 👈"

        ( True, False ) ->
            " - ❌ Incorrect"

        _ ->
            ""
