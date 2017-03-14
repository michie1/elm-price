port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import FormatNumber
import FormatNumber.Locales
import Keyboard
import Keyboard.Extra
import Html.Events


type alias Model =
    { price : Int
    , keyboardState : Keyboard.Extra.State
    , focus : Bool
    }


model : Model
model =
    { price = 10000
    , keyboardState = Keyboard.Extra.initialState
    , focus = False
    }


main : Program Never Model Msg
main =
    program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , Keyboard.ups KeyUp
        , focus Focus
        ]


type Msg
    = Increment
    | Decrement
    | KeyboardExtraMsg Keyboard.Extra.Msg
    | KeyUp Keyboard.KeyCode
    | OnClick Int
    | Focus Bool
    | Clear
    | Backspace


limitPrice : Int -> Int
limitPrice price =
    Basics.min price 100000000000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | price = model.price + 1 }, Cmd.none )

        Decrement ->
            ( { model | price = model.price - 1 }, Cmd.none )

        KeyboardExtraMsg keyMsg ->
            ( { model | keyboardState = Keyboard.Extra.update keyMsg model.keyboardState }
            , Cmd.none
            )

        KeyUp key ->
            if model.focus then
                let
                    price =
                        if key == 46 || key == 8 then
                            -- backspace, delete
                            model.price // 10
                        else if key == 67 then
                            0
                            -- c
                        else
                            let
                                n =
                                    if key >= 48 && key <= 57 then
                                        Just <| key - 48
                                    else if key == 45 then
                                        Just 0
                                    else if key == 35 then
                                        Just 1
                                    else if key == 40 then
                                        Just 2
                                    else if key == 34 then
                                        Just 3
                                    else if key == 37 then
                                        Just 4
                                    else if key == 12 then
                                        Just 5
                                    else if key == 39 then
                                        Just 6
                                    else if key == 36 then
                                        Just 7
                                    else if key == 38 then
                                        Just 8
                                    else if key == 33 then
                                        Just 9
                                    else
                                        Nothing
                            in
                                case n of
                                    Nothing ->
                                        model.price

                                    Just value ->
                                        model.price * 10 + value
                in
                    ( { model | price = limitPrice price }, Cmd.none )
            else
                ( model, Cmd.none )

        OnClick n ->
            ( { model | price = limitPrice (model.price * 10 + n) }, Cmd.none )

        Focus value ->
            ( { model | focus = value }, Cmd.none )

        Clear ->
            ( { model | price = 0 }, Cmd.none )

        Backspace ->
            ( { model | price = model.price // 10 }, Cmd.none )


myLocale : FormatNumber.Locales.Locale
myLocale =
    { decimals = 2
    , thousandSeparator = "."
    , decimalSeparator = ","
    }


view : Model -> Html Msg
view model =
    let
        focusClass =
            if model.focus then
                "focus"
            else
                "no-focus"
    in
        div [ Html.Attributes.id "inner", class focusClass ]
            [ span [] [ text <| "€" ++ (FormatNumber.format myLocale <| (toFloat model.price) / 100) ]
            --, div [] [ input [ Html.Attributes.value <| "€" ++ (FormatNumber.format myLocale <| (toFloat model.price) / 100) ] [] ]
            , div []
                [ div []
                    [ button [ Html.Events.onClick (OnClick 7) ] [ text "7" ]
                    , button [ Html.Events.onClick (OnClick 8) ] [ text "8" ]
                    , button [ Html.Events.onClick (OnClick 9) ] [ text "9" ]
                    ]
                , div []
                    [ button [ Html.Events.onClick (OnClick 4) ] [ text "4" ]
                    , button [ Html.Events.onClick (OnClick 5) ] [ text "5" ]
                    , button [ Html.Events.onClick (OnClick 6) ] [ text "6" ]
                    ]
                , div []
                    [ button [ Html.Events.onClick (OnClick 1) ] [ text "1" ]
                    , button [ Html.Events.onClick (OnClick 2) ] [ text "2" ]
                    , button [ Html.Events.onClick (OnClick 3) ] [ text "3" ]
                    ]
                , div []
                    [ button [ Html.Events.onClick (OnClick 0) ] [ text "0" ]
                    , button [ Html.Events.onClick Clear ] [ text "C" ]
                    , button [ Html.Events.onClick Backspace ] [ text "backspace" ]
                    ]
                ]
            ]


port focus : (Bool -> msg) -> Sub msg
