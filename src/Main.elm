module Main exposing (..)

import Array
import Array2D exposing (Array2D)
import Browser
import Browser.Events as Events
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Bg
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Json.Decode as Decode
import Keyboard as Kb
import Keyboard.Arrows as KbArrows
import List.Extra as LE
import Random
import Set
import Task
import Url


-- Main --


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subs
        , onUrlChange = NewUrl
        , onUrlRequest = Link
        }



-- Model --


type Pick
    = Red
    | Green
    | Blue
    | Yellow
    | Picker
    | Empty


type alias Rightness =
    Maybe { place : Int, color : Int }


type alias Code =
    List Pick


type alias Round =
    ( Code, Rightness )


type alias GameBoard =
    Array.Array Pick


type GameState
    = Playing
    | Winner
    | Loser


type alias Model =
    { game : GameBoard
    , gameState : GameState
    , secret : Code
    , key : Nav.Key
    , url : Url.Url
    , pressedKeys : List Kb.Key
    }


type Msg
    = Link Browser.UrlRequest
    | NewUrl Url.Url
    | KeyMsg Kb.Msg
    | GenSecret
    | NewSecret Code
    | PickNext Pick
    | Reset


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model
        Array.empty
        Playing
        [ Empty, Empty, Empty, Empty ]
        key
        url
        []
    , Random.generate NewSecret genCode
    )


genCode : Random.Generator (List Pick)
genCode =
    Random.map4
        (\a b c d -> [ a, b, c, d ])
        (Random.uniform Red [ Green, Blue, Yellow ])
        (Random.uniform Red [ Green, Blue, Yellow ])
        (Random.uniform Red [ Green, Blue, Yellow ])
        (Random.uniform Red [ Green, Blue, Yellow ])



-- Update --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Link _ ->
            ( model, Cmd.none )

        NewUrl _ ->
            ( model, Cmd.none )

        KeyMsg keyMsg ->
            ( { model | pressedKeys = Kb.update keyMsg model.pressedKeys }
            , Cmd.none
            )

        GenSecret ->
            ( model
            , Random.generate NewSecret genCode
            )

        NewSecret code ->
            ( { model | secret = code }
            , Cmd.none
            )

        PickNext pick ->
            let
                gameLength =
                    Array.length model.game

                winCheck =
                    if (modBy 4 gameLength == 3) && (gameLength >= 3) then
                        model.game
                            |> Array.slice (gameLength - 3) gameLength
                            |> Array.push pick
                            |> Array.toList
                            |> LE.zip model.secret
                            |> List.foldr (\( a, b ) i -> (a == b) && i) True
                    else
                        False
            in
            if winCheck then
                ( { model
                    | game = Array.push pick model.game
                    , gameState = Winner
                  }
                , Cmd.none
                )
            else if gameLength >= 4 * 12 - 1 then
                ( { model
                    | game = Array.push pick model.game
                    , gameState = Loser
                  }
                , Cmd.none
                )
            else
                ( { model | game = Array.push pick model.game }
                , Cmd.none
                )

        Reset ->
            ( { model
                | game = Array.empty
                , gameState = Playing
              }
            , Cmd.none
            )



-- Subscriptions --


subs : Model -> Sub Msg
subs model =
    Sub.none



-- View --


view : Model -> Browser.Document Msg
view model =
    let
        drawBoard =
            case model.gameState of
                Playing ->
                    column
                        [ width fill
                        , spacing 20
                        ]
                    <|
                        List.map (gameRow model.secret) <|
                            LE.greedyGroupsOfWithStep 4 4 <|
                                Array.toList <|
                                    Array.push Picker
                                        model.game

                Winner ->
                    row
                        [ width fill
                        ]
                        [ el
                            [ Font.size 110
                            , Font.color <| inv <| rgb 0.75 0.5 1
                            ]
                          <|
                            text
                                "You Won!"
                        , el [ width <| px 60 ] none
                        , Input.button
                            [ width fill
                            , height <| px 110
                            , spacing 10
                            , padding 10
                            , Border.color <| inv <| rgb 1 1 1
                            , Border.width 3
                            , Border.rounded 10
                            , Font.size 48
                            , Font.color <| inv <| rgb 1 1 1
                            ]
                            { onPress = Just Reset
                            , label =
                                el
                                    [ centerX
                                    , centerY
                                    ]
                                <|
                                    text "RESET"
                            }
                        ]

                Loser ->
                    row
                        [ width fill
                        ]
                        [ el
                            [ Font.size 110
                            , Font.color <| inv <| rgb 0.75 0.5 1
                            ]
                          <|
                            text
                                "You Lost!"
                        , el [ width <| px 60 ] none
                        , Input.button
                            [ width fill
                            , height <| px 110
                            , spacing 10
                            , padding 10
                            , Border.color <| inv <| rgb 1 1 1
                            , Border.width 3
                            , Border.rounded 10
                            , Font.size 48
                            , Font.color <| inv <| rgb 1 1 1
                            ]
                          <|
                            { onPress = Just Reset
                            , label =
                                el
                                    [ centerX
                                    , centerY
                                    ]
                                <|
                                    text "RESET"
                            }
                        ]
    in
    { title = "Mind!"
    , body =
        [ Element.layout
            [ width fill
            , Font.family [ Font.sansSerif ]
            , Font.color <| inv <| rgb 1 1 1
            , Bg.color <| inv <| rgb 0.05 0 0.1
            , padding 50
            ]
          <|
            column
                [ width fill
                , height fill
                , spacing 20
                ]
                [ row
                    [ width fill
                    , centerX
                    , spacing 10
                    , height <| px 90
                    ]
                    [ el
                        [ width fill
                        , height <| px 110
                        , spacing 10
                        , padding 10
                        , Border.color <| inv <| rgb 0.1 0 0.2
                        , Border.width 3
                        , Border.rounded 10
                        , Font.size 64
                        , Font.color <| inv <| rgb 0.75 0.5 1
                        ]
                      <|
                        el
                            [ centerX
                            , centerY
                            ]
                        <|
                            text "Mind!"
                    , if model.gameState /= Playing then
                        row [ height fill ]
                            [ pickToColor <|
                                Maybe.withDefault Empty <|
                                    LE.getAt 0 model.secret
                            , pickToColor <|
                                Maybe.withDefault Empty <|
                                    LE.getAt 1 model.secret
                            , pickToColor <|
                                Maybe.withDefault Empty <|
                                    LE.getAt 2 model.secret
                            , pickToColor <|
                                Maybe.withDefault Empty <|
                                    LE.getAt 3 model.secret
                            ]
                      else
                        row [ height fill ]
                            [ pickToColor Empty
                            , pickToColor Empty
                            , pickToColor Empty
                            , pickToColor Empty
                            ]
                    ]
                , drawBoard
                ]
        ]
    }



--isCodeSame : Code -> Code -> Bool
--isCodeSame codeOne codeTwo =
--    List.map2


gameRow : Code -> Code -> Element Msg
gameRow secret code =
    row
        [ width fill
        , height <| px 110
        , spacing 10
        , padding 10
        , Bg.color <| inv <| rgb 0.1 0 0.2
        , Border.rounded 10
        ]
        [ pickToColor <| Maybe.withDefault Empty <| LE.getAt 0 code
        , pickToColor <| Maybe.withDefault Empty <| LE.getAt 1 code
        , pickToColor <| Maybe.withDefault Empty <| LE.getAt 2 code
        , pickToColor <| Maybe.withDefault Empty <| LE.getAt 3 code
        , el [ width <| px 10 ] none
        , code |> checkRightness secret |> rightnessToEl
        ]


pickToColor : Pick -> Element Msg
pickToColor pick =
    case pick of
        Red ->
            el
                [ width <| px 90
                , height fill
                , Bg.color <| inv <| red
                , Border.color <| inv <| gray
                , Border.width 3
                , Border.rounded 50
                ]
                none

        Green ->
            el
                [ width <| px 90
                , height fill
                , Bg.color <| inv <| green
                , Border.color <| inv <| gray
                , Border.width 3
                , Border.rounded 50
                ]
                none

        Blue ->
            el
                [ width <| px 90
                , height fill
                , Bg.color <| inv <| blue
                , Border.color <| inv <| gray
                , Border.width 3
                , Border.rounded 50
                , padding 5
                ]
                none

        Yellow ->
            el
                [ width <| px 90
                , height fill
                , Bg.color <| inv <| yellow
                , Border.color <| inv <| gray
                , Border.width 3
                , Border.rounded 50
                ]
                none

        Picker ->
            column
                [ width <| px 90
                , height fill
                , padding 4
                , spacing 3
                ]
                [ row
                    [ width fill
                    , height fill
                    , spacing 3
                    ]
                    [ Input.button
                        [ width fill
                        , height fill
                        , Bg.color <| inv <| red
                        , Border.width 2
                        , Border.color <| inv <| white
                        , Border.rounded 15
                        ]
                        { onPress = Just (PickNext Red)
                        , label = none
                        }
                    , Input.button
                        [ width fill
                        , height fill
                        , Bg.color <| inv <| blue
                        , Border.width 2
                        , Border.width 2
                        , Border.color <| inv <| white
                        , Border.rounded 15
                        ]
                        { onPress = Just (PickNext Blue)
                        , label = none
                        }
                    ]
                , row
                    [ width fill
                    , height fill
                    , spacing 3
                    ]
                    [ Input.button
                        [ width fill
                        , height fill
                        , Bg.color <| inv <| yellow
                        , Border.width 2
                        , Border.color <| inv <| white
                        , Border.rounded 15
                        ]
                        { onPress = Just (PickNext Yellow)
                        , label = none
                        }
                    , Input.button
                        [ width fill
                        , height fill
                        , Bg.color <| inv <| green
                        , Border.width 2
                        , Border.color <| inv <| white
                        , Border.rounded 15
                        ]
                        { onPress = Just (PickNext Green)
                        , label = none
                        }
                    ]
                ]

        Empty ->
            el
                [ width <| px 90
                , height fill
                , Border.width 3
                , Border.color <| inv <| gray
                , Border.rounded 50
                ]
                none


checkRightness : Code -> Code -> Rightness
checkRightness secret code =
    let
        tupleEq : ( Int, Pick ) -> ( Int, Pick ) -> Bool
        tupleEq ( a, b ) ( c, d ) =
            a == c && b == d

        pairify : Code -> List ( Int, Pick )
        pairify =
            List.indexedMap Tuple.pair

        colorCheck : Int
        colorCheck =
            let
                countEach pick =
                    pick
                        |> LE.gatherEquals
                        |> List.map
                            (\( a, b ) ->
                                pickToString a ++ (String.fromInt <| List.length b + 1)
                            )

                codeCountSet =
                    countEach code
                        |> Set.fromList

                secretCountSet =
                    countEach secret
                        |> Set.fromList
            in
            Set.intersect secretCountSet codeCountSet
                |> Set.size

        placeCheck : Int
        placeCheck =
            List.map2 tupleEq (pairify secret) (pairify code)
                |> List.filter identity
                |> List.length

        codeCompletenessCheck =
            not (List.member Picker code)
                && not (List.member Empty code)
    in
    if codeCompletenessCheck then
        Just
            { place =
                placeCheck
            , color =
                colorCheck
            }
    else
        Nothing


rightnessToEl : Rightness -> Element Msg
rightnessToEl rightness =
    let
        justRight =
            Maybe.withDefault { place = 0, color = 0 } rightness

        rightPlace =
            justRight.place

        rightColor =
            justRight.color

        attrs =
            [ width fill
            , height fill
            , Bg.color <| inv <| rgb 0 0 0
            , Border.rounded 10
            , padding 5
            ]

        pluralization n =
            if n == 1 then
                ""
            else
                "s"
    in
    case rightness of
        Just vals ->
            column attrs
                [ el
                    [ width fill
                    , height fill
                    ]
                    (el
                        [ centerY
                        ]
                     <|
                        text <|
                            String.fromInt rightPlace
                                ++ " - peg"
                                ++ pluralization rightPlace
                                ++ " in right place"
                    )
                , el
                    [ width fill
                    , height fill
                    ]
                  <|
                    el
                        [ centerY
                        , Font.color <| inv <| rgb 0.5 0.5 0.5
                        ]
                    <|
                        text <|
                            String.fromInt rightColor
                                ++ " - correct color amount"
                                ++ pluralization rightColor
                ]

        Nothing ->
            el attrs none


pickToString : Pick -> String
pickToString pick =
    case pick of
        Red ->
            "Red"

        Green ->
            "Green"

        Blue ->
            "Blue"

        Yellow ->
            "Yellow"

        Picker ->
            "Picker"

        Empty ->
            "Empty"



--


isInverted =
    --True
    False


inv : Color -> Color
inv color =
    case isInverted of
        True ->
            let
                rgb =
                    color |> toRgb
            in
            { rgb
                | red = 1.0 - rgb.red
                , green = 1.0 - rgb.green
                , blue = 1.0 - rgb.blue
            }
                |> fromRgb

        False ->
            color


red : Color
red =
    rgb 0.9 0.1 0


blue : Color
blue =
    rgb 0 0.15 0.75


yellow : Color
yellow =
    rgb 1 0.7 0


green : Color
green =
    rgb 0.2 0.8 0


gray : Color
gray =
    rgb 0.3 0.2 0.4


white : Color
white =
    --rgb 0.05 0.6 1
    rgb 1 1 1


noBorder =
    { bottom = 0
    , left = 0
    , right = 0
    , top = 0
    }
