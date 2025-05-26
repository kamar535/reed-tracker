module Main exposing (..)

import Array exposing (Array)
import Array.Extra
import Browser
import Color exposing (blue)
import Debug exposing (toString)
import Dict exposing (Dict)
import Duration
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes
import Html.Attributes.Extra
import List.Extra
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Maybe exposing (withDefault)
import Maybe.Extra
import Select exposing (Select)
import Set exposing (Set)
import String
import Task
import Time
import Time.Extra
import Tuple



---- MODEL ----


type Theme
    = Light
    | Dark


type alias Brand =
    { name : String
    , abbreviation : String
    , strengths : Array String
    }


type ReedAlignment
    = Over3
    | Over2
    | Over1
    | Perfect
    | Under1
    | Under2
    | Under3
    | Under4
    | Under5
    | Under6


type alias Reed =
    { id : String
    , lastUsed : Maybe Time.Posix
    , brand : Int
    , strength : Int
    , age : Maybe Int
    , rating : Maybe Int
    , note : String
    , alignment : ReedAlignment
    }


type alias Reeds =
    Dict String Reed


type alias Instrument =
    { name : String
    , reeds : Dict String Reed
    }


type SortOrder
    = Asc
    | Desc


type ReedAttribute
    = ReedID
    | ReedBrand
    | ReedStrength
    | ReedAge
    | ReedRating
    | ReedNote
    | ReedLastUsed


reedAttributeToString : ReedAttribute -> String
reedAttributeToString attribute =
    case attribute of
        ReedID ->
            "id"

        ReedBrand ->
            "brand"

        ReedStrength ->
            "strength"

        ReedAge ->
            "age"

        ReedRating ->
            "rating"

        ReedNote ->
            "note"

        ReedLastUsed ->
            "last used"


bostonSaxShop : Brand
bostonSaxShop =
    { name = "Boston Sax Shop Black Label"
    , abbreviation = "BSS"
    , strengths = Array.fromList [ "2", "2.5", "3", "3.5", "4", "4.5" ]
    }


betterSax : Brand
betterSax =
    { name = "BetterSax Jazz Cut"
    , abbreviation = "BSJ"
    , strengths = Array.fromList [ "2", "2.5", "3", "3.5", "4" ]
    }


daddarioSelectJazz : Brand
daddarioSelectJazz =
    { name = "D'Addario Select Jazz"
    , abbreviation = "DSJ"
    , strengths =
        Array.fromList
            [ "2S"
            , "2M"
            , "2H"
            , "3S"
            , "3M"
            , "3H"
            , "4S"
            , "4M"
            , "4H"
            ]
    }


testReeds : List Reed
testReeds =
    [ { id = "1"
      , lastUsed = Nothing
      , brand = 0
      , strength = 1
      , age = Nothing
      , rating = Just 4
      , note = ""
      , alignment = Over3
      }
    , { id = "2"
      , lastUsed = Nothing
      , brand = 0
      , strength = 2
      , age = Just 7
      , rating = Just 2
      , note = ""
      , alignment = Under1
      }
    , { id = "3"
      , lastUsed = Nothing
      , brand = 1
      , strength = 2
      , age = Nothing
      , rating = Nothing
      , note = ""
      , alignment = Under3
      }
    , { id = "4"
      , lastUsed = Nothing
      , brand = 1
      , strength = 2
      , age = Just 3
      , rating = Nothing
      , note = ""
      , alignment = Over3
      }
    , { id = "R1"
      , lastUsed = Nothing
      , brand = 2
      , strength = 4
      , age = Just 3
      , rating = Just 3
      , note = "Plays a little bright"
      , alignment = Under6
      }
    ]


type EditMode
    = EditAll
    | EditBasic


type alias EditReedStatus =
    { beforeOpenEditID : String
    , openFields : Set String
    , activeDelta : Maybe DeltaActive
    , deltaHrs : Int
    , deltaMin : Int
    , mode : EditMode
    , delete : Bool
    , reed : Reed
    }


type State
    = StateListReeds
    | StateEditReed EditReedStatus
    | StateSelectInstrument
    | StateEditNewInstrument String
      --| StateAddNewInstrument String
    | StateEditInstrument Int String Bool


type alias Model =
    { currentTime : Time.Posix
    , instruments : Array Instrument
    , instrument : Int
    , reeds2 : Dict String Reed
    , theme : Theme
    , sortBy : ReedAttribute
    , sortOrder : SortOrder
    , state : State
    , brands2 : Array Brand
    , selectBrand : Select Brand
    , invalid : Maybe ( ReedAttribute, String )
    , verticalSliderValue : Int
    }


initReeds : List Reed -> Dict String Reed
initReeds reeds =
    reeds |> List.map (\reed -> ( reed.id, reed )) |> Dict.fromList


initBrands : List Brand -> Dict String Brand
initBrands brands =
    brands |> List.map (\brand -> ( brand.name, brand )) |> Dict.fromList


alto =
    { name = "Alto saxophone"
    , reeds = initReeds testReeds
    }


tenor =
    { name = "Tenor saxophone"
    , reeds = initReeds testReeds
    }


baritone =
    { name = "Baritone saxophone"
    , reeds = initReeds testReeds
    }


clarinet =
    { name = "Clarinet"
    , reeds = initReeds testReeds
    }



--init : ( Model, Cmd Msg )
--init =


init : Int -> ( Model, Cmd Msg )
init currentTime =
    ( { currentTime = Time.millisToPosix currentTime
      , instruments = Array.fromList [ alto, tenor, baritone, clarinet ]
      , instrument = 0
      , reeds2 = initReeds testReeds
      , theme = Dark
      , sortBy = ReedID
      , sortOrder = Asc
      , state = StateListReeds
      , brands2 = Array.fromList [ daddarioSelectJazz, bostonSaxShop, betterSax ]
      , selectBrand = Select.init "select-brand" |> Select.setItems [ daddarioSelectJazz, bostonSaxShop, betterSax ]
      , invalid = Nothing
      , verticalSliderValue = 0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetSort ReedAttribute
    | Tick Time.Posix
    | SetLastUsed Int String Time.Posix
    | SetState State
    | AddNewInstrument String
    | EditNewInstrumentName String
    | EditInstrumentName Int String
    | SetInstrument Int
    | RequestDeleteInstrument Int String
    | ConfirmDeleteInstrument Int
    | SetInstrumentName Int String
    | EditReed Reed ReedAttribute
    | SaveReed EditReedStatus
    | CancelEditReed
    | SetRating (Maybe Int)
    | SetAge (Maybe Int)
    | SetStrengtIndex Int
    | SelectBrand (Select.Msg Brand)
    | SetBrand Int
    | SetReedId String
    | SetReedNote String
    | SetEditReed EditReedStatus
    | DeleteNote
    | CloseEditSection ReedAttribute
    | SetEditMode EditMode
    | NewReed
    | DeleteReed
    | CancelDeleteReed
    | ConfirmDeleteReed
    | VerticalSlider Int
    | Nop


flipSortOrder : SortOrder -> SortOrder
flipSortOrder sortOrder =
    case sortOrder of
        Asc ->
            Desc

        Desc ->
            Asc


intToAlignment : Int -> Maybe ReedAlignment
intToAlignment n =
    case n of
        0 ->
            Just Under6

        1 ->
            Just Under5

        2 ->
            Just Under4

        3 ->
            Just Under3

        4 ->
            Just Under2

        5 ->
            Just Under1

        6 ->
            Just Perfect

        7 ->
            Just Over1

        8 ->
            Just Over2

        9 ->
            Just Over3

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Tick posix ->
            ( { model | currentTime = posix }, Cmd.none )

        SetLastUsed _ reed posix ->
            let
                instrument =
                    getInstrument model

                newInstrument : Maybe Instrument
                newInstrument =
                    instrument
                        |> Maybe.andThen
                            (\i ->
                                Dict.get reed i.reeds
                                    |> Maybe.map (\r -> { r | lastUsed = Just posix })
                                    |> Maybe.map (\r -> Dict.insert reed r i.reeds)
                                    |> Maybe.map (\reeds -> { i | reeds = reeds })
                            )

                newModel =
                    newInstrument
                        |> Maybe.map
                            (\i -> Array.set model.instrument i model.instruments)
                        |> Maybe.map (\instruments -> { model | instruments = instruments })
                        |> withDefault model
            in
            ( { newModel | state = StateListReeds }, Cmd.none )

        SetState state ->
            ( { model | state = state }, Cmd.none )

        EditNewInstrumentName name ->
            ( { model | state = StateEditNewInstrument name }, Cmd.none )

        AddNewInstrument name ->
            let
                id =
                    Array.length model.instruments

                newInstruments =
                    model.instruments |> Array.push { name = name, reeds = Dict.empty }

                newModel =
                    { model | instrument = id, instruments = newInstruments, state = StateListReeds }
            in
            ( newModel, Cmd.none )

        EditInstrumentName id name ->
            ( { model | state = StateEditInstrument id name False }, Cmd.none )

        SetInstrument id ->
            ( { model | instrument = id, state = StateListReeds }, Cmd.none )

        RequestDeleteInstrument id name ->
            let
                state =
                    StateEditInstrument id name True

                newModel =
                    { model | state = state }
            in
            ( newModel, Cmd.none )

        ConfirmDeleteInstrument id ->
            let
                newInstruments =
                    model.instruments |> Array.Extra.removeAt id

                state =
                    if Array.length newInstruments == 0 then
                        StateEditNewInstrument "New Instrument"

                    else
                        StateListReeds

                newModel =
                    { model
                        | instrument = 0
                        , instruments = newInstruments
                        , state = state
                    }
            in
            ( newModel, Cmd.none )

        SetInstrumentName id name ->
            let
                newModel =
                    Array.get id model.instruments
                        |> Maybe.map (\i -> { i | name = name })
                        |> Maybe.map (\i -> Array.set id i model.instruments)
                        |> Maybe.map (\i -> { model | instruments = i, state = StateListReeds })
                        |> withDefault model
            in
            ( newModel, Cmd.none )

        VerticalSlider x ->
            case model.state of
                StateEditReed status ->
                    let
                        reed =
                            status.reed

                        newReed =
                            { reed | alignment = withDefault reed.alignment (intToAlignment x) }

                        newStatus =
                            { status | reed = newReed }
                    in
                    ( { model
                        | state = StateEditReed newStatus
                        , verticalSliderValue = x
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SetSort sortBy2 ->
            if model.sortBy == sortBy2 then
                ( { model | sortOrder = flipSortOrder model.sortOrder }, Cmd.none )

            else
                ( { model | sortBy = sortBy2 }, Cmd.none )

        EditReed reed _ ->
            let
                status =
                    { beforeOpenEditID = reed.id
                    , openFields = Set.empty
                    , mode = EditBasic
                    , activeDelta = Just ActiveMin
                    , deltaHrs = 0
                    , deltaMin = 0
                    , reed = reed
                    , delete = False
                    }
            in
            ( { model | state = StateEditReed status }, Cmd.none )

        CancelEditReed ->
            ( { model | state = StateListReeds }, Cmd.none )

        SetRating rating ->
            case model.state of
                StateEditReed status ->
                    let
                        finalNewRating =
                            status.reed.rating
                                |> Maybe.map
                                    (\oldRating ->
                                        rating
                                            |> Maybe.map
                                                (\newRating ->
                                                    if newRating == oldRating then
                                                        oldRating - 1

                                                    else
                                                        newRating
                                                )
                                    )
                                |> Maybe.withDefault rating
                                |> (\r ->
                                        case r of
                                            Just 0 ->
                                                Nothing

                                            x ->
                                                x
                                   )

                        reed =
                            status.reed

                        newReed =
                            { reed | rating = finalNewRating }
                    in
                    ( { model | state = StateEditReed { status | reed = newReed } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetAge age ->
            case model.state of
                StateEditReed status ->
                    let
                        reed =
                            status.reed

                        newReed =
                            { reed | age = age }

                        newStatus =
                            { status | reed = newReed }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SaveReed status ->
            let
                newInstrument : Maybe Instrument
                newInstrument =
                    getInstrument model
                        |> Maybe.map
                            (\instrument ->
                                let
                                    reeds =
                                        (if status.beforeOpenEditID == status.reed.id then
                                            instrument.reeds

                                         else
                                            Dict.remove status.beforeOpenEditID instrument.reeds
                                        )
                                            |> Dict.insert status.reed.id status.reed
                                in
                                { instrument | reeds = reeds }
                            )

                newModel =
                    newInstrument
                        |> Maybe.map
                            (\instrument ->
                                { model | instruments = Array.set model.instrument instrument model.instruments }
                            )
                        |> withDefault model

                cmd =
                    Task.perform (SetLastUsed model.instrument status.reed.id) Time.now
            in
            ( { newModel | state = StateListReeds }, cmd )

        SetStrengtIndex index ->
            case model.state of
                StateEditReed status ->
                    let
                        reed =
                            status.reed

                        newReed =
                            { reed | strength = index }

                        newStatus =
                            { status | reed = newReed }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectBrand brand ->
            Select.update SelectBrand brand model.selectBrand
                |> Tuple.mapFirst (\select -> { model | selectBrand = select })

        SetBrand brandID ->
            case model.state of
                StateEditReed status ->
                    let
                        reed =
                            status.reed

                        oldStrength =
                            reed.strength

                        oldMaxStrengthIndex =
                            Array.get reed.brand model.brands2
                                |> Maybe.map (.strengths >> Array.length)

                        newMaxStrengthIndex =
                            Array.get brandID model.brands2
                                |> Maybe.map (.strengths >> Array.length)

                        newStrength =
                            Maybe.map2 (\old new -> round <| (toFloat new / toFloat old) * toFloat reed.strength) oldMaxStrengthIndex newMaxStrengthIndex
                                |> Maybe.withDefault 0

                        strength =
                            Array.get brandID model.brands2
                                |> Maybe.map
                                    (\b ->
                                        if brandID > Array.length b.strengths then
                                            Array.length b.strengths

                                        else
                                            brandID
                                    )
                                |> Maybe.withDefault 0

                        newReed =
                            { reed | brand = brandID, strength = newStrength }

                        newStatus =
                            { status | reed = newReed }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetReedId new ->
            case model.state of
                StateEditReed status ->
                    let
                        reed =
                            status.reed

                        newReed =
                            { reed | id = new }

                        newStatus =
                            { status | reed = newReed }

                        allreadyExist =
                            getReeds model
                                |> Maybe.map
                                    (\reeds ->
                                        reeds
                                            |> Dict.filter (\k v -> k /= status.beforeOpenEditID)
                                            |> Dict.member new
                                    )

                        newModel =
                            allreadyExist
                                |> Maybe.map
                                    (\ae ->
                                        if ae then
                                            { model | invalid = Just ( ReedID, "Reed " ++ new ++ " allready exist" ), state = StateEditReed newStatus }

                                        else if String.isEmpty new then
                                            { model | invalid = Just ( ReedID, "This field cannot be empty" ), state = StateEditReed newStatus }

                                        else if String.length new > 3 then
                                            { model | invalid = Nothing }

                                        else
                                            { model | invalid = Nothing, state = StateEditReed newStatus }
                                    )
                                |> withDefault model
                    in
                    ( newModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetEditReed status ->
            ( { model | state = StateEditReed status }, Cmd.none )

        SetReedNote note ->
            case model.state of
                StateEditReed status ->
                    let
                        reed =
                            status.reed

                        newReed =
                            { reed | note = note }

                        newStatus =
                            { status | reed = newReed }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DeleteNote ->
            case model.state of
                StateEditReed status ->
                    let
                        reed =
                            status.reed

                        newReed =
                            { reed | note = "" }

                        newStatus =
                            { status | reed = newReed }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DeleteReed ->
            case model.state of
                StateEditReed status ->
                    let
                        newStatus =
                            { status | delete = True }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CancelDeleteReed ->
            case model.state of
                StateEditReed status ->
                    let
                        newStatus =
                            { status | delete = False }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ConfirmDeleteReed ->
            case model.state of
                StateEditReed status ->
                    let
                        newReeds =
                            Dict.remove status.reed.id model.reeds2
                    in
                    ( { model | reeds2 = newReeds, state = StateListReeds }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseEditSection attribute ->
            case model.state of
                StateEditReed status ->
                    let
                        newStatus =
                            { status | openFields = Set.remove (reedAttributeToString attribute) status.openFields }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetEditMode mode ->
            case model.state of
                StateEditReed status ->
                    let
                        newStatus =
                            { status | mode = mode }
                    in
                    ( { model | state = StateEditReed newStatus }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NewReed ->
            let
                reed : Reed
                reed =
                    { id = ""
                    , lastUsed = Nothing
                    , brand = 0
                    , strength = 0
                    , age = Nothing
                    , rating = Nothing
                    , note = ""
                    , alignment = Under1
                    }

                status : EditReedStatus
                status =
                    { beforeOpenEditID = ""
                    , openFields = Set.empty
                    , mode = EditAll
                        , activeDelta = Just ActiveMin
                    , deltaHrs = 0
                    , deltaMin = 0
                    , reed = reed
                    , delete = False
                    }
            in
            ( { model
                | state = StateEditReed status
                , invalid = Just ( ReedID, "This field cannot be empty" )
              }
            , Cmd.none
            )


getInstrument : Model -> Maybe Instrument
getInstrument model =
    Array.get model.instrument model.instruments


getInstrumentName : Model -> Maybe String
getInstrumentName model =
    getInstrument model |> Maybe.map (\i -> i.name)



---- VIEW ----


view : Model -> Html Msg
view model =
    layout
        [ padding 0
        , Background.color <| rgb255 32 32 32
        , Font.color <| textColor model.theme
        , inFront keypad
        ]
    <|
        el
            [ width (fill |> Element.maximum 600)
            , height fill
            , centerX
            , Background.color <| darkerGrey -- backgroundColor model.theme
            ]
        <|
            column [ width fill, padding 0, height fill ]
                [ el
                    [ width fill
                    , padding 10
                    , Background.color orange
                    , Font.color white
                    , case model.state of
                        StateListReeds ->
                            onClick <| SetState StateSelectInstrument

                        _ ->
                            emptyAttribute
                    ]
                  <|
                    case model.state of
                        StateSelectInstrument ->
                            text "Instruments"

                        StateEditInstrument _ _ _ ->
                            text "Edit instrument"

                        StateEditNewInstrument _ ->
                            text "Add new instrument"

                        _ ->
                            text <| (Array.get model.instrument model.instruments |> Maybe.map (\i -> i.name ++ " reeds") |> withDefault "No instrument")
                , el [ height (px 10) ] none

                --, el [ height (px 10) ] none
                , case model.state of
                    StateListReeds ->
                        column [ width fill ]
                            [ numOfReeds model
                                |> Maybe.map
                                    (\n ->
                                        if n > 0 then
                                            listReeds model

                                        else
                                            el [ padding 10 ] <|
                                                text "No reeds yet for this instrument."
                                    )
                                |> withDefault (text "Error: No such instrument")
                            , el [ width fill, height (px 40), Background.color black ] none
                            , el [ width fill, height (px 40), padding 10, Background.color black ] <|
                                bigButton "Add reed" darkGrey NewReed
                            ]

                    StateSelectInstrument ->
                        viewSelectInstrument model <| Array.toList model.instruments

                    StateEditNewInstrument name ->
                        column [ width fill, height fill, spacing 30, padding 10 ]
                            [ Input.text
                                [ Font.alignLeft
                                , Background.color black
                                , Border.color lightBlue
                                , Border.width 2
                                , width fill
                                ]
                                { onChange = EditNewInstrumentName
                                , text = name
                                , placeholder = Nothing
                                , label = Input.labelAbove [ paddingXY 0 20, Font.alignLeft ] <| text "Instrument name"
                                }
                            , column [ width fill, spacing 20, alignBottom ]
                                [ if Array.length model.instruments > 0 then
                                    bigButton "Cancel" black <| SetState StateListReeds

                                  else
                                    none
                                , bigButton "Save" lightBlue <| AddNewInstrument name
                                ]
                            ]

                    StateEditInstrument id name confirmeDelete ->
                        column [ width fill, height fill, spacing 30, padding 10 ]
                            [ Input.text
                                [ Font.alignLeft
                                , Background.color black
                                , Border.color lightBlue
                                , Border.width 2
                                , width fill
                                ]
                                { onChange = EditInstrumentName id
                                , text = name
                                , placeholder = Nothing
                                , label = Input.labelAbove [ paddingXY 0 20, Font.alignLeft ] <| text "Instrument name"
                                }
                            , column [ width fill, spacing 20, alignBottom ]
                                (if confirmeDelete then
                                    [ paragraph [] [ text "Are you sure you want to delte this Instrument?" ]
                                    , bigButton "No" black <| EditInstrumentName id name
                                    , bigButton "Yes" red <| ConfirmDeleteInstrument id
                                    ]

                                 else
                                    [ bigButton "Delete" black <| RequestDeleteInstrument id name
                                    , bigButton "Cancel" black <| SetState StateListReeds
                                    , bigButton "Save" lightBlue <| SetInstrumentName id name
                                    ]
                                )
                            ]

                    StateEditReed status ->
                        viewEditReed model status
                ]


viewSelectInstrument : Model -> List Instrument -> Element Msg
viewSelectInstrument model instruments =
    let
        instrument id i =
            row
                [ width fill
                , spacing 40
                ]
                [ row
                    [ width fill
                    , Element.Events.onClick <| SetInstrument id
                    ]
                    [ paragraph
                        [ alignLeft
                        , Font.alignLeft
                        , Font.color <|
                            if id == model.instrument then
                                white

                            else
                                mediumGray
                        ]
                        [ text <| i.name ]
                    , el [ alignLeft, height fill, centerX ] <|
                        el [ centerX ] <|
                            if id == model.instrument then
                                html <|
                                    Filled.check 30
                                        (Color Color.white)

                            else
                                none
                    ]
                , el
                    [ alignRight
                    , height fill
                    , centerX
                    , Font.color mediumGray
                    , onClick <| SetState <| StateEditInstrument id i.name False
                    ]
                  <|
                    html <|
                        Filled.more_vert 30 Inherit
                ]
    in
    column [ width fill, spacing 10, padding 10 ] <|
        List.indexedMap instrument (instruments |> List.sortBy .name)
            ++ [ bigButton "Add instrument" black <| SetState <| StateEditNewInstrument "New Instrument" ]


viewSelectBrand : Model -> Element Msg
viewSelectBrand model =
    let
        select =
            Select.view
                |> Select.toElement []
                    { select = model.selectBrand
                    , onChange = SelectBrand
                    , itemToString = \b -> b.name
                    , label = Input.labelAbove [] (text "Choose a brand")
                    , placeholder = Just (Input.placeholder [] (text "Type to search"))
                    }
    in
    select


selectBrandV2 : Int -> List Brand -> Element Msg
selectBrandV2 selected brands =
    let
        brand id b =
            row [ width fill, spacing 10, Element.Events.onClick <| SetBrand id ]
                [ el [ width (px 50), height fill, centerX ] <|
                    el [ centerX ] <|
                        html <|
                            (if id == selected then
                                Filled.radio_button_checked

                             else
                                Filled.radio_button_unchecked
                            )
                                30
                                (Color Color.blue)
                , paragraph [ width fill, Font.alignLeft ] [ text <| b.name ++ " (" ++ b.abbreviation ++ ")" ]
                ]
    in
    column [ spacing 10 ] <|
        List.indexedMap brand brands


red =
    rgb255 255 0 0


orange =
    rgb255 255 102 0


darkGrey =
    rgb255 28 28 28


darkerGrey =
    rgb255 18 18 18


lightGrey =
    rgb255 191 191 191


middleGrey =
    rgb255 108 108 108


lightBlue =
    rgb255 50 123 236


white =
    rgb255 255 255 255


green =
    rgb 0 255 0


yellow =
    rgb255 0 255 0


black =
    rgb255 0 0 0


lightGray =
    rgb255 211 211 211


mediumGray =
    rgb255 160 160 160


backgroundColor : Theme -> Color
backgroundColor mode =
    case mode of
        Dark ->
            darkGrey

        Light ->
            white


textColor : Theme -> Color
textColor mode =
    case mode of
        Dark ->
            lightGrey

        Light ->
            darkGrey


viewMaybeInt : Maybe Int -> Element Msg
viewMaybeInt rating =
    rating
        |> Maybe.map (String.fromInt >> text)
        |> Maybe.withDefault none


viewMaybeString : Maybe String -> Element Msg
viewMaybeString s =
    s
        |> Maybe.map text
        |> Maybe.withDefault none


headerCell : Model -> Element Msg -> ReedAttribute -> Element Msg
headerCell model name sortBy2 =
    row
        [ centerX
        , Background.color lightBlue
        , Element.Events.onClick <| SetSort sortBy2
        ]
        [ el [ width fill ] none
        , el [ width (px 20) ] none
        , el
            [ paddingEach
                { top = 10
                , bottom = 10
                , left = 0
                , right = 0
                }
            , centerX
            , width fill
            , Background.color
                lightBlue
            , Border.color lightBlue
            , Border.width 1
            , Font.color
                (if model.sortBy == sortBy2 then
                    yellow

                 else
                    white
                )
            ]
          <|
            name
        , el [ width (px 20) ] <|
            if model.sortBy == sortBy2 then
                html <|
                    (if model.sortOrder == Asc then
                        Filled.arrow_drop_down

                     else
                        Filled.arrow_drop_up
                    )
                        20
                        (Color <| Color.yellow)

            else
                none
        , el [ width fill ] none
        ]


sortOrderCell : Model -> ReedAttribute -> Element Msg
sortOrderCell model sortBy2 =
    el
        [ paddingEach
            { top = 10
            , bottom = 10
            , left = 0
            , right = 0
            }
        , centerX
        , width (px 10)
        , height fill
        , Background.color
            lightBlue
        , Border.color lightBlue
        , Border.width 1
        , Font.color white
        ]
    <|
        if model.sortBy == sortBy2 then
            el [] <|
                html <|
                    (if model.sortOrder == Asc then
                        Filled.arrow_drop_down

                     else
                        Filled.arrow_drop_up
                    )
                        20
                        (Color <| Color.yellow)

        else
            el [ width (px 10) ] none


zebra : Theme -> Color
zebra mode =
    case mode of
        Dark ->
            darkerGrey

        Light ->
            lightGrey


getReedFullBrand : Model -> Reed -> String
getReedFullBrand model reed =
    Array.get reed.brand model.brands2
        |> Maybe.map (\brand -> brand.name ++ " (" ++ brand.abbreviation ++ ")")
        |> Maybe.withDefault "error"


getReedBrand : Model -> Reed -> String
getReedBrand model reed =
    Array.get reed.brand model.brands2
        |> Maybe.map .name
        |> Maybe.withDefault "error"


getReedBrandAbbreviation : Model -> Reed -> String
getReedBrandAbbreviation model reed =
    Array.get reed.brand model.brands2
        |> Maybe.map .abbreviation
        |> Maybe.withDefault "error"


getReedStrength : Model -> Reed -> String
getReedStrength model reed =
    Array.get reed.brand model.brands2
        |> Maybe.map (\brand -> Array.get reed.strength brand.strengths)
        |> Maybe.Extra.join
        |> Maybe.withDefault "error"


cell : Theme -> Int -> Reed -> ReedAttribute -> Element Msg -> Element Msg
cell mode i reed attribute e =
    el
        [ paddingEach
            { top = 10
            , bottom = 10
            , left = 0
            , right = 0
            }
        , height fill
        , Background.color <|
            case modBy 2 i of
                0 ->
                    backgroundColor mode

                _ ->
                    zebra mode
        , Element.Events.onClick <| EditReed reed attribute
        ]
        e


emptyAttribute =
    htmlAttribute <| Html.Attributes.Extra.empty


maybeCompare : (a -> a -> Order) -> Maybe a -> Maybe a -> Order
maybeCompare f a b =
    case a of
        Just some_a ->
            case b of
                Just some_b ->
                    f some_a some_b

                Nothing ->
                    GT

        Nothing ->
            LT


compareRating a b =
    compare a b


compareMaybeRating getter a b =
    maybeCompare compare (getter a) (getter b)


descending a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


adjustForSortOrder : SortOrder -> Order -> Order
adjustForSortOrder sortOrder order =
    order
        |> (case sortOrder of
                Asc ->
                    identity

                Desc ->
                    makeDesc
           )


sortBy : (Reed -> comparable) -> SortOrder -> Reed -> Reed -> Order
sortBy getter order a b =
    compare (getter a) (getter b)
        |> adjustForSortOrder order


sortByMaybe : (Reed -> Maybe comparable) -> SortOrder -> Reed -> Reed -> Order
sortByMaybe getter order a b =
    maybeCompare compare (getter a) (getter b)
        |> adjustForSortOrder order


sortDataDesc : (Reed -> comparable) -> (Reed -> Reed -> Order)
sortDataDesc getter =
    \a b ->
        case compare (getter a) (getter b) of
            LT ->
                GT

            EQ ->
                EQ

            GT ->
                LT


makeDesc : Order -> Order
makeDesc order =
    case order of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


sortByIDDesc : Reed -> Reed -> Order
sortByIDDesc a b =
    case compare a.id b.id of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT



-- sortByStringAsInt : Reed -> comparable


sortFoo getter order a b =
    22


sortByStringAsInt sortOrder a b =
    let
        order =
            if a == b then
                EQ

            else
                case ( a |> String.toInt, b |> String.toInt ) of
                    ( Nothing, Nothing ) ->
                        compare a b

                    ( Just _, Nothing ) ->
                        LT

                    ( Just x, Just y ) ->
                        compare x y

                    ( Nothing, Just _ ) ->
                        GT
    in
    order
        |> adjustForSortOrder sortOrder


getReeds : Model -> Maybe Reeds
getReeds model =
    model
        |> getInstrument
        |> Maybe.map .reeds


numOfReeds : Model -> Maybe Int
numOfReeds model =
    Array.get model.instrument model.instruments
        |> Maybe.map (\instrument -> Dict.size instrument.reeds)


listReeds : Model -> Element Msg
listReeds model =
    let
        table =
            Array.get model.instrument model.instruments
                |> Maybe.map (\i -> viewReedTable model i.reeds)

        error =
            text <| "No reeds for instrument " ++ String.fromInt model.instrument
    in
    withDefault error table


xxxmaybeCompare : (a -> a -> Order) -> Maybe a -> Maybe a -> Order
xxxmaybeCompare f a b =
    case a of
        Just some_a ->
            case b of
                Just some_b ->
                    f some_a some_b

                Nothing ->
                    GT

        Nothing ->
            LT


posixCompare : Time.Posix -> Time.Posix -> Order
posixCompare a b =
    compare (Time.posixToMillis a) (Time.posixToMillis b)


durationNew : Time.Posix -> Time.Posix -> String
durationNew now timeStamp =
    let
        x =
            Time.Extra.partsToPosix Time.utc (Time.Extra.Parts 8 Time.Sep 26 14 30 0 0)

        fooNow =
            Time.millisToPosix <| Time.posixToMillis now + Time.posixToMillis x

        d =
            Time.millisToPosix (Time.posixToMillis fooNow - Time.posixToMillis timeStamp)

        parts =
            Time.Extra.posixToParts Time.utc d

        ps =
            [ parts.year, parts.day, parts.hour, parts.minute, parts.second ]

        pss =
            List.map String.fromInt ps |> String.join ":"
    in
    pss



{--
durationNew2 : Time.Posix -> Time.Posix -> String
durationNew2 now timeStamp =
    let
        diff interval =
            Time.Extra.diff interval Time.utc timeStamp now

        years =
            diff Time.Extra.Year

        months =
            diff Time.Extra.Month

        days =
            diff Time.Extra.Day

        hours =
            diff Time.Extra.Hour

        minutes =
            diff Time.Extra.Minute

        seconds =
            diff Time.Extra.Second
    in
    String.fromInt years
        ++ "y"
        ++ String.fromInt months
        ++ "m"
        ++ String.fromInt days
        ++ "d"
        ++ String.fromInt hours
        ++ "h"
        ++ String.fromInt minutes
        ++ "m"
        ++ String.fromInt seconds
        ++ "s"


durationFOO : Time.Posix -> Time.Posix -> String
durationFOO now time =
    let
        d =
            Duration.from time now

        years =
            Duration.inJulianYears d |> floor

        days =
            Duration.inDays d |> floor

        days2 =
            Duration.addTo time <| Duration.julianYears (toFloat years)

        days3 =
            toFloat years |> Duration.julianYears |> Duration.addTo time

        days4 =
            Duration.from days3 now |> Duration.inDays |> round

        hours =
            Duration.inHours d |> floor

        minutes =
            Duration.inMinutes d |> floor

        secondsPrime =
            Duration.from (Duration.addTo time (Duration.minutes (minutes |> toFloat))) now |> Duration.inSeconds |> round

        seconds =
            d |> Duration.inSeconds |> round

        dstring =
            if days > 0 then
                String.fromInt days ++ " d " ++ String.fromInt hours ++ " h"

            else if hours > 0 then
                String.fromInt hours ++ " h " ++ String.fromInt minutes ++ " m"

            else if minutes > 0 then
                String.fromInt minutes
                    ++ " m "
                    ++ String.fromInt secondsPrime
                    ++ " s"

            else if seconds > 0 then
                String.fromInt seconds ++ " s"

            else
                ""
    in
    dstring

--}


viewReedTable : Model -> Reeds -> Element Msg
viewReedTable model reeds =
    let
        f : SortOrder -> Reed -> Reed -> Order
        f =
            case model.sortBy of
                ReedID ->
                    \sortOrder reedA reedB -> sortByStringAsInt sortOrder (String.toLower reedA.id) (String.toLower reedB.id)

                ReedBrand ->
                    sortBy (\r -> getReedBrand model r)

                ReedStrength ->
                    sortBy (\r -> getReedStrength model r)

                ReedAge ->
                    sortByMaybe .age

                ReedRating ->
                    sortByMaybe .rating

                ReedNote ->
                    sortBy .note

                ReedLastUsed ->
                    sortByMaybe (\r -> r.lastUsed |> Maybe.map Time.posixToMillis)

        data =
            Dict.values reeds
                |> List.sortWith (f model.sortOrder)

        ( unused, used ) =
            Dict.values reeds
                |> List.sortWith (sortByMaybe (\r -> r.lastUsed |> Maybe.map Time.posixToMillis) Asc)
                |> List.partition (\r -> Maybe.Extra.isNothing r.lastUsed)

        -- |> Maybe.Extra.values
        unused2 =
            List.map (\x -> ( "", x )) unused

        used2 =
            List.indexedMap (\i x -> ( String.fromInt <| i + 1, x )) used

        data2 =
            unused2 ++ used2

        aa =
            List.foldl (\( x, r ) acc -> Dict.insert r.id x acc) Dict.empty used2
    in
    table [ width fill, spacing 0 ]
        { data = data |> List.indexedMap (\i r -> { row = i, reed = r })
        , columns =
            [ { header = headerCell model (el [] <| text "ID") ReedID
              , width = fill
              , view = \row -> cell model.theme row.row row.reed ReedID <| text <| row.reed.id
              }
            , { header = headerCell model (el [] <| html <| Filled.history 20 Inherit) ReedLastUsed
              , width = fill

              --, view = \row -> cell model.theme row.row row.reed ReedLastUsed <| text <| (row.reed.lastUsed |> Maybe.map (\lastUsed -> durationNew model.currentTime lastUsed) |> withDefault "")
              , view = \row -> cell model.theme row.row row.reed ReedLastUsed <| text <| (Dict.get row.reed.id aa |> withDefault "")
              }
            , { header = headerCell model (el [] <| text "Brand") ReedBrand
              , width = fill
              , view = \row -> cell model.theme row.row row.reed ReedBrand <| text <| getReedBrandAbbreviation model row.reed
              }
            , { header = headerCell model (el [] <| text "#") ReedStrength
              , width = fill
              , view = \row -> cell model.theme row.row row.reed ReedStrength <| text <| getReedStrength model row.reed
              }
            , { header = headerCell model (el [] <| text "Age") ReedAge
              , width = fill
              , view = \row -> cell model.theme row.row row.reed ReedAge <| viewMaybeInt row.reed.age
              }
            , { header = headerCell model (el [] <| star model) ReedRating
              , width = fill
              , view = \row -> cell model.theme row.row row.reed ReedRating <| viewMaybeInt row.reed.rating
              }
            ]
        }


star : Model -> Element Msg
star model =
    let
        color =
            if model.sortBy == ReedRating then
                Color.yellow

            else
                Color.white
    in
    html <| Filled.star 20 (Color <| color)


verticalSlider model =
    Input.slider
        [ height <| px 60
        , width <| px 30
        , behindContent <|
            -- Slider track
            el
                [ width <| px 10
                , height fill
                , centerX
                , Background.color black
                , Border.rounded 6
                ]
                Element.none
        ]
        { onChange = VerticalSlider << round
        , label =
            Input.labelHidden ""
        , min = 0
        , max = 10
        , step = Just 0.1
        , value = toFloat model.verticalSliderValue
        , thumb = Input.defaultThumb
        }


alignmentImageName alignment =
    case alignment of
        Perfect ->
            "perfect.png"

        Over1 ->
            "over-1.png"

        Over2 ->
            "over-2.png"

        Over3 ->
            "over-3.png"

        Under1 ->
            "under-1.png"

        Under2 ->
            "under-2.png"

        Under3 ->
            "under-3.png"

        Under4 ->
            "under-4.png"

        Under5 ->
            "under-5.png"

        Under6 ->
            "under-6.png"


viewAlignment : Model -> ReedAlignment -> Element Msg
viewAlignment model alignment =
    row [ centerX ]
        [ el [ width (px 30) ] none
        , el [] <|
            image [ width (px 100) ]
                { src = "reed-alignment/" ++ alignmentImageName alignment
                , description = "bar"
                }
        , verticalSlider model
        ]


viewRating : Maybe Int -> Element Msg
viewRating rating =
    let
        r =
            rating |> Maybe.withDefault 0
    in
    column [ width fill, spacing 20 ]
        [ row [ width fill ]
            [ el [ width (px 30) ] none
            , el [ centerX, height (px 30), Font.size 40 ] <| el [ centerX ] <| text <| (Maybe.map String.fromInt rating |> Maybe.withDefault "")
            , el [ width (px 30) ] <| none
            ]
        , row [ width fill, paddingEach { top = 0, bottom = 20, left = 0, right = 0 } ]
            [ row [ centerX, spacing 5 ] <|
                (List.range 1 10
                    |> List.map
                        (\i ->
                            el [ Element.Events.onClick <| SetRating (Just i) ] <|
                                html <|
                                    Filled.star 26 <|
                                        Color <|
                                            if i > r then
                                                Color.black

                                            else
                                                Color.yellow
                        )
                )
            ]
        ]


functionKey attributes element =
    el
        ([ width (px 80)
         , height fill
         , Border.rounded 5
         , Background.color darkGrey
         ]
            ++ attributes
        )
    <|
        el [ centerY, centerX ] <|
            element


okKey : Element Msg
okKey =
    functionKey [] <| text "OK"


clearKey : Element Msg
clearKey =
    functionKey [] <| text "C"


backSpaceKey : Element Msg
backSpaceKey =
    functionKey [] <|
        html <|
            Outlined.backspace 30 Inherit


keypadKeyAttributes : List (Attribute msg)
keypadKeyAttributes =
    [ width (px 40)
    , height (px 40)
    , padding 10

    -- , Border.color white
    -- , Border.width 1
    , Border.rounded 5
    , Background.color darkGrey
    ]


numKey : Int -> Element Msg
numKey n =
    el keypadKeyAttributes <| text <| String.fromInt n


noneKey attributes =
    functionKey attributes none


keypad : Element Msg
keypad =
    el
        [ htmlAttribute <| Html.Attributes.style "position" "fixed"
        , htmlAttribute <| Html.Attributes.style "bottom" "0"
        , Background.color black
        , width fill
        ]
    <|
        row
            [ spacing 20
            , padding 20
            , Font.size 20
            , centerX

            --, Border.color white
            --, Border.width 1
            , Border.roundEach { topLeft = 5, topRight = 5, bottomLeft = 0, bottomRight = 0 }
            ]
            [ column [ spacing 20 ]
                [ row [ spacing 20 ] <| List.map numKey [ 1, 2, 3 ]
                , row [ spacing 20 ] <| List.map numKey [ 4, 5, 6 ]
                , row [ spacing 20 ] <| List.map numKey [ 7, 8, 9 ]
                , row [ centerX ] [ numKey 0 ]
                ]
            , column [ spacing 20, height fill ]
                [ backSpaceKey
                , clearKey
                , okKey
                , noneKey [ Background.color black ]
                ]
            ]


viewUsageOLD : Element Msg
viewUsageOLD =
    column [ spacing 10, scrollbars ]
        [ el [] <|
            column
                [ height (px 40)
                , clip
                , scrollbarY
                ]
            <|
                List.map (\x -> text <| String.pad 2 '0' <| String.fromInt x) (List.range 0 24)
        , row [ height (px 100), centerX, padding 10, spacing 20 ]
            [ row []
                [ column [ spacing 5 ]
                    [ el [ height (px 35), alignBottom, moveDown 5, centerX ] <| html <| Filled.keyboard_arrow_up 30 Inherit
                    , el [ centerX, Font.center ] <| text "10"
                    , el [ height (px 35), alignBottom, moveUp 5, centerX ] <| html <| Filled.keyboard_arrow_down 30 Inherit
                    , el [ centerX, Font.color middleGrey ] <| text "Hrs"
                    ]
                ]
            , el [ height fill, centerY ] <| el [ centerY ] <| text ":"
            , row []
                [ column [ spacing 5 ]
                    [ row []
                        [ el [ height (px 35), alignBottom, centerX, moveDown 5 ] <| html <| Filled.keyboard_arrow_up 30 Inherit
                        , el [ height (px 35), alignBottom, centerX ] <| html <| Filled.keyboard_double_arrow_up 30 Inherit
                        ]
                    , el [ centerX ] <| text "17"
                    , row []
                        [ el [ height (px 35), alignBottom, centerX, moveUp 5 ] <| html <| Filled.keyboard_arrow_down 30 Inherit
                        , el [ height (px 35), alignBottom, centerX ] <| html <| Filled.keyboard_double_arrow_down 30 Inherit
                        ]
                    , el [ centerX, Font.color middleGrey ] <| text "Min"
                    ]
                ]
            ]
        ]


viewUsage : Element Msg
viewUsage =
    viewUsageNew False Nothing (text "12") (text "17")


timeColumn : Bool -> Bool -> Element Msg -> Element Msg -> Element Msg
timeColumn editable active over under =
    column [ spacing 5 ]
        [ el
            [ centerX
            , Font.center
            , Font.size 30
            , padding 5
            , if editable then
                Background.color black

              else
                emptyAttribute
            , Border.rounded 5
            , if active then
                Border.color lightBlue

              else
                Border.color darkGrey
            , 
                Border.width 2

          
            ]
            over
        , el [ centerX, height (px 15), Font.size 15, Font.color middleGrey ] under
        ]


type DeltaActive
    = ActiveHrs
    | ActiveMin


activeToBool : Maybe DeltaActive -> DeltaActive -> Bool
activeToBool active da =
    active
        |> Maybe.map
            (\a -> a == da)
        |> Maybe.withDefault False


viewUsageNew : Bool -> Maybe DeltaActive -> Element Msg -> Element Msg -> Element Msg
viewUsageNew editable active hrs min =
    column [ spacing 0 ]
        [ row [ centerX, padding 0, spacing 0 ]
            [ timeColumn editable
                (activeToBool active ActiveHrs)
                hrs
                (text "hrs")
            , timeColumn False False (text ":") none
            , timeColumn editable (activeToBool active ActiveMin) min (text "min")
            ]
        ]


paddingBottom : Int -> Attribute Msg
paddingBottom p =
    paddingEach { bottom = p, top = 0, left = 0, right = 0 }


viewUsage2 : EditReedStatus -> Element Msg
viewUsage2 status =
    column [ width fill, paddingBottom 10 ]
        [ row [ centerX, spacing 5 ]
            [ viewUsage
            , timeColumn False False (text "+") none
            , viewUsageNew True status.activeDelta
           
                (text <| String.padLeft 2 '0' <| String.fromInt status.deltaHrs)
                (text <| String.padLeft 2 '0' <| String.fromInt status.deltaMin)
            ]
        ]


viewAge : Reed -> Element Msg
viewAge reed =
    let
        decAge =
            reed.age
                |> Maybe.map
                    (\r ->
                        if r > 1 then
                            Just (r - 1)

                        else
                            Just 1
                    )
                |> Maybe.Extra.join

        incAge =
            reed.age
                |> Maybe.map
                    (\r ->
                        r + 1
                    )
                |> Maybe.withDefault 1
                |> Just

        dec =
            Element.Events.onClick (SetAge decAge)

        inc =
            Element.Events.onClick (SetAge incAge)
    in
    row [ width fill, centerX, spacing 10 ]
        [ el [ width (px 30) ] none
        , el [ width fill ] none
        , el [ dec ] <| html <| Filled.remove_circle 40 (Color Color.blue)
        , el [ centerX, width (px 50), Font.size 40 ] <| (reed.age |> Maybe.map (\a -> text <| String.fromInt a) |> Maybe.withDefault (text ""))
        , el [ inc ] <| html <| Filled.add_circle 40 (Color Color.blue)
        , el [ width fill ] none
        , deleteButton (SetAge Nothing)
        ]


previewSection : List (Element.Attribute Msg) -> String -> Element Msg -> Element Msg
previewSection attributes title e =
    row
        ([ centerX
         , padding 0
         , spacing 10
         ]
            ++ attributes
        )
        [ el [ Font.color middleGrey ] <| text title
        , e
        ]


leftSection : List (Element.Attribute Msg) -> String -> Reed -> Msg -> Element Msg -> Element Msg
leftSection attributes title reed msg e =
    column
        ([ centerX
         , paddingEach { top = 0, bottom = 0, left = 0, right = 0 }
         , spacing 10
         , Element.Events.onClick msg
         ]
            ++ attributes
        )
        [ el [ Font.color middleGrey ] <| text title
        , e
        ]


section : String -> Maybe (Element Msg) -> Element Msg -> Element Msg
section title right e =
    column
        [ width fill
        , paddingEach { top = 0, bottom = 0, left = 0, right = 0 }
        , spacing 10
        ]
        [ row [ width fill ]
            [ el [ Font.color middleGrey ] <| text title
            , Maybe.withDefault none right
            ]
        , e
        ]


bigButton : String -> Color -> Msg -> Element Msg
bigButton txt color msg =
    el
        [ width fill
        , padding 10
        , centerX
        , Border.rounded 10
        , Background.color color
        , Element.Events.onClick msg
        ]
    <|
        text txt


button : String -> Msg -> Element Msg
button txt msg =
    el
        [ width (px 100)
        , centerX
        , padding 10
        , Font.size 15
        , Background.color black
        , Border.rounded 10
        , Element.Events.onClick msg
        ]
    <|
        text txt


deleteButton : Msg -> Element Msg
deleteButton msg =
    el [ width (px 30), Element.alignRight, Element.Events.onClick msg ] <| html <| Outlined.backspace 30 (Color Color.blue)


viewStrength : Model -> Reed -> Element Msg
viewStrength model reed =
    case model.state of
        StateEditReed status ->
            case status.mode of
                EditAll ->
                    section "Strength" Nothing <|
                        editStrength model reed

                EditBasic ->
                    previewSection [] "Strength" <|
                        el [ centerX ] <|
                            text <|
                                getReedStrength model reed

        _ ->
            none


editStrength : Model -> Reed -> Element Msg
editStrength model reed =
    let
        data =
            Array.get reed.brand model.brands2
                |> Maybe.map
                    (\brand ->
                        let
                            n =
                                Array.length brand.strengths
                        in
                        ( if reed.strength > 0 then
                            Element.Events.onClick <| SetStrengtIndex (reed.strength - 1)

                          else
                            emptyAttribute
                        , Array.get reed.strength brand.strengths
                            |> Maybe.withDefault "Error"
                        , if reed.strength < n - 1 then
                            Element.Events.onClick <| SetStrengtIndex (reed.strength + 1)

                          else
                            emptyAttribute
                        )
                    )
    in
    data
        |> Maybe.map
            (\( prev, current, next ) ->
                column [ width fill, centerX, spacing 10 ]
                    [ row [ centerX, spacing 10 ]
                        [ el [ width fill ] none
                        , el [ prev ] <|
                            html <|
                                Filled.remove_circle 30 (Color Color.blue)
                        , el [ centerX, width (px 50) ] <| text current
                        , el [ next ] <| html <| Filled.add_circle 30 (Color Color.blue)
                        , el [ width fill ] none
                        ]
                    ]
            )
        |> Maybe.withDefault (el [] <| text "Error")


viewReedID : Model -> Reed -> Element Msg
viewReedID model reed =
    case model.state of
        StateEditReed status ->
            case status.mode of
                EditAll ->
                    editReedID model reed

                EditBasic ->
                    previewSection [ Font.size 40 ] "Reed" <| el [ centerX ] <| text <| reed.id

        _ ->
            none


viewReedBrand : Model -> Reed -> Element Msg
viewReedBrand model reed =
    case model.state of
        StateEditReed status ->
            case status.mode of
                EditAll ->
                    section "Brand" Nothing <|
                        selectBrandV2 reed.brand (Array.toList model.brands2)

                EditBasic ->
                    previewSection [] "" <|
                        paragraph [ width fill ] <|
                            [ text <| getReedFullBrand model reed ]

        _ ->
            none


viewReedNote : Model -> Reed -> Element Msg
viewReedNote model reed =
    section "Note" (Just <| deleteButton DeleteNote) <|
        column [ width fill ]
            [ Input.text
                [ Font.alignLeft
                , Background.color black
                , Border.width 0
                , width fill
                ]
                { onChange = SetReedNote
                , text = reed.note
                , placeholder = Nothing
                , label = Input.labelHidden ""
                }
            , el [ height (px 10) ] none
            ]


editReedID : Model -> Reed -> Element Msg
editReedID model reed =
    section "Reed" Nothing <|
        column [ width fill, spacing 10 ]
            [ Input.text
                [ centerX
                , width (px 100)
                , Border.width 0
                , case model.invalid of
                    Just ( ReedID, _ ) ->
                        Background.color red

                    _ ->
                        Background.color black
                ]
                { onChange = SetReedId
                , text = reed.id
                , placeholder = Nothing
                , label = Input.labelHidden "Reed ID"
                }
            , case model.invalid of
                Just ( ReedID, error ) ->
                    el [ centerX ] <| text error

                _ ->
                    none
            ]


framedSection : Element Msg -> Element Msg
framedSection e =
    el [ padding 10, width fill ] <| el [ width fill, Border.width 2, Border.color middleGrey, Border.rounded 10 ] e


viewEditReed : Model -> EditReedStatus -> Element Msg
viewEditReed model status =
    column
        [ width fill
        , height fill
        , Background.color darkGrey
        , centerX
        , spacing 0
        ]
    <|
        [ framedSection <|
            column
                [ width fill
                , centerX
                , padding 10
                , spacing <|
                    case model.state of
                        StateEditReed { mode } ->
                            case mode of
                                EditAll ->
                                    15

                                EditBasic ->
                                    15

                        _ ->
                            10
                ]
            <|
                [ -- el [ width (px 100), centerX, Background.color orange, padding 10, Border.rounded 10 ] <| text reed.id
                  viewReedID model status.reed
                , viewReedBrand model status.reed
                , viewStrength model status.reed

                --viewReedNote model reed
                -- , el [ height (px 10) ] none
                , case status.mode of
                    EditAll ->
                        case model.invalid of
                            Just ( ReedID, _ ) ->
                                none

                            _ ->
                                bigButton "Done edit" black <| SetEditMode EditBasic

                    EditBasic ->
                        bigButton "Edit" black <| SetEditMode EditAll
                ]
        , framedSection <|
            column [ width fill, centerX, padding 10, spacing 0 ]
                [ section "Uage" Nothing <| viewUsage2 status
                , section "Age" Nothing <| viewAge status.reed
                , section "Rating" Nothing <| viewRating <| status.reed.rating
                , viewReedNote model status.reed
                , section "Alignment" Nothing <| viewAlignment model status.reed.alignment
                ]
        , column
            [ padding 10, spacing 20, width fill, height fill ]
          <|
            if deleteReedRequested model then
                [ el [ height fill ] none
                , paragraph [] [ text "Are you sure you want to delte this reed?" ]
                , bigButton "No" black CancelDeleteReed
                , bigButton "Yes" red ConfirmDeleteReed
                ]

            else
                [ el [ height fill ] none
                , bigButton "Cancel" black CancelEditReed
                , bigButton "Delete" black DeleteReed
                , if Maybe.Extra.isJust model.invalid then
                    none

                  else
                    bigButton "Save" lightBlue (SaveReed status)
                , el [ height fill ] none
                ]
        ]


deleteReedRequested model =
    case model.state of
        StateEditReed status ->
            status.delete

        _ ->
            False



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions --always Sub.none
        }
