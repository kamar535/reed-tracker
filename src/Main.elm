module Main exposing (..)

import Array exposing (Array)
import Browser
import Color
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes.Extra
import Material.Icons as Filled
import Material.Icons.Outlined as Outlined
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra
import Select exposing (Select)
import Set exposing (Set)
import String
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


type alias Reed =
    { id : String
    , brand : Int
    , strength : Int
    , age : Maybe Int
    , rating : Maybe Int
    , note : String
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
      , brand = 0
      , strength = 1
      , age = Nothing
      , rating = Just 4
      , note = ""
      }
    , { id = "2"
      , brand = 0
      , strength = 2
      , age = Just 7
      , rating = Just 2
      , note = ""
      }
    , { id = "3"
      , brand = 1
      , strength = 2
      , age = Nothing
      , rating = Nothing
      , note = ""
      }
    , { id = "4"
      , brand = 1
      , strength = 2
      , age = Just 3
      , rating = Nothing
      , note = ""
      }
    , { id = "R1"
      , brand = 2
      , strength = 4
      , age = Just 3
      , rating = Just 3
      , note = "Plays a little bright"
      }
    ]


type EditMode
    = EditAll
    | EditBasic


type alias EditReedStatus =
    { beforeOpenEditID : String
    , openFields : Set String
    , mode : EditMode
    , delete : Bool
    , reed : Reed
    }


type State
    = StateListReeds
    | StateEditReed EditReedStatus


type alias Model =
    { reeds2 : Dict String Reed
    , theme : Theme
    , sortBy : ReedAttribute
    , sortOrder : SortOrder
    , state : State
    , brands2 : Array Brand
    , selectBrand : Select Brand
    , invalid : Maybe ( ReedAttribute, String )
    }


initReeds : List Reed -> Dict String Reed
initReeds reeds =
    reeds |> List.map (\reed -> ( reed.id, reed )) |> Dict.fromList


initBrands : List Brand -> Dict String Brand
initBrands brands =
    brands |> List.map (\brand -> ( brand.name, brand )) |> Dict.fromList


init : ( Model, Cmd Msg )
init =
    ( { reeds2 = initReeds testReeds
      , theme = Dark
      , sortBy = ReedID
      , sortOrder = Asc
      , state = StateListReeds
      , brands2 = Array.fromList [ daddarioSelectJazz, bostonSaxShop, betterSax ]
      , selectBrand = Select.init "select-brand" |> Select.setItems [ daddarioSelectJazz, bostonSaxShop, betterSax ]
      , invalid = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = SetSort ReedAttribute
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
    | Nop


flipSortOrder : SortOrder -> SortOrder
flipSortOrder sortOrder =
    case sortOrder of
        Asc ->
            Desc

        Desc ->
            Asc


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
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
                newReeds =
                    (if status.beforeOpenEditID == status.reed.id then
                        model.reeds2

                     else
                        Dict.remove status.beforeOpenEditID model.reeds2
                    )
                        |> Dict.insert status.reed.id status.reed
            in
            ( { model | reeds2 = newReeds, state = StateListReeds }, Cmd.none )

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
                            model.reeds2
                                |> Dict.filter (\k v -> k /= status.beforeOpenEditID)
                                |> Dict.member new

                        newModel =
                            if allreadyExist then
                                { model | invalid = Just ( ReedID, "Reed " ++ new ++ " allready exist" ), state = StateEditReed newStatus }

                            else if String.isEmpty new then
                                { model | invalid = Just ( ReedID, "This field cannot be empty" ), state = StateEditReed newStatus }

                            else if String.length new > 3 then
                                { model | invalid = Nothing }

                            else
                                { model | invalid = Nothing, state = StateEditReed newStatus }
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
                    , brand = 0
                    , strength = 0
                    , age = Nothing
                    , rating = Nothing
                    , note = ""
                    }

                status : EditReedStatus
                status =
                    { beforeOpenEditID = ""
                    , openFields = Set.empty
                    , mode = EditAll
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



---- VIEW ----


view : Model -> Html Msg
view model =
    layout
        [ padding 0
        , Background.color <| rgb255 32 32 32
        , Font.color <| textColor model.theme
        ]
    <|
        el
            [ width (fill |> Element.maximum 600)
            , height fill
            , centerX
            , Background.color <| black -- backgroundColor model.theme
            ]
        <|
            column [ width fill, padding 0, height fill ]
                [ el
                    [ width fill
                    , padding 10
                    , Background.color orange
                    , Font.color white
                    ]
                  <|
                    text "Alto Saxophone"

                --, el [ height (px 10) ] none
                , case model.state of
                    StateListReeds ->
                        column [ width fill ]
                            [ viewTable model
                            , el [ width fill, Background.color black ] none
                            , el [ width fill, height (px 40), Background.color black ] none
                            , el [ width fill, height (px 40), padding 10, Background.color black ] <|
                                bigButton "Add reed" darkGrey NewReed
                            ]

                    StateEditReed status ->
                        viewEditReed model status
                ]


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


viewTable : Model -> Element Msg
viewTable model =
    let
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

        data =
            Dict.values model.reeds2
                |> List.sortWith (f model.sortOrder)
    in
    table [ width fill, spacing 0 ]
        { data = data |> List.indexedMap (\i r -> { row = i, reed = r })
        , columns =
            [ { header = headerCell model (el [] <| text "ID") ReedID
              , width = fill
              , view = \row -> cell model.theme row.row row.reed ReedID <| text <| row.reed.id
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
        , row [ width fill ]
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
    case model.state of
        StateEditReed status ->
            case status.mode of
                EditAll ->
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

                EditBasic ->
                    previewSection [] "" <|
                        if reed.note == "" then
                            none

                        else
                            paragraph [] <|
                                [ text reed.note ]

        _ ->
            none


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
                , label = Input.labelHidden "sdaf"
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
                , viewReedNote model status.reed

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
                [ section "Age" Nothing <| viewAge status.reed
                , section "Rating" Nothing <| viewRating <| status.reed.rating
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
