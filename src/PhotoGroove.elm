port module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as JD exposing (Decoder, at, field, int, map3, string, succeed)
import Json.Decode.Pipeline as JDP exposing (optional)
import Json.Encode as JE
import Random


port setFilters : FilterOptions -> Cmd msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Int }
    }


type alias Photo =
    { url : String, size : Int, title : String }


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    at [ "detail", "userSlidTo" ] int
        |> JD.map toMsg
        |> on "slide"


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> JDP.required "url" string
        |> JDP.required "size" int
        |> optional "title" string "(untitle)"


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chooseSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidNoise Int
    | SlidRipple Int


type ThumbnailSize
    = Small
    | Medium
    | Large


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


view : Model -> Html Msg
view model =
    div
        [ class "content" ]
    <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (JE.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chooseSize) ]
        (List.map
            (viewThumbnail selectedUrl)
            photos
        )
    , canvas [ id "main-canvas", class "large" ] []

    {- img
       [ class "large"
       , src (urlPrefix ++ "large/" ++ selectedUrl)
       ]
       []
    -}
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


initModel : Model
initModel =
    { status = Loading
    , chooseSize = Medium
    , hue = 5
    , ripple = 0
    , noise = 0
    }


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        --thought
        Errored errorMessage ->
            status


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = model.hue }
                    , { name = "ripple", amount = model.ripple }
                    , { name = "noise", amount = model.noise }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, Cmd.none )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto selectedUrl ->
            applyFilters { model | status = selectUrl selectedUrl model.status }

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | chooseSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    ( { model | status = Loaded photos first.url }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err httperror) ->
            ( { model | status = Errored "server error" }, Cmd.none )

        SlidHue hue ->
            ( { model | hue = hue }, Cmd.none )

        SlidNoise noise ->
            ( { model | noise = noise }, Cmd.none )

        SlidRipple ripple ->
            ( { model | ripple = ripple }, Cmd.none )


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (JD.list photoDecoder)
        }


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
