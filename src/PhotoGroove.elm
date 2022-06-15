module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD exposing (Decoder, field, int, map3, string, succeed)
import Json.Decode.Pipeline as JDP exposing (optional)
import Json.Encode as JE
import Random


type alias Photo =
    { url : String, size : Int, title : String }


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


photoDecoder : Decoder Photo
photoDecoder =
    succeed Photo
        |> JDP.required "url" string
        |> JDP.required "size" int
        |> optional "title" string "(untitle)"



{-
   photoDecoder : Decoder Photo
   photoDecoder =
       map3 (\url size title -> { url = url, size = size, title = title })
           (field "url" string)
           (field "size" int)
           (field "title" string)
-}


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chooseSize : ThumbnailSize
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))


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
                viewLoaded photos selectedUrl model.chooseSize

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]


viewFilter : String -> Int -> Html Msg
viewFilter name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (JE.int magnitude)
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "filters" ]
        [ viewFilter "Hue" 0, viewFilter "Ripple" 0, viewFilter "Noise" 0 ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map
            (viewThumbnail selectedUrl)
            photos
        )
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

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
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                first :: rest ->
                    ( { model | status = Loaded photos first.url }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err httperror) ->
            ( { model | status = Errored "server error" }, Cmd.none )


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
