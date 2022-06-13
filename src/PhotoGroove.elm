module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, img, text, button)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type alias Photo = { url : String } 
type alias Model = 
    { photos: List Photo
    , selectedUrl:String
    , chooseSize : ThumbnailSize 
    }
type alias Msg = { description : String, data : String }

type ThumbnailSize 
  = Small
  | Medium
  | Large 

urlPrefix : String  
urlPrefix = 
  "http://elm-in-action.com/"

photoListUrl : String 
photoListUrl = 
  "http://elm-in-action.com/list-photos"

view : Model -> Html Msg 
view model = 
  div
    [ class "content" ]
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick { description = "ClickedSurpriseMe", data = "" } ]
            [ text "Surprise Me!" ]
    , div [ id "thumbnails" ] 
          (List.map
                (viewThumbnail model.selectedUrl)
                model.photos
          )
    , img 
        [ class "large"
        , src (urlPrefix ++ "large/" ++ model.selectedUrl)
        ]
        [
        ]
    ]

viewThumbnail : String -> Photo -> Html Msg 
viewThumbnail selectedUrl thumb = 
    img 
      [ src (urlPrefix ++ thumb.url) 
      , classList [("selected" , selectedUrl == thumb.url) ] 
      , onClick { description = "ClickedPhoto", data = thumb.url }
      ] 
      [
      ] 

initModel : Model  
initModel = 
  { photos = [ { url = "1.jpeg" }
        , { url = "2.jpeg" } 
        , { url = "3.jpeg" }
        ] 
  , selectedUrl = "1.jpeg"
  , chooseSize = Medium
  }

photoArray : Array Photo  
photoArray = 
  Array.fromList initModel.photos

update : Msg -> Model -> Model
update msg model = 
    case msg.description of 
      "ClickedPhoto" -> 
        { model | selectedUrl = msg.data } 
      "ClickedSurpriseMe" -> 
        { model | selectedUrl = "2.jpeg" }
      _ ->  
        model 
main = 
  Browser.sandbox 
    { init = initModel 
    , view = view
    , update = update 
    }
