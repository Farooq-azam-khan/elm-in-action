module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

type alias Photo = { url : String } 
type alias Model = {photos: List Photo, selectedUrl:String} 
type alias Msg = { description : String, data : String }

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

viewThumbnail selectedUrl thumb = 
    img 
      [ src (urlPrefix ++ thumb.url) 
      , classList [("selected" , selectedUrl == thumb.url) ] 
      , onClick { description = "ClickedPhoto", data = thumb.url }
      ] 
      [
      ] 

initModel : {photos: List {url : String}, selectedUrl:String}
initModel = 
  { photos = [ { url = "1.jpeg" }
        , { url = "2.jpeg" } 
        , { url = "3.jpeg" }
        ] 
  , selectedUrl = "1.jpeg"
  }

photoArray : Array Photo  
photoArray = 
  Array.fromList initModel.photos

update : Msg -> Model -> Model
update msg model = 
    if msg.description == "ClickedPhoto" then 
        { model | selectedUrl = msg.data } 
    else 
        model 
main = 
  Browser.sandbox 
    { init = initModel 
    , view = view
    , update = update 
    }
