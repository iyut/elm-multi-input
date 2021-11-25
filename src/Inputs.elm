module Inputs exposing (rowInput, Model)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MAIN

rowInput = Browser.element
         { init = init
         , update = update
         , subscriptions = subscriptions
         , view = view 
         }




-- MODEL


type alias Model =
  { id : Int 
  , name : String 
  , age : Int 
  }


init : () -> (Model, Cmd Msg)
init _ = 
  (Model 0 "" 0, Cmd.none)




-- UPDATE


type Msg
  = ID String
  | Name String 
  | Age String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    ID id ->
      case String.toInt id of 
        Just intId ->
          ({ model | id = intId }, Cmd.none)
        
        Nothing ->
          (model, Cmd.none)
    
    Name name ->
      ({ model | name = name }, Cmd.none)
    
    Age age ->
      case String.toInt age of 
        Just intAge ->
          ({ model | age = intAge }, Cmd.none)
        
        Nothing ->
          (model, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg 
subscriptions model
  = Sub.none 




-- VIEW 


view : Model -> Html Msg 
view model = 
  div [] 
  [ viewInput "text" "ID" (String.fromInt model.id) ID
  , viewInput "text" "Name" model.name Name
  , viewInput "text" "Age" (String.fromInt model.age) Age
  ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput typ plcholder val evt = 
  label []
  [ text plcholder
  , input [ type_ typ, placeholder plcholder, value val, onInput evt ] []
  ]
  