module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- MAIN

main = Browser.element
         { init = init
         , update = update
         , subscriptions = subscriptions
         , view = view 
         }




-- MODEL
type alias Account = 
  { id   : Int 
  , name : String 
  , age  : Int 
  }

type alias Model =
  { accounts : List Account
  } 


init : () -> (Model, Cmd Msg)
init _ = 
  ( { accounts = 
      [ { id = 1, name = "John", age = 42 }
      , { id = 2, name = "Jake", age = 43 }
      , { id = 3, name = "Jack", age = 44 }
      ]
    }
  , Cmd.none)




-- UPDATE


type Msg
  = NoOp String
  | RemoveRow Int
  | AddRow

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of 
    NoOp str ->
      ( model, Cmd.none )

    RemoveRow id ->
      let
        newAccounts = List.filter ( \account -> not ( checkAccount id account ) ) model.accounts
      in
        ( { model | accounts = newAccounts }, Cmd.none )
    
    AddRow ->
      let
        newAccounts = List.append model.accounts [ ( newAccount model.accounts ) ]

      in
        ( { model | accounts = newAccounts }, Cmd.none )

newAccount : List Account -> Account
newAccount accounts = 
  case maxId accounts of 
    Just nextId ->
      { id = nextId + 1, name = "", age = 0 }

    Nothing ->
      { id = 1, name = "", age = 0 }

maxId : List Account -> Maybe Int
maxId accounts =
  accounts
    |> List.map .id
    |> List.maximum


checkAccount : Int -> Account -> Bool
checkAccount id act = 
  if id == act.id then 
    True
  else 
    False

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg 
subscriptions model
  = Sub.none 




-- VIEW 


view : Model -> Html Msg 
view model = 
  div [] 
  [ text "Accounts"
  , viewRows model.accounts 
  , button [ onClick AddRow ] [ text "-" ]
  ]
  
viewRows : List Account -> Html Msg
viewRows acts = 
  div [] ( List.map viewAccount acts )


viewAccount : Account -> Html Msg 
viewAccount act = 
  div [] 
  [ viewInput "text" "ID" (String.fromInt act.id) NoOp
  , viewInput "text" "Name" act.name NoOp
  , viewInput "text" "Age" (String.fromInt act.age) NoOp
  , button [ onClick ( RemoveRow act.id ) ] [ text "+" ]
  ]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput typ plcholder val evt = 
  label []
  [ text plcholder
  , input [ type_ typ, placeholder plcholder, value val, onInput evt ] []
  ]
  