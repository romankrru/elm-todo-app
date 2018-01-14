import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

main =
  Html.beginnerProgram
    { model = model
    , update = update
    , view = view
    }

-- MODEL

type alias Model =
  { todo : String
  , todos : List String
  }

model : Model
model =
  { todo = ""
  , todos = []
  }

-- UPDATE
type Msg
  = UpdateTodo String
  | AddTodo
  | RemoveAll
  | RemoveItem String

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTodo text ->
      { model | todo = text }
    
    AddTodo ->
      { model
      | todos = model.todo :: model.todos
      , todo = ""
      }

    RemoveAll ->
      { model | todos = [] }

    RemoveItem text ->
      { model | todos = List.filter (\x -> x /= text) model.todos }

-- VIEW

stylesheet =
  let
    tag =
      "link"

    attrs =
      [ attribute "Rel" "stylesheet"
      , attribute "property" "stylesheet"
      , attribute "href" "./style.css"
      ]

    children = []

  in
    node tag attrs children

todoItem : String -> Html Msg
todoItem todo =
  li []
    [ text todo
    , button [ onClick (RemoveItem todo) ] [ text "x" ]
    ]

todoList : List String -> Html Msg
todoList todos =
  let
    child = 
      List.map todoItem todos
  in
    ul [] child

view model = 
  div []
  [ stylesheet
  , input 
      [ type_ "text" 
      , onInput UpdateTodo
      , value model.todo
      ]
      []
  , button [ onClick AddTodo ] [ text "Submit" ]
  , button [ onClick RemoveAll ] [ text "Remove All" ]
  , div [] [ todoList model.todos ]
  ]