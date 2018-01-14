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

type alias Todo =
  { text : String
  , identifier : Int
  , completed : Bool
  }

type alias Model =
  { todo : String
  , todos : List Todo
  , globalId : Int
  }

model : Model
model =
  { todo = ""
  , todos = []
  , globalId = 0
  }

-- UPDATE

type Msg
  = UpdateTodo String
  | AddTodo
  | RemoveAll
  | RemoveItem Int
  | ToggleTodo Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateTodo text ->
      { model | todo = text }
    
    AddTodo ->
      if String.length model.todo > 0 then
        let
          newTodo : Todo
          newTodo = 
          { text = model.todo
          , identifier = model.globalId
          , completed = False
          }
        in
          { model
          | todos = newTodo :: model.todos
          , todo = ""
          , globalId = model.globalId + 1
          }
      else
        model

    RemoveAll ->
      { model | todos = [] }

    RemoveItem identifier ->
      { model | todos = List.filter (\x -> x.identifier /= identifier) model.todos }

    ToggleTodo identifier ->
      let
        updateTodo t =
          if t.identifier == identifier then
            { t | completed = not t.completed }
          else
            t
      in  
        { model | todos = List.map updateTodo model.todos }
          
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

todoItem : Todo -> Html Msg
todoItem todo =
  li
    [ class "list__item"
    , onClick (ToggleTodo todo.identifier)
    ]
    [ text todo.text
    , button 
      [ onClick (RemoveItem todo.identifier)
      , class "list__remove-item-btn"
      ]
      [ text "×" ]
    ]

todoList : List Todo -> Html Msg
todoList todos =
  let
    child = 
      List.map todoItem todos
  in
    ul [ class "list" ] child

getActiveTodos : List Todo -> List Todo
getActiveTodos todos =
  List.filter (\t -> t.completed == False) todos

getCompletedTodos : List Todo -> List Todo
getCompletedTodos todos =
  List.filter (\t -> t.completed == True) todos

view model = 
  div
  [ class "container"
  ]
  [ stylesheet
  , h1
      [ class "app-name" ]
      [ text "Elm Todo App" ]
  , div
      [ class "add-controls" ]
      [ input
          [ type_ "text"
          , placeholder "Type here..."
          , class "input add-controls__input"
          , onInput UpdateTodo
          , value model.todo
          ]
          []
      , button [ onClick AddTodo, class "button add-controls__button" ] [ text "Add" ]
      , button [ onClick RemoveAll, class "button add-controls__button" ] [ text "Remove All" ]  
      ]
  , div 
      [ class "todos todos_active" ]
      [ h4 [class "todos__heading"] [text "To be done:"]
      , todoList (getActiveTodos model.todos)
      ]
  , div 
      [ class "todos todos_done" ]
      [ h4 [class "todos__heading"] [text "Already done:"]
      , todoList (getCompletedTodos model.todos)
      ]
  ]