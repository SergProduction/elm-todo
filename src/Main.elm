import Browser
import Html exposing (Html, text, div, button, ul, li, span)
import Html.Events exposing (onClick)
import Debug
import TaskStruct as T exposing (Task(..), mapTask, filterTask, addTask, removeTask)




main = Browser.sandbox
  { init = init
  , update = update
  , view = view }


type alias Model = Task


init : Model
init = Task
  { name = "begin"
  , pos = 0
  , lvl = 0
  , children = []
  }


type Msg
  = Add Int Int
  | Del Int Int



update : Msg -> Model -> Model
update msg model =
  case msg of
    Add pos lvl -> addTask pos lvl model
    Del pos lvl -> case removeTask pos lvl model of
      Nothing -> Task { name = "main task", pos = 0, lvl = 0, children = [] }
      Just t -> t


view : Model -> Html Msg
view (Task task) =
  ul []
    [ li []
      [ div [] [text task.name]
      , div [] [ span [] [text (String.fromInt task.lvl)], span [] [text (String.fromInt task.pos)] ]
      , div [onClick (Add task.pos task.lvl)] [text "add"]
      , div [onClick (Del task.pos task.lvl)] [text "del"]
      ]
    , li [] (List.map view task.children)
    ]