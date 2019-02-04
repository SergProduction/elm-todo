import Browser
import Html exposing (Html, node, text, div, button, ul, li, span)
import Html.Events exposing (onClick)
import Html.Attributes exposing (rel, href, class)
import Debug
import TaskStruct as T exposing (Task(..), mapTask, filterTask, addTask, removeTask)



main = Browser.sandbox
  { init = init
  , update = update
  , view = view }


type alias Model = Task


init : Model
init =
  Task
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
view model =
  div [ class "main" ]
    [ node "link" [ rel "stylesheet", href "main.css" ] []
    , div [class "task-tree"] [recRenderTask model]
    ]


recRenderTask : Task -> Html Msg
recRenderTask (Task task) =
  ul []
    [ li [class "name" ]
      [ div [class "del", onClick (Del task.pos task.lvl)] [text "del"]
      , div [class "add", onClick (Add task.pos task.lvl)] [text "add"]
      , div [class "id"]
        [ span [] [text (String.fromInt task.lvl ++ ".")]
        , span [] [text (String.fromInt task.pos)]
        ]
      , div [] [text task.name]
      ]
    , li [] (List.map recRenderTask task.children)
    ]
