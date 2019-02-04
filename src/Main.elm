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


type alias Model =
  { taskTree: Task
  , taskEditable: Maybe Task
  }


init : Model
init =
  { taskTree = Task
    { name = "begin"
    , pos = 0
    , lvl = 0
    , children = []
    }
  , taskEditable = Nothing
  }


type Msg
  = Add Int Int
  | Del Int Int
  | Edit Task


update : Msg -> Model -> Model
update msg model =
  case msg of
    Add pos lvl -> { model | taskTree = addTask pos lvl model.taskTree }
    Del pos lvl -> case removeTask pos lvl model.taskTree of
      Nothing -> { model | taskTree = Task { name = "main task", pos = 0, lvl = 0, children = [] } }
      Just t -> { model | taskTree = t }
    Edit t -> {model | taskEditable = Just t }


view : Model -> Html Msg
view model =
  div [ class "main" ]
    [ node "link" [ rel "stylesheet", href "main.css" ] []
    , div [class "task-tree"] [recRenderTask model.taskTree]
    , div [] [renderTaskEditable model.taskEditable]
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
      , div [ onClick <| Edit (Task task) ] [text task.name]
      ]
    , li [] (List.map recRenderTask task.children)
    ]


renderTaskEditable : Maybe Task -> Html Msg
renderTaskEditable t = case t of
  Nothing -> text ""
  _ -> span [] [text "TaskEditable"]