import Browser
import Html exposing (Html, node, text, div, button, ul, li, span, input, textarea)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (rel, href, class, type_, value, placeholder)
import Debug
import TaskStruct as T exposing (
  Task(..)
  , createTask
  , mapTask, filterTask
  , addTask, removeTask
  , findAndUpdateTaskName, findAndUpdateTaskDesk
  )



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
  { taskTree = createTask "begin"
  , taskEditable = Nothing
  }

type TreeTask
  = Add Int Int
  | Del Int Int

type EditTask
  = Edit Task
  | Name String
  | Desk String


type Msg
  = TreeTask TreeTask
  | EditTask EditTask

update : Msg -> Model -> Model
update msg model =
  case msg of
    TreeTask (Add pos lvl) -> { model | taskTree = addTask pos lvl model.taskTree }
    TreeTask (Del pos lvl) -> case removeTask pos lvl model.taskTree of
      Nothing -> { model | taskTree =  createTask "main task" }
      Just t  -> { model | taskTree = t }
    EditTask (Edit t) -> {model | taskEditable = Just t }
    EditTask (Name taskName) -> case model.taskEditable of
      Nothing       -> { model | taskEditable = Just <| createTask taskName } -- такого не может быть. но почему-то мне нужно обработать этот случай
      Just (Task t) ->
        { model
        | taskEditable = Just <| Task { t | name = taskName }
        , taskTree = findAndUpdateTaskName model.taskTree (Task t) taskName
        }
    EditTask (Desk desk) -> case model.taskEditable of
      Nothing       -> { model | taskEditable = Just <| createTask "" } -- такого не может быть. но почему-то мне нужно обработать этот случай
      Just (Task t) ->
        { model
        | taskEditable = Just <| Task { t | desk = desk }
        , taskTree = findAndUpdateTaskDesk model.taskTree (Task t) desk
        }



view : Model -> Html Msg
view model =
  div [ class "main" ]
    [ node "link" [ rel "stylesheet", href "main.css" ] []
    , div [class "task-tree"] [recViewTaskTree model.taskTree]
    , div [] [viewTaskEditable model.taskEditable]
    ]


recViewTaskTree : Task -> Html Msg
recViewTaskTree (Task task) =
  ul []
    [ li [class "name" ]
      [ div [class "del", onClick (TreeTask <| Del task.pos task.lvl)] [text "del"]
      , div [class "add", onClick (TreeTask <| Add task.pos task.lvl)] [text "add"]
      , div [class "id"]
        [ span [] [text (String.fromInt task.lvl ++ ".")]
        , span [] [text (String.fromInt task.pos)]
        ]
      , div [ onClick <| EditTask <| Edit (Task task) ] [text task.name]
      ]
    , li [] (List.map recViewTaskTree task.children)
    ]


viewTaskEditable : Maybe Task -> Html Msg
viewTaskEditable t = case t of
  Nothing -> text ""
  Just (Task task) -> div [class "editable"]
    [ input [type_ "text", value task.name, onInput (\n -> EditTask (Name n))] []
    , textarea [onInput (\n -> EditTask (Desk n)), value task.desk] [text task.desk]
    ]
