import Browser
import Html exposing (Html, text, div, button, ul, li, span)
import Html.Events exposing (onClick)
import Debug



main = Browser.sandbox
  { init = init
  , update = update
  , view = view }


type Model
  = EmptyTask
  | Task
    { name : String
    , pos : Int
    , lvl : Int
    , children : List Model
    }


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


mapTask : (Model -> Model) -> Model -> Model
mapTask f t = case t of
  EmptyTask -> EmptyTask
  _ -> case f t of
      EmptyTask -> EmptyTask
      (Task task) -> Task { task | children = List.map (mapTask f) task.children }


taskIsNotEmpty : Model -> Bool
taskIsNotEmpty t = case t of
  EmptyTask -> False
  _ -> True


filterTask : (Model -> Bool) -> Model -> Model
filterTask f t = case Debug.log "filterTask" t of
  EmptyTask -> EmptyTask
  (Task task) ->
    if f (Task task) then
      Task { task | children = List.filter (\tch -> taskIsNotEmpty ( filterTask f tch )) task.children }
    else
      EmptyTask


addTaskByPosAndLvl : Int -> Int -> Model -> Model
addTaskByPosAndLvl pos lvl t = case t of
    EmptyTask -> EmptyTask
    (Task task) -> if task.pos == pos && task.lvl == lvl
      then
        Task { task |
               children = List.append
                 task.children
                 [ Task { name = "new task", pos = (List.length task.children), lvl = lvl + 1, children = [] } ]
        }
      else
        Task task


removeTaskByPosAndLvl : Int -> Int -> Model -> Bool
removeTaskByPosAndLvl pos lvl t = case t of
    EmptyTask -> False
    (Task task) -> if task.pos == pos && task.lvl == lvl
      then
        False
      else
        True

addTask : Int -> Int -> Model -> Model
addTask pos lvl t = mapTask (addTaskByPosAndLvl pos lvl) t

removeTask : Int -> Int -> Model -> Model
removeTask pos lvl t = filterTask (removeTaskByPosAndLvl pos lvl) t

update : Msg -> Model -> Model
update msg model =
  case msg of
    Add pos lvl -> addTask pos lvl model
    Del pos lvl -> removeTask pos lvl model


view : Model -> Html Msg
view t = case t of
  EmptyTask -> text ""
  (Task task) -> ul []
    [ li []
      [ div [] [text task.name]
      , div [] [ span [] [text (String.fromInt task.lvl)], span [] [text (String.fromInt task.pos)] ]
      , div [onClick (Add task.pos task.lvl)] [text "add"]
      , div [onClick (Del task.pos task.lvl)] [text "del"]
      ]
    , li [] (List.map view task.children)
    ]