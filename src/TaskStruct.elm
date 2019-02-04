module TaskStruct exposing (Task(..), mapTask, filterTask, addTask, removeTask)
import Maybe.Extra exposing (values)

type Task =
    Task
    { name : String
    , pos : Int
    , lvl : Int
    , children : List Task
    }


mapTask : (Task -> Task) -> Task -> Task 
mapTask f t =
  let
    (Task task) = f t
  in
    Task { task | children = List.map (mapTask f) task.children }


isJust : Maybe a -> Bool
isJust x =
  case x of
    Nothing -> False
    _       -> True

filterTask : (Task -> Bool) -> Task -> Maybe Task 
filterTask f (Task t) =
  if f (Task t) then
    Just ( Task { t | children = t.children |> List.map (filterTask f) |> values } )
  else
    Nothing


addTaskByPosAndLvl : Int -> Int -> Task -> Task
addTaskByPosAndLvl pos lvl (Task task) =
  if task.pos == pos && task.lvl == lvl
    then
      Task { task |
             children = List.append
               task.children
               [ Task { name = "new task", pos = (List.length task.children), lvl = lvl + 1, children = [] } ]
      }
    else
      Task task


addTask : Int -> Int -> Task -> Task
addTask pos lvl t = mapTask (addTaskByPosAndLvl pos lvl) t

removeTask : Int -> Int -> Task -> Maybe Task
removeTask pos lvl task = filterTask
  ( \(Task t) -> not (t.pos == pos && t.lvl == lvl) )
  task
