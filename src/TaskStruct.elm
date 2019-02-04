module TaskStruct exposing (
  Task(..)
  , createTask
  , mapTask, filterTask
  , addTask, removeTask
  , findAndUpdateTaskName, findAndUpdateTaskDesk
  )
import Maybe.Extra exposing (values)

type Status
  = Planned
  | Complete
  | Error

{-
type alias Branch =
  { name : String
  , text : String
  , children : List Task
  }
-}

type Task =
    Task
    { name : String
    , status: Status
    , desk: String
    , pos : Int
    , lvl : Int
    -- , branches : Dict String, Branch
    , children : List Task
    }


mapTask : (Task -> Task) -> Task -> Task 
mapTask f t =
  let
    (Task task) = f t
  in
    Task { task | children = List.map (mapTask f) task.children }


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
               [ createTaskPosLvlName (List.length task.children) (lvl + 1) "new task" ]
      }
    else
      Task task


addTask : Int -> Int -> Task -> Task
addTask pos lvl t = mapTask (addTaskByPosAndLvl pos lvl) t

removeTask : Int -> Int -> Task -> Maybe Task
removeTask pos lvl task = filterTask
  ( \(Task t) -> not (t.pos == pos && t.lvl == lvl) )
  task

createTaskPosLvlName : Int -> Int -> String -> Task
createTaskPosLvlName pos lvl name = Task
  { name = name
  , status = Planned
  , desk = ""
  , pos = pos
  , lvl = lvl
  -- , branches = empty
  , children = []
  }

createTask : String -> Task
createTask = createTaskPosLvlName 0 0


findAndUpdateTaskName : Task -> Task -> String -> Task
findAndUpdateTaskName treeTask (Task oneTask) taskName =
  mapTask (\(Task t) -> if t.lvl == oneTask.lvl && t.pos == oneTask.pos
      then
        Task { t | name = taskName }
      else
        (Task t)
  ) treeTask

findAndUpdateTaskDesk : Task -> Task -> String -> Task
findAndUpdateTaskDesk treeTask (Task oneTask) desk =
  mapTask (\(Task t) -> if t.lvl == oneTask.lvl && t.pos == oneTask.pos
      then
        Task { t | desk = desk }
      else
        (Task t)
  ) treeTask