port module Main exposing (EditTask(..), Model, Msg(..), TreeTask(..), init, main, recViewTaskTree, subscriptions, update, view, viewTaskEditable)

import Browser
import Debug
import Html exposing (Html, button, div, input, li, node, span, text, textarea, ul)
import Html.Attributes exposing (class, href, placeholder, rel, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import TaskStruct as T
    exposing
        ( Task(..)
        , addTask
        , createTask
        , filterTask
        , findAndUpdateTaskDesk
        , findAndUpdateTaskName
        , mapTask
        , removeTask
        )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { taskTree : Task
    , taskEditable : Maybe Task
    }


port cache : E.Value -> Cmd msg


init : String -> ( Model, Cmd Msg )
init t =
    let
        f =
            case D.decodeString T.decode t of
                Ok v ->
                    v

                Err e ->
                    createTask "begin"
    in
    ( { taskTree = f
      , taskEditable = Nothing
      }
    , Cmd.none
    )


type TreeTask
    = Add Int Int
    | Del Int Int


type EditTask
    = Edit Task
    | Name String
    | Desk String
    | Save


type Msg
    = TreeTask TreeTask
    | EditTask EditTask


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TreeTask (Add pos lvl) ->
            ( { model | taskTree = addTask pos lvl model.taskTree }, Cmd.none )

        TreeTask (Del pos lvl) ->
            case removeTask pos lvl model.taskTree of
                Nothing ->
                    ( { model | taskTree = createTask "main task" }, Cmd.none )

                Just t ->
                    ( { model | taskTree = t }, Cmd.none )

        EditTask (Edit t) ->
            ( { model | taskEditable = Just t }, Cmd.none )

        EditTask (Name taskName) ->
            case model.taskEditable of
                Nothing ->
                    ( { model | taskEditable = Just <| createTask taskName }, Cmd.none )

                -- такого не может быть. но почему-то мне нужно обработать этот случай
                Just (Task t) ->
                    ( { model
                        | taskEditable = Just <| Task { t | name = taskName }
                        , taskTree = findAndUpdateTaskName model.taskTree (Task t) taskName
                      }
                    , Cmd.none
                    )

        EditTask (Desk desk) ->
            case model.taskEditable of
                Nothing ->
                    ( { model | taskEditable = Just <| createTask "" }, Cmd.none )

                -- такого не может быть. но почему-то мне нужно обработать этот случай
                Just (Task t) ->
                    ( { model
                        | taskEditable = Just <| Task { t | desk = desk }
                        , taskTree = findAndUpdateTaskDesk model.taskTree (Task t) desk
                      }
                    , Cmd.none
                    )

        EditTask Save ->
            ( model
            , cache (T.encode model.taskTree)
            )


view : Model -> Html Msg
view model =
    div [ class "main" ]
        [ div [ class "task-tree" ] [ recViewTaskTree model.taskTree ]
        , div [] [ viewTaskEditable model.taskEditable ]
        ]


recViewTaskTree : Task -> Html Msg
recViewTaskTree (Task task) =
    ul []
        [ li [ class "name" ]
            [ div [ class "del", onClick (TreeTask <| Del task.pos task.lvl) ] [ text "del" ]
            , div [ class "add", onClick (TreeTask <| Add task.pos task.lvl) ] [ text "add" ]
            , div [ class "id" ]
                [ span [] [ text (String.fromInt task.lvl ++ ".") ]
                , span [] [ text (String.fromInt task.pos) ]
                ]
            , div [ onClick <| EditTask <| Edit (Task task) ] [ text task.name ]
            ]
        , li [] (List.map recViewTaskTree task.children)
        ]


viewTaskEditable : Maybe Task -> Html Msg
viewTaskEditable t =
    case t of
        Nothing ->
            text ""

        Just (Task task) ->
            div [ class "editable" ]
                [ button [ onClick (EditTask Save) ] [ text "save" ]
                , input [ type_ "text", value task.name, onInput (\n -> EditTask (Name n)) ] []
                , textarea [ onInput (\n -> EditTask (Desk n)), value task.desk ] [ text task.desk ]
                ]
