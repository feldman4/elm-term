module Terminal exposing (..)

import Html exposing (text, button, div, br)
import MyCss exposing (..)
import Keyboard exposing (KeyCode)
import Char
import FileSystem exposing (FileSystem, Daemon(..), addFile, unsafeAddFile, files, emptySystem)
import Command exposing (execute, catDaemon, updateDaemons)
import Time exposing (Time)


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : Model
init =
    let
        f name =
            unsafeAddFile name (String.toUpper name)

        system =
            List.foldl f emptySystem files
                |> (\x -> { x | daemons = [ catDaemon 99 "hello" ] })
    in
        { history = [], buffer = "echo hello | write world", system = system }


type alias Model =
    { history : List Entry
    , system : FileSystem String
    , buffer : String
    }


type alias Entry =
    { input : String, output : String }



-- UPDATE


type Msg
    = KeyPress KeyCode
    | KeyDown KeyCode
    | Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            let
                ( streams, system_, daemons_ ) =
                    updateDaemons 1 model.system model.system.daemons

                output =
                    streams
                        |> List.filter ((/=) "")
                        |> String.join "\n"

                entry =
                    { input = "daemons speak", output = output }

                addHistory model =
                    if output == "" then
                        model
                    else
                        { model | history = entry :: model.history }
            in
                if List.isEmpty model.system.daemons then
                    model ! []
                else
                    ({ model
                        | system = system_
                     }
                        |> addHistory
                    )
                        ! []

        KeyPress key ->
            model.buffer
                |> updateBuffer key
                |> (\x -> { model | buffer = x } ! [])

        KeyDown key ->
            case key of
                8 ->
                    update (KeyPress key) model

                13 ->
                    model.buffer
                        |> (\x -> execute x model.system)
                        |> (\( stdout, sys ) ->
                                { model
                                    | history = { input = model.buffer, output = stdout } :: model.history
                                    , system = sys
                                    , buffer = ""
                                }
                                    ! []
                           )

                _ ->
                    model ! []


updateBuffer : KeyCode -> String -> String
updateBuffer key buffer =
    case key of
        13 ->
            buffer

        8 ->
            String.dropRight 1 buffer

        _ ->
            key |> Char.fromCode |> String.fromChar |> (++) buffer



-- VIEW


cursor : String
cursor =
    String.fromChar '█'


view : Model -> Html.Html Msg
view model =
    let
        history =
            { input = model.buffer ++ cursor, output = "" }
                :: model.history
                |> List.reverse
                |> List.map (printEntry "$ ")
                |> Debug.log "entries"
                |> String.join "\n"

        mainStyle =
            backgroundStyle ++ terminalText ++ fullWindow
    in
        div [ styles mainStyle ] [ breakText history ]


printEntry : String -> Entry -> String
printEntry prompt { input, output } =
    if output == "" then
        prompt ++ input
    else
        [ prompt ++ input, output ] |> String.join "\n"


breakText : String -> Html.Html msg
breakText string =
    string
        |> String.split "\n"
        |> List.map text
        |> List.intersperse (br [] [])
        |> (\x -> div [] x)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [ Keyboard.downs KeyDown
    , Keyboard.presses KeyPress
    , Time.every Time.second Tick
    ]
        |> Sub.batch
