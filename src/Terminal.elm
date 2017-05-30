module Terminal exposing (updateBuffer, updateBufferDown, Entry(..), Terminal, printDaemonOutput, view)

import Html exposing (text, button, div, br)
import MyCss exposing (..)
import Keyboard exposing (KeyCode)
import Char
import FileSystem exposing (FileSystem, Daemon(..), addFile, unsafeAddFile, files, emptySystem)
import Command exposing (execute, catDaemon, updateDaemons, stringConfig, Config)
import Time exposing (Time)


config : Config String
config =
    stringConfig


main : Program Never Model Msg
main =
    Html.program
        { init = init ! []
        , update = update
        , view = view config
        , subscriptions = subscriptions
        }


init : Model
init =
    let
        f name =
            unsafeAddFile name (String.toUpper name)

        system =
            List.foldl f emptySystem files

        daemons =
            [ { daemon = catDaemon config "/usr/litter" "lo", name = "kitty", lifetime = 100 } ]
    in
        { history = []
        , buffer =
            "spawn 9 dog ( echo hel ! append hello )"
            --"echo hello | write world"
        , system = { system | daemons = daemons }
        }


type alias Model =
    Terminal String


type alias Terminal a =
    { history : List (Entry a)
    , system : FileSystem a
    , buffer : String
    }


type Entry a
    = Input a
    | Output a



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
                ( streams, system, _ ) =
                    updateDaemons 1 model.system model.system.daemons

                history =
                    (printDaemonOutput config streams) ++ model.history
            in
                ({ model | system = system, history = history }) ! []

        KeyPress key ->
            { model | buffer = updateBuffer key model.buffer } ! []

        KeyDown key ->
            case key of
                8 ->
                    update (KeyPress key) model

                13 ->
                    let
                        ( stdout, system ) =
                            execute config model.buffer model.system

                        history =
                            Output stdout :: Input model.buffer :: model.history
                    in
                        { model | history = history, system = system, buffer = "" } ! []

                _ ->
                    { model | buffer = updateBufferDown key model.buffer model.history } ! []


updateBuffer : KeyCode -> String -> String
updateBuffer key buffer =
    case key of
        13 ->
            buffer

        8 ->
            String.dropRight 1 buffer

        _ ->
            key |> Char.fromCode |> String.fromChar |> (++) buffer


updateBufferDown : Int -> String -> List (Entry String) -> String
updateBufferDown key buffer history =
    case key of
        38 ->
            let
                isInput x =
                    case x of
                        Input s ->
                            Just s

                        _ ->
                            Nothing
            in
                history
                    |> List.filterMap isInput
                    |> List.filter (String.startsWith buffer)
                    |> List.head
                    |> Maybe.withDefault buffer

        40 ->
            ""

        _ ->
            buffer



-- VIEW


cursor : String
cursor =
    String.fromChar '█'


prompt : String
prompt =
    "$ "


view : Config a -> Terminal a -> Html.Html msg
view config model =
    let
        history =
            Input (config.fromString (model.buffer ++ cursor))
                :: model.history
                |> List.reverse
                |> List.filter ((/=) (Output config.null))
                |> List.map (printEntry config prompt)
                |> String.join "\n"

        mainStyle =
            backgroundStyle ++ terminalText ++ fullWindow
    in
        div [ styles mainStyle ] [ breakText history ]


printDaemonOutput : Config a -> List a -> List (Entry a)
printDaemonOutput config streams =
    let
        screams =
            streams
                |> List.filter ((/=) config.null)
                |> List.map Output
    in
        case screams of
            [] ->
                []

            xs ->
                Output (config.fromString "--- daemons speak ---") :: xs |> List.reverse


printEntry : Config a -> String -> Entry a -> String
printEntry config prompt entry =
    case entry of
        Input s ->
            prompt ++ (config.toString s)

        Output s ->
            (config.toString s)



-- view : Model -> Html.Html Msg
-- view model =
--     let
--         emptyOutput s =
--             case s of
--                 Output "" ->
--                     True
--
--                 _ ->
--                     False
--
--         history =
--             Input (model.buffer ++ cursor)
--                 :: model.history
--                 |> List.reverse
--                 |> List.filter (not << emptyOutput)
--                 |> List.map (printEntry prompt)
--                 |> String.join "\n"
--
--         mainStyle =
--             backgroundStyle ++ terminalText ++ fullWindow
--     in
--         div [ styles mainStyle ] [ breakText history ]
--
-- printDaemonOutput : List String -> List (Entry String)
-- printDaemonOutput streams =
--     let
--         screams =
--             streams
--                 |> List.filter ((/=) "")
--                 |> List.map Output
--     in
--         case screams of
--             [] ->
--                 []
--
--             xs ->
--                 Output "--- daemons speak ---" :: xs |> List.reverse
--
--
-- printEntry : String -> Entry String -> String
-- printEntry prompt entry =
--     case entry of
--         Input s ->
--             prompt ++ s
--
--         Output s ->
--             s


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
