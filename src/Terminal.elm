module Terminal exposing (..)

import Html exposing (text, button, div, br)
import MyCss exposing (..)
import String.Extra
import Keyboard exposing (KeyCode)
import Char
import FileSystem exposing (FlatSystem, addFile, unsafeAddFile, files, emptySystem)
import Command exposing (execute)


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
    in
        { history = "write /hello", system = system }


type alias Model =
    { history : String
    , system : FlatSystem String
    }



-- UPDATE


type Msg
    = KeyPress KeyCode
    | KeyDown KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPress key ->
            model.history
                |> updateHistory key
                |> (\x -> { model | history = x })
                |> (\x -> x ! [])

        KeyDown key ->
            case key of
                8 ->
                    update (KeyPress key) model

                13 ->
                    model.history
                        |> String.lines
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault model.history
                        |> (\x -> execute x model.system)
                        |> (\( txt, sys ) ->
                                { model
                                    | history = model.history ++ "\n" ++ txt
                                    , system = sys
                                }
                                    ! []
                           )

                _ ->
                    model ! []


updateHistory : KeyCode -> String -> String
updateHistory key history =
    case key of
        13 ->
            history ++ "\n"

        8 ->
            if String.endsWith "\n" history then
                history
            else
                String.dropRight 1 history

        _ ->
            key |> Char.fromCode |> String.fromChar |> (++) history



-- VIEW


view : Model -> Html.Html Msg
view model =
    let
        history =
            model.history
                |> printHistory ">"
                |> addCursor

        mainStyle =
            backgroundStyle ++ terminalText ++ fullWindow
    in
        div [ styles mainStyle ] [ breakText history ]


printHistory : String -> String -> String
printHistory prompt history =
    history
        |> String.Extra.replace "\n" ("\n" ++ prompt)
        |> (++) prompt


addCursor : String -> String
addCursor string =
    string ++ (String.fromChar '█')


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
    [ Keyboard.downs KeyDown, Keyboard.presses KeyPress ] |> Sub.batch
