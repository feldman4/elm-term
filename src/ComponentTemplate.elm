module ComponentTemplate exposing (Model, Msg(..), init, update, view)

import Html exposing (text, div, program)
import Html.Attributes exposing (id, class)


main : Program Never Model Msg
main =
    Html.program
        { init = init ""
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { field : String
    }


init : String -> ( Model, Cmd Msg )
init initialField =
    { field = initialField
    }
        ! []



-- UPDATE


type Msg
    = Response String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Response newField ->
            { model
                | field = newField
            }
                ! []

        NoOp ->
            model ! []



-- VIEW


view : Model -> Html.Html Msg
view model =
    div [ id "field", class "field" ] [ text model.field ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [] |> Sub.batch



--     [ AnimationFrame.diffs Animate
--     , Keyboard.downs (keyChange True)
--     , Keyboard.ups (keyChange False)
--     , Window.resizes Resize
--     , Drag.subscriptions DragMsg model.dragModel
--     , Sub.map SpeechMsg (Speech.subscriptions model.speechModel)
--     ]
--         |> Sub.batch
-- PORTS
-- port check : String -> Cmd msg
-- port suggestions : (List String -> msg) -> Sub msg
