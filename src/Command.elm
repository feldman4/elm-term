module Command exposing (..)

import FileSystem exposing (..)
import Html exposing (text)
import List.Extra
import Regex exposing (Regex)
import Maybe.Extra


main : Html.Html msg
main =
    text ""


type alias Stream =
    String


type alias Error =
    String


type alias PureCommand a =
    a -> a


type alias IOCommand a =
    a -> FlatSystem a -> Result Error ( a, FlatSystem a )


{-| Outputs strings. Can be IOCommand a if a function to lift to Stream type
is provided.
-}
ls : Name -> IOCommand String
ls name _ system =
    getContents (makeAbsolute name system) 1 system
        |> Result.map (String.join " ")
        |> Result.map (\a -> ( a, system ))


lsCwd : IOCommand String
lsCwd stream system =
    ls system.cwd stream system


cd : Name -> IOCommand a
cd name stream system =
    let
        name_ =
            makeAbsolute (name ++ "/") system |> Debug.log "name_"

        system_ =
            { system | cwd = name_ } |> validate
    in
        if member name_ system then
            Result.Ok ( stream, system_ )
        else
            Result.Err (name ++ ": No such directory")


cdUp : IOCommand a
cdUp stream system =
    cd (getParent system.cwd) stream system


pwd : IOCommand String
pwd _ system =
    Result.Ok ( system.cwd, system )


echo : String -> IOCommand String
echo x _ system =
    Result.Ok ( x, system )


{-| Ignores stdin.
-}
cat : Name -> IOCommand a
cat name _ system =
    get (makeAbsolute name system) system
        |> Result.map (\a -> ( a, system ))


{-| Can be IOCommand a if a null stream is provided.
-}
write : Name -> IOCommand String
write name stream system =
    addFile (makeAbsolute name system) stream system
        |> Result.map (\x -> ( "", x ))


matchCommandArity0 : String -> IOCommand a -> String -> Maybe (IOCommand a)
matchCommandArity0 pattern command input =
    case Regex.find (Regex.AtMost 1) (Regex.regex pattern) input of
        match :: _ ->
            Just command

        _ ->
            Nothing


matchCommandArity1 : String -> (String -> IOCommand a) -> String -> Maybe (IOCommand a)
matchCommandArity1 pattern command input =
    case Regex.find (Regex.AtMost 1) (Regex.regex pattern) input of
        { submatches } :: _ ->
            case submatches of
                (Just a) :: [] ->
                    Just (command a)

                _ ->
                    Nothing

        _ ->
            Nothing


{-|
- split at |
- simple matcher: String -> Maybe Command
- register commands as simple matchers
- including ls => a becomes String
-}
parse : String -> Maybe (IOCommand String)
parse input =
    let
        commands =
            [ matchCommandArity0 "^ls$" lsCwd
            , matchCommandArity0 "^pwd$" pwd
            , matchCommandArity0 "^cd\\s+\\.\\.$" cdUp
            , matchCommandArity1 "echo\\s+(.*)" echo
            , matchCommandArity1 "cat\\s+([\\w|\\.|/]+)" cat
            , matchCommandArity1 "write\\s+([\\w|\\.|/]+)" write
            , matchCommandArity1 "ls\\s+([\\w|\\.|/]+)" ls
            , matchCommandArity1 "cd\\s+([\\w|\\.|/]+)" cd
            ]
    in
        input
            |> String.split "|"
            |> List.map String.trim
            |> Maybe.Extra.traverse ((flip traverse2) commands)
            |> Maybe.map (List.Extra.foldl1 join)
            |> Maybe.withDefault Nothing


join : (a -> b -> Result c ( d, e )) -> (d -> e -> Result c value) -> a -> b -> Result c value
join cmd1 cmd2 a b =
    case cmd1 a b of
        Ok ( x, y ) ->
            cmd2 x y

        Err s ->
            Err s


traverse2 : a -> List (a -> Maybe b) -> Maybe b
traverse2 a fs =
    case fs of
        [] ->
            Nothing

        f :: rest ->
            case f a of
                Just b ->
                    Just b

                Nothing ->
                    traverse2 a rest


execute : String -> FlatSystem String -> ( String, FlatSystem String )
execute input system =
    case parse input of
        Just command ->
            case command "stdin" system of
                Ok x ->
                    x

                Err s ->
                    ( "Error: " ++ s, system )

        Nothing ->
            ( "Error: failed to parse " ++ input, system )
