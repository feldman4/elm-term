module Command exposing (..)

import FileSystem exposing (..)
import Html exposing (text)
import List.Extra
import Regex exposing (Regex)
import Maybe.Extra


{-| Mimics UNIX command line.
-}
main : Html.Html msg
main =
    text ""



-- TOP LEVEL


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
            [ matchCommandArity0 "^$" doNothing
            , matchCommandArity0 "^ls$" lsCwd
            , matchCommandArity0 "^l$" lsCwd
            , matchCommandArity0 "^pwd$" pwd
            , matchCommandArity0 "^cd\\s+\\.\\.$" cdUp
            , matchCommandArity1 "echo\\s+(.*)" echo
            , matchCommandArity1 "cat\\s+([\\w|\\.|/]+)" cat
            , matchCommandArity1 "write\\s+([\\w|\\.|/]+)" write
            , matchCommandArity1 "append\\s+([\\w|\\.|/]+)" append
            , matchCommandArity1 "ls\\s+([\\w|\\.|/]+)" ls
            , matchCommandArity1 "cd\\s+([\\w|\\.|/]+)" cd
            , matchCommandArity0 "^daemons$" showDaemons
            ]
    in
        input
            |> String.split "|"
            |> List.map String.trim
            |> Maybe.Extra.traverse ((flip traverse2) commands)
            |> Maybe.map (List.Extra.foldl1 join)
            |> Maybe.withDefault Nothing


execute : String -> FileSystem String -> ( String, FileSystem String )
execute input system =
    case parse input of
        Just command ->
            case command "" system of
                Ok x ->
                    x

                Err s ->
                    ( "Error: " ++ s, system )

        Nothing ->
            ( "Error: failed to parse " ++ input, system )



-- DAEMONS


type alias DaemonPipe a =
    ( List a, FileSystem a, List (Daemon a) )


updateDaemons : Int -> FileSystem a -> List (Daemon a) -> DaemonPipe a
updateDaemons cycles system daemons =
    let
        ( streams, system_, daemons_ ) =
            List.foldl (updateDaemon cycles) ( [], system, [] ) daemons
    in
        ( streams, { system_ | daemons = daemons_ }, [] )


updateDaemon : Int -> Daemon a -> DaemonPipe a -> DaemonPipe a
updateDaemon cycles (Daemon _ _ daemon) ( stream, system, daemons ) =
    case daemon cycles system of
        ( stream_, system_, Just daemon_ ) ->
            ( stream_ :: stream, system_, daemon_ :: daemons )

        ( stream_, system_, Nothing ) ->
            ( stream_ :: stream, system_, daemons )


{-| Creates its own replacement.
TODO: eliminate double lifetime
-}
catDaemon : Int -> String -> Daemon String
catDaemon cycles name =
    if cycles <= 0 then
        Daemon ("cat:" ++ name) 0 (\cycles_ system -> ( "", system, Nothing ))
    else
        let
            innerDaemon elapsed system =
                let
                    ( stream_, system_ ) =
                        (append name) "cat" system |> Result.withDefault ( "", system )

                    daemonSpawn =
                        catDaemon (cycles - elapsed) name
                in
                    ( stream_, system_, Just daemonSpawn )
        in
            Daemon ("cat:" ++ name) cycles innerDaemon



-- TYPES


type alias Stream =
    String


type alias Error =
    String


type alias IOCommand a =
    a -> FileSystem a -> Result Error ( a, FileSystem a )



-- COMMANDS


{-| Outputs strings. Can be IOCommand a if a function to lift to Stream type
is provided.
-}
ls : Name -> IOCommand String
ls name _ system =
    let
        name_ =
            makeAbsolute (name ++ "/") system
    in
        getContents name_ 1 system
            |> Result.map ((List.map (String.dropLeft 1)) >> String.join "\n")
            |> Result.map (\a -> [ "---  " ++ name_ ++ "  ---", a ] |> String.join "\n")
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
    get name system
        |> Result.map (\a -> ( a, system ))


{-| Can be IOCommand a if a null stream is provided.
-}
write : Name -> IOCommand String
write name stream system =
    addFile name stream system
        |> Result.map (\x -> ( "", x ))


append : Name -> IOCommand String
append name stream system =
    get name system
        |> Result.map (\x -> ( "", unsafeAddFile name (x ++ stream) system ))


showDaemons : IOCommand String
showDaemons stream system =
    let
        format (Daemon name lifetime _) =
            (toString lifetime) ++ " " ++ name
    in
        system.daemons
            |> List.map format
            |> String.join "\n"
            |> (\x -> Result.Ok ( x, system ))


doNothing : IOCommand String
doNothing stream system =
    Result.Ok ( stream, system )


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



-- UTILITIES


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
