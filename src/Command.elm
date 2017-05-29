module Command exposing (..)

import FileSystem exposing (..)
import Html exposing (text)
import List.Extra
import Maybe.Extra
import Regex exposing (Regex)
import String.Extra


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
            [ -- navigate
              matchCommandArity0 "^$" doNothing
            , matchCommandArity0 "^ls$" lsCwd
            , matchCommandArity0 "^l$" lsCwd
            , matchCommandArity0 "^pwd$" pwd
            , matchCommandArity0 "^help$" man
            , matchCommandArity0 "^cd\\s+\\.\\.$" cdUp
            , matchCommandArity1 "^ls\\s+([\\w|\\.|/]+)" ls
            , matchCommandArity1 "^cd\\s+([\\w|\\.|/]+)" cd
              -- manipulate files
            , matchCommandArity1 "^echo\\s+(.*)" echo
            , matchCommandArity1 "^cat\\s+([\\w|\\.|/]+)" cat
            , matchCommandArity1 "^write\\s+([\\w|\\.|/]+)" write
            , matchCommandArity1 "^append\\s+([\\w|\\.|/]+)" append
              -- daemons
            , matchCommandArity0 "^daemons$" showDaemons
            , matchCommandArity1 "^kill\\s+([\\w|\\.|/]+)" kill
            , matchCommandArity3 "^spawn\\s+([0-9]+)\\s+(\\w+)\\s+\\(\\s*(.*?)\\s*\\)" spawn
            ]
    in
        input
            |> String.split "|"
            |> List.map String.trim
            |> Maybe.Extra.traverse ((flip traverse2) commands)
            |> Maybe.map (List.Extra.foldl1 join)
            |> Maybe.withDefault Nothing


manString : String
manString =
    """
  help           - print help to stdout
  l              - show directory contents
  ls             - show directory contents
  pwd            - print working directory
  cd [directory] - change directory
  echo [string]  - send text to stdout
  cat [file]     - send file contents to stdout
  write [file]   - write stdin to file, if file doesn't exist
  append [file]  - append stdin to file, if file exists
  daemons        - show running processes
  spawn [int] [string] (expr)
                 - create a process, e.g.,
                     (spawn 10 nuisance (echo blah))
                     (spawn 1 catWriter (echo cat ! append hello))
                   the parentheses can contain any valid expression that
                   does not use spawn, with the pipe symbol `|` replaced by `!`
  kill [string]  - kill process
  [cmd] | [cmd]  - combine commands by piping stdout to stdin
""" |> String.Extra.replace " " "\x2002"


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
    ( List a, FileSystem a, List (MetaDaemon a) )


{-| Daemons act on FileSystem sequentially. Weird as they have access to daemon
list within FileSystem, but any changes to it are discarded at the end of the
update (can't delete each other). One fix is to Daemon a u with the filesystem
as a parameter but postponing for now.
-}
updateDaemons : Int -> FileSystem a -> List (MetaDaemon a) -> DaemonPipe a
updateDaemons cycles system daemons =
    let
        ( streams, system_, daemons_ ) =
            List.foldl (updateDaemon cycles) ( [], system, [] ) daemons
    in
        ( streams, { system_ | daemons = List.reverse daemons_ }, [] )


updateDaemon : Int -> MetaDaemon a -> DaemonPipe a -> DaemonPipe a
updateDaemon cycles meta ( stream, system, daemons ) =
    if meta.lifetime == 0 then
        -- delete daemon
        ( stream, system, daemons )
    else
        let
            unpack =
                (\(Daemon d) -> d)
        in
            case (unpack meta.daemon) system of
                ( stream_, system_, Just daemon_ ) ->
                    ( stream_ :: stream, system_, { meta | daemon = daemon_, lifetime = meta.lifetime - cycles } :: daemons )

                ( stream_, system_, Nothing ) ->
                    ( stream_ :: stream, system_, daemons )


{-|
-}
catDaemon : Name -> String -> Daemon String
catDaemon name content =
    let
        innerDaemon system =
            let
                ( stream_, system_ ) =
                    (append name) content system |> Result.withDefault ( "", system )

                daemonSpawn =
                    catDaemon name content
            in
                ( stream_, system_, Just daemonSpawn )
    in
        Daemon innerDaemon



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


man : IOCommand String
man _ system =
    Result.Ok ( manString, system )


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


kill : Name -> IOCommand String
kill daemonName stream system =
    let
        target =
            (\{ name } -> name == daemonName)
    in
        if List.any target system.daemons then
            let
                system_ =
                    { system | daemons = List.filter (not << target) system.daemons }
            in
                Result.Ok ( "killed " ++ daemonName, system_ )
        else
            Result.Err ("no daemons around named " ++ daemonName)


{-| Parse parenthesized input, handling errors etc.
-}
spawn : String -> String -> String -> IOCommand String
spawn lifetime daemonName input stream system =
    let
        parseResult n =
            input
                |> String.Extra.replace "!" "|"
                |> parse
                |> Result.fromMaybe ("failed to parse command " ++ input)
                |> Result.map (makeDaemon n)

        stdin =
            ""

        innerDaemon : IOCommand String -> (FileSystem String -> ( String, FileSystem String, Maybe (Daemon String) ))
        innerDaemon command system_ =
            let
                ( stream_, system__ ) =
                    command stdin system_ |> Result.withDefault ( "", system_ )
            in
                ( stream_, system__, Just (Daemon (innerDaemon command)) )

        makeDaemon lifetime_ command =
            { lifetime = lifetime_
            , name = daemonName
            , daemon = Daemon (innerDaemon command)
            }

        registerDaemon daemon =
            Result.Ok ( stream, { system | daemons = (Debug.log "new d" daemon) :: system.daemons } )
    in
        String.toInt (Debug.log "l" lifetime)
            |> Result.mapError (\_ -> "lifetime " ++ lifetime ++ "is not an int")
            |> Result.andThen parseResult
            |> Result.andThen registerDaemon


showDaemons : IOCommand String
showDaemons stream system =
    let
        format { name, lifetime } =
            (toString lifetime) ++ " " ++ name

        plural n =
            if n == 1 then
                ""
            else
                "s"

        header =
            List.length system.daemons
                |> (\n -> ( toString n, plural n ))
                |> (\( n, s ) -> "---  " ++ n ++ " active daemon" ++ s ++ "  ---\n")
    in
        system.daemons
            |> List.map format
            |> String.join "\n"
            |> (++) header
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


matchCommandArity3 : String -> (String -> String -> String -> IOCommand a) -> String -> Maybe (IOCommand a)
matchCommandArity3 pattern command input =
    case Regex.find (Regex.AtMost 1) (Regex.regex pattern) (Debug.log "input" input) of
        { submatches } :: _ ->
            case submatches of
                (Just a) :: (Just b) :: (Just c) :: [] ->
                    Just (command a b c)

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
