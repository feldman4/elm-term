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



-- string system


stringConfig : Config String
stringConfig =
    { null = ""
    , fromString = identity
    , compile = (\name system a -> Err "Not implemented")
    , append = (++)
    , toString = identity
    , toCommand = (\_ -> Nothing)
    , parsers = Parsers [ parseCommands ]
    }



-- string and executable system


type File
    = Stream String
    | Executable String (IOCommand File)


fileConfig : Config File
fileConfig =
    let
        toString x =
            case x of
                Stream s ->
                    s

                Executable name _ ->
                    "Executable: " ++ name

        toCommand x =
            case x of
                Executable _ command ->
                    Just command

                Stream _ ->
                    Nothing

        compile name system input =
            case input of
                Executable _ command ->
                    Result.Ok (Executable name command)

                Stream expr ->
                    expr
                        |> String.Extra.replace "!" "|"
                        |> parse fileConfig system
                        |> Maybe.Extra.orElseLazy (\() -> parseFiles fileConfig system expr)
                        |> Result.fromMaybe ("failed to parse " ++ expr)
                        |> Result.map (\x -> Executable name x)

        append x y =
            case ( x, y ) of
                ( Stream a, Stream b ) ->
                    Stream (a ++ b)

                ( Executable a f, Executable b g ) ->
                    Executable (a ++ ":" ++ b) (join f g)

                ( Stream a, Executable b g ) ->
                    Executable (b ++ "(" ++ a ++ ")") (join (echo fileConfig a) g)

                ( Executable a f, Stream b ) ->
                    Stream b
    in
        { null = Stream ""
        , fromString = Stream
        , append = append
        , toString = toString
        , toCommand = toCommand
        , compile = compile
        , parsers = Parsers [ parseCommands, parseFiles ]
        }



-- TOP LEVEL


parse : Config a -> FileSystem a -> String -> Maybe (IOCommand a)
parse config system input =
    let
        parsers =
            config.parsers
                |> (\(Parsers p) -> p)
                |> List.map (\x -> x config system)
    in
        input
            |> String.split "|"
            |> List.map String.trim
            |> Maybe.Extra.traverse ((flip traverse2) parsers)
            |> Maybe.map (List.Extra.foldl1 join)
            |> Maybe.withDefault Nothing


{-|
- split at |
- simple matcher: String -> Maybe Command
- register commands as simple matchers
- including ls => a becomes String
-}
parseCommands : Config a -> FileSystem a -> String -> Maybe (IOCommand a)
parseCommands config _ input =
    let
        commands =
            [ -- navigate
              matchCommandArity0 "^$" doNothing
            , matchCommandArity0 "^ls$" (lsCwd config)
            , matchCommandArity0 "^l$" (lsCwd config)
            , matchCommandArity0 "^pwd$" (pwd config)
            , matchCommandArity0 "^help$" (man config)
            , matchCommandArity0 "^cd\\s+\\.\\.$" cdUp
            , matchCommandArity1 "^cd\\s+([\\w|\\.|/]+)$" cd
            , matchCommandArity1 "^ls\\s+([\\w|\\.|/]+)$" (ls config)
              -- manipulate files
            , matchCommandArity1 "^echo\\s+\\\"(.*)\\\"$" (echo config)
            , matchCommandArity1 "^echo\\s+(.*)$" (echo config)
            , matchCommandArity1 "^cat\\s+([\\w|\\.|/]+)$" cat
            , matchCommandArity1 "^write\\s+([\\w|\\.\\*|/]+)$" (write config)
            , matchCommandArity1 "^append\\s+([\\w|\\.|/]+)$" (append config)
            , matchCommandArity1 "^rm\\s+([\\w|\\.|/]+)$" (rm config)
              -- compile
            , matchCommandArity1 "^compile\\s+(\\w+)$" (compile config)
              -- daemons
            , matchCommandArity0 "^daemons$" (showDaemons config)
            , matchCommandArity1 "^kill\\s+([\\w|\\.|/]+)$" (kill config)
            , matchCommandArity2 "^spawn\\s+(\\w+)\\s+(\\w+)$" (spawn config)
            ]
    in
        traverse2 input commands


{-| How to deal with Stream String in a FileSystem a? Probably don't want to
make that executable. Could add a filter in config.runnable.
-}
parseFiles : Config File -> FileSystem File -> String -> Maybe (IOCommand File)
parseFiles config system input =
    let
        isExecutable x =
            case x of
                Executable _ command ->
                    Just command

                Stream _ ->
                    Nothing
    in
        get (String.trim input) system
            |> Result.toMaybe
            |> Maybe.andThen isExecutable


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
  rm [file]      - remove file or directory
  daemons        - show running processes
  spawn [int] [string]
                 - create a process from an executable or by compiling a string
                     echo "echo blah" | spawn 10 nuisance
                     echo "echo cat ! append hello" | spawn 1 catWriter
                     echo "echo cat" | write exe | cat exe | spawn 1 runOnce
  kill [string]  - kill process
  [cmd] | [cmd]  - combine commands by piping stdout to stdin
  compile [string]
                 - create a command with given name from stdin, with the
                 pipe symbol `|` replaced by `!`
  run [file]     - run a command on the contents of a file
                   the following are equivalent if tmp does not exist
                     echo [expr] | compile exe | run file
                     echo [expr] | compile exe | write tmp | cat file | tmp | rm tmp

""" |> String.Extra.replace " " "\x2002"


execute : Config a -> String -> FileSystem a -> ( a, FileSystem a )
execute config input system =
    case parse config system input of
        Just command ->
            case command config.null system of
                Ok x ->
                    x

                Err s ->
                    ( "Error: " ++ s |> config.fromString, system )

        Nothing ->
            ( "Error: failed to parse " ++ input |> config.fromString, system )



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
catDaemon : Config a -> Name -> a -> Daemon a
catDaemon config name content =
    let
        innerDaemon system =
            let
                ( stream_, system_ ) =
                    (append config name) content system
                        |> Result.withDefault ( config.null, system )

                daemonSpawn =
                    catDaemon config name content
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


type alias Config a =
    { null : a
    , fromString : String -> a
    , toString : a -> String
    , toCommand : a -> Maybe (IOCommand a)
    , append : a -> a -> a
    , -- weird to depend on FileSystem, only need FlatSystem a u
      compile :
        String -> FileSystem a -> a -> Result Error a
    , parsers : Parsers a
    }


type Parsers a
    = Parsers (List (Config a -> FileSystem a -> String -> Maybe (IOCommand a)))



-- COMMANDS


{-| Outputs strings. Can be IOCommand a if a function to lift to Stream type
is provided.
-}
ls : Config a -> Name -> IOCommand a
ls { fromString } name _ system =
    let
        name_ =
            makeAbsolute (name ++ "/") system
    in
        getContents name_ 1 system
            |> Result.map ((List.map (String.dropLeft 1)) >> String.join "\n")
            |> Result.map (\a -> [ "---  " ++ name_ ++ "  ---", a ] |> String.join "\n")
            |> Result.map (\a -> ( fromString a, system ))


lsCwd : Config a -> IOCommand a
lsCwd config stream system =
    ls config system.cwd stream system


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


pwd : Config a -> IOCommand a
pwd { fromString } _ system =
    Result.Ok ( fromString system.cwd, system )


echo : Config a -> String -> IOCommand a
echo { fromString } x _ system =
    Result.Ok ( fromString x, system )


man : Config a -> IOCommand a
man { fromString } _ system =
    Result.Ok ( fromString manString, system )


{-| Ignores stdin.
-}
cat : Name -> IOCommand a
cat name _ system =
    get name system
        |> Result.map (\a -> ( a, system ))


{-| Can be IOCommand a if a null stream is provided.
Inside a pipeline, nice if write and append echo to stdout. At the end of a
pipeline, not so nice. Could make writeAndContinue with a tee operator.
-}
write : Config a -> Name -> IOCommand a
write { null } name stream system =
    addFile name stream system
        |> Result.map (\x -> ( null, x ))


append : Config a -> Name -> IOCommand a
append { null, append } name stream system =
    get name system
        |> Result.map (\x -> ( null, unsafeAddFile name (append x stream) system ))


rm : Config a -> Name -> IOCommand a
rm { null, append } name stream system =
    if member (makeAbsolute name system) system then
        Ok ( stream, remove name system )
    else if member (makeAbsolute (name ++ "/") system) system then
        Ok ( stream, remove (makeAbsolute (name ++ "/") system) system )
    else
        Err ("no such file or directory " ++ name)


{-| Compiles incoming data. Looks like a generic transformation if strings and
executables are both in the language.
-}
compile : Config a -> String -> IOCommand a
compile config name stream system =
    config.compile name system stream
        |> Result.map (\a -> ( a, system ))


kill : Config a -> String -> IOCommand a
kill { fromString } daemonName stream system =
    let
        target =
            (\{ name } -> name == daemonName)
    in
        if List.any target system.daemons then
            let
                system_ =
                    { system | daemons = List.filter (not << target) system.daemons }
            in
                Result.Ok ( "killed " ++ daemonName |> fromString, system_ )
        else
            Result.Err ("no daemons around named " ++ daemonName)


{-| Parse parenthesized input, handling errors etc.
-}
spawn : Config a -> String -> String -> IOCommand a
spawn config lifetime daemonName stream system =
    let
        parseResult n =
            config.compile "spawned" system stream
                |> Result.andThen (\x -> x |> config.toCommand |> Result.fromMaybe "wtf")
                |> Result.map (makeDaemon n)

        innerDaemon command system_ =
            let
                ( stream_, system__ ) =
                    command config.null system_ |> Result.withDefault ( config.null, system_ )
            in
                ( stream_, system__, Just (Daemon (innerDaemon command)) )

        makeDaemon lifetime_ command =
            { lifetime = lifetime_
            , name = daemonName
            , daemon = Daemon (innerDaemon command)
            }

        registerDaemon daemon =
            Result.Ok ( stream, { system | daemons = daemon :: system.daemons } )
    in
        String.toInt lifetime
            |> Result.mapError (\_ -> lifetime ++ " is not an integer")
            |> Result.andThen parseResult
            |> Result.andThen registerDaemon


showDaemons : Config a -> IOCommand a
showDaemons { fromString } stream system =
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
            |> (\x -> Result.Ok ( fromString x, system ))


doNothing : IOCommand a
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


matchCommandArity2 : String -> (String -> String -> IOCommand a) -> String -> Maybe (IOCommand a)
matchCommandArity2 pattern command input =
    case Regex.find (Regex.AtMost 1) (Regex.regex pattern) input of
        { submatches } :: _ ->
            case submatches of
                (Just a) :: (Just b) :: [] ->
                    Just (command a b)

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
