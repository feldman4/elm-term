module FileSystem exposing (..)

{-| Mimics UNIX storage.
-}

import Dict exposing (Dict)
import Html exposing (text)
import List.Extra
import Regex


main : Html.Html msg
main =
    text ""


files : List String
files =
    [ "/usr/bin/"
    , "/usr/profile"
    , "/lib/so"
    , "/hello"
    ]


type alias Name =
    String


type alias Content =
    String


type alias Error =
    String



-- PATH


isFolder : String -> Bool
isFolder =
    String.endsWith "/"


isFile : String -> Bool
isFile =
    not << isFolder


isAbsolute : String -> Bool
isAbsolute =
    String.startsWith "/"


makeAbsolute : String -> CWD u -> String
makeAbsolute path { cwd } =
    if isAbsolute path then
        path |> sanitize
    else
        (cwd ++ "/" ++ path) |> sanitize


sanitize : String -> String
sanitize path =
    path |> Regex.replace Regex.All (Regex.regex "/+") (\_ -> "/")


getParent : String -> String
getParent path =
    path
        |> String.split "/"
        |> List.Extra.dropWhileRight ((==) "")
        |> (List.reverse << (List.drop 1) << List.reverse)
        |> String.join "/"
        |> (\s -> (s ++ "/"))


replace : a -> a -> a -> a
replace a1 a2 a =
    if a1 == a then
        a2
    else
        a


getParents : String -> List String
getParents path =
    case getParent path of
        "/" ->
            [ "/" ]

        parent ->
            parent :: getParents parent



-- single dictionary


{-| A simple dictionary from paths to values, with special accessors
for directories. Represent empty directories as Nothing. Only add a file if the
parent directory exists. When removing a key, also remove all keys that begin
with it.
-}
type alias FileSystem a =
    FlatSystem a (Daemonic a)


type alias Daemonic a =
    { daemons : List (MetaDaemon a) }


type alias MetaDaemon a =
    { daemon : Daemon a, lifetime : Int, name : String }


type Daemon a
    = Daemon (FileSystem a -> ( a, FileSystem a, Maybe (Daemon a) ))


type alias FlatSystem a u =
    Storage a (CWD u)


type alias Storage a u =
    { u | storage : Dict Name (Maybe a) }


type alias CWD u =
    { u | cwd : String }


emptySystem : FileSystem a
emptySystem =
    { storage = Dict.empty, cwd = "/", daemons = [] }


validate : FlatSystem a u -> FlatSystem a u
validate ({ storage, cwd } as system) =
    if cwd == "/" then
        system
    else if not (Dict.member cwd storage) then
        validate { system | cwd = getParent cwd }
    else
        system


member : Name -> Storage a u -> Bool
member name system =
    if name == "/" then
        True
    else
        Dict.member name system.storage


safeInsert : Name -> Maybe a -> Storage a u -> Result Error (Storage a u)
safeInsert name x system =
    let
        nameExists =
            member name system

        parentExists =
            member (getParent name) system
    in
        case ( parentExists, nameExists ) of
            ( True, False ) ->
                Result.Ok { system | storage = Dict.insert name x system.storage }

            ( False, _ ) ->
                Result.Err ((getParent name) ++ " : No such directory")

            ( True, True ) ->
                Result.Err (name ++ " : File exists")


{-| Insert with parent directories, overriding existing.
-}
unsafeInsert : Name -> Maybe a -> Storage a u -> Storage a u
unsafeInsert name x system =
    system.storage
        |> Dict.insert name x
        |> (\d -> List.foldl (\x -> Dict.insert x Nothing) d (getParents name))
        |> (\x -> { system | storage = x })


makeDir : Name -> Storage a u -> Result Error (Storage a u)
makeDir name =
    safeInsert name Nothing


addFile : Name -> a -> FlatSystem a u -> Result Error (FlatSystem a u)
addFile name file system =
    safeInsert (makeAbsolute name system) (Just file) system


unsafeAddFile : Name -> a -> FlatSystem a u -> FlatSystem a u
unsafeAddFile name file system =
    unsafeInsert (makeAbsolute name system) (Just file) system


getContents : Name -> Int -> FlatSystem a u -> Result Error (List Name)
getContents name depth system =
    let
        name_ =
            makeAbsolute name system

        test x =
            getParents x
                |> List.take depth
                |> List.member name_
    in
        if member name_ system then
            system.storage
                |> Dict.keys
                |> List.filter (String.contains name_)
                |> List.filter test
                |> List.filter ((/=) name_)
                |> Result.Ok
        else
            Result.Err "No such directory"


get : Name -> FlatSystem a u -> Result Error a
get name system =
    case Dict.get (makeAbsolute name system) system.storage of
        Just (Just x) ->
            Result.Ok x

        Nothing ->
            Result.Err "No such file"

        Just Nothing ->
            Result.Err "Is a directory"
