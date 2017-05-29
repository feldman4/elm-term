module MyCss exposing (..)

import Css exposing (..)
import Css.Elements exposing (body, li)
import Css.Namespace exposing (namespace)
import Html.Attributes
import Html


type CssClasses
    = NavBar


type CssIds
    = Page


styles : List Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style


aStyle : List Mixin
aStyle =
    [ position relative, left (px 5) ]


backgroundStyle : List Mixin
backgroundStyle =
    [ backgroundColor (hex "000000")
    , color (hex "ff00ff")
    ]


terminalText : List Mixin
terminalText =
    [ fontFamily monospace ]


fullWindow : List Mixin
fullWindow =
    [ height (pct 100), overflow scroll ]


css : Stylesheet
css =
    (stylesheet << namespace "dreamwriter")
        [ body
            [ overflowX auto
            , minWidth (px 1280)
            ]
        , id Page
            [ backgroundColor (rgb 200 128 64)
            , color (hex "CCFFFF")
            , width (pct 100)
            , height (pct 100)
            , boxSizing borderBox
            , padding (px 8)
            , margin zero
            ]
        , class NavBar
            [ margin zero
            , padding zero
            , children
                [ li
                    [ (display inlineBlock) |> important
                    , color primaryAccentColor
                    ]
                ]
            ]
        ]


primaryAccentColor : Color
primaryAccentColor =
    hex "ccffaa"
