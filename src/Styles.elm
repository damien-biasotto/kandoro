module Styles exposing (defaultPalette, style)

import Html.Styled exposing (..)
import Css exposing (..)
import Css exposing (Px)
import Css.Global exposing (global, class, children, everything, selector, typeSelector)

type PaletteProperties
    = PaletteProperties {
        color: Color
        , light: Color
        , dark: Color
        , text: Color
    }


type Palette
    = Palette {
        primary : PaletteProperties
        , secondary : PaletteProperties
        }

{- taken from https://www.sitepoint.com/building-trello-layout-css-grid-flexbox/ -}
appBarHeight : Int
appBarHeight = 40

navBarHeight : Int
navBarHeight = 50

listWidth : Int
listWidth = 300

gap : Int
gap = 10

scrollBarThickNess : Int
scrollBarThickNess = 17

listHeaderHeight : Int
listHeaderHeight = 36

listFooterHeight : Int
listFooterHeight = 36

listBorderRadius : Int
listBorderRadius = 5

cardBorderRadius : Int
cardBorderRadius = 3

toPx int
    = px <| toFloat int

style : Palette -> Html msg
style (Palette palette) =
    let
        (PaletteProperties primaryStyle) = palette.primary
        (PaletteProperties secondaryStyle) = palette.secondary
    in
    global [
        typeSelector "body" [
            margin zero
            , fontFamily sansSerif
            , fontSize <| px 14
            , lineHeight <| Css.em 1.3
        ]
        , class "board"
        [ height <| vh 100
        , property "display" "grid"
        , property "grid-template-rows" ((List.foldr (\x acc -> String.fromInt x ++ "px " ++ acc) "" [appBarHeight, navBarHeight]) ++ " 1fr")
        , backgroundColor secondaryStyle.dark
        ]
        , selector "[class^=navbar--]" [
            paddingLeft <| toPx gap
            , displayFlex
            , alignItems center
            , color primaryStyle.text
            ]
        , class "navbar--app-bar" [
            backgroundColor primaryStyle.dark
            , fontSize <| Css.rem 1.5
            ]
        , class "navbar--board-bar" [
            backgroundColor secondaryStyle.dark
            , fontSize <| Css.rem 1.1
            ]
    , class "board--content" [
        displayFlex
        , overflowX auto
        , children
        [
            everything [
                flexGrow zero
                , flexShrink zero
                , flexBasis auto
                , marginLeft <| toPx gap
            ]
         ]
         , after [
                        property "content" ""
                        , flexGrow zero
                        , flexShrink zero
                        , flexBasis <| toPx gap
            ]

    ]
    , class "board--column--content" [
        children [
            typeSelector "ul" [
            maxHeight <| calc (pct 100) minus (calc (toPx listHeaderHeight) plus (toPx listFooterHeight))
            , listStyle none
            , margin zero
            , padding zero
            , overflowY auto
            , children [
                selector "li:not(:last-child)" [

                        marginBottom <| toPx gap
                   ]
                , typeSelector "li" [
                    backgroundColor <| secondaryStyle.dark
                    , padding <| toPx gap
                    , borderRadius <| toPx cardBorderRadius
                    , boxShadow4 zero (px 1) (px 1) (rgba 0 0 0 0.1)
                    , children [
                        typeSelector "img" [
                            display block
                            , width <| calc (pct 100) plus (toPx <| 2 * gap)
                            , margin4 (toPx <| 0 - gap) zero (toPx <| gap) (toPx <| 0 - gap)
                            , borderTopLeftRadius <| toPx cardBorderRadius
                            , borderTopRightRadius <| toPx cardBorderRadius
                        ]
                    ]
                ]
                ]
                ]
        ]
    ]
    , class "board--column" [
        height <| calc (pct 100) minus (calc (toPx gap) plus (toPx scrollBarThickNess))
        , width <| toPx listWidth
        , children [
            everything [
                backgroundColor <| primaryStyle.light
                , color <| primaryStyle.text
                , padding2 zero (toPx gap)
            ]
         ]
        , children [
            typeSelector "header" [
                lineHeight <| toPx listHeaderHeight
                , fontSize (px 16)
                , fontWeight bold
                , borderTopLeftRadius <| toPx listBorderRadius
                , borderTopRightRadius <| toPx listBorderRadius
                ]
            , typeSelector "footer" [
                lineHeight <| toPx listFooterHeight
                , borderBottomLeftRadius <| toPx listBorderRadius
                , borderBottomRightRadius <| toPx listBorderRadius
                , color <| hex "ffffff"
                ]
            ]
        ]
    ]

defaultPalette : Palette
defaultPalette =
    (Palette {
        primary = (PaletteProperties {
            color = hex "7b1fa2"
            , light = hex "ae52d4"
            , dark = hex "4a0072"
            , text = hex "ffffff"
            })
        , secondary = (PaletteProperties {
             color = hex "aa00ff"
             , light = hex "e254ff"
             , dark = hex "7200ca"
             , text = hex "ffffff"
            })
        })
