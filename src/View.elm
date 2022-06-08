module View exposing (viewCalc)

import Html exposing (Html)
import String exposing (fromInt)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.Events as SE
import View.Attributes exposing (Attribute, BgColor, Shape, Style)



{-
   Possible Tasks
   1. Change the layout dynamically, based on number of rows and columns
   2. Add configurable types like
       1. Width
       2. height
       3. Background Color
       4. roundedness
       5. Border
       6. style
-}


type alias CalcConfig =
    { shape : Shape
    , fcolor : String
    , displaycolor : BgColor
    , style : Style
    , columns : Int
    , padding : String
    , dimentionsbtn : ( Int, Int )
    }


defCalcConfig : CalcConfig
defCalcConfig =
    { shape = View.Attributes.Rect 2 2
    , fcolor = "rgb(100 130 0)"
    , displaycolor =
        { displayTop = "rgb(0 100 100)"
        , displayBottom = "pink"
        , body = "blue"
        }
    , style =
        { size = "10px"
        , weight = "normal"
        , family = "monospace"
        }
    , columns = 4
    , padding = "10px"
    , dimentionsbtn = ( 20, 30 )
    }


viewButtons : ( Float, Float ) -> String -> msg -> Shape -> String -> Style -> Int -> Int -> Svg msg
viewButtons ( x, y ) label msg shape fcolor fntstyle btnH btnW =
    let
        ( transX, transY ) =
            ( x + toFloat btnW / 2, y + toFloat btnH / 2 )
    in
    S.g
        [ SE.onClick msg ]
        [ case shape of
            View.Attributes.Rect rx ry ->
                S.rect
                    [ SA.x (String.fromFloat x)
                    , SA.y (String.fromFloat y)
                    , SA.height (String.fromInt btnH)
                    , SA.width (String.fromInt btnW)
                    , SA.rx (fromInt rx)
                    , SA.ry (fromInt ry)
                    , SA.style ("fill:" ++ fcolor ++ ";stroke-width:0.5;stroke:rgb(0,0,0)")
                    , SA.fillOpacity "0.5"
                    , SA.cursor "pointer"
                    ]
                    []

            View.Attributes.Circle r ->
                S.circle
                    [ SA.cx (String.fromFloat (x + toFloat btnW / 2))
                    , SA.cy (String.fromFloat (y + toFloat btnH / 2))
                    , SA.r (fromInt r)
                    , SA.style ("fill:" ++ fcolor ++ ";stroke-width:0.5;stroke:rgb(0,0,0)")
                    , SA.fillOpacity "0.5"
                    , SA.cursor "pointer"
                    ]
                    []

            View.Attributes.Ellipse rx ry ->
                S.ellipse
                    [ SA.cx (String.fromFloat (x + toFloat btnW / 2))
                    , SA.cy (String.fromFloat (y + toFloat btnH / 2))
                    , SA.rx (fromInt rx)
                    , SA.ry (fromInt ry)
                    , SA.style ("fill:" ++ fcolor ++ ";stroke-width:0.5;stroke:rgb(0,0,0)")
                    , SA.fillOpacity "0.5"
                    , SA.cursor "pointer"
                    ]
                    []
        , S.text_
            [ SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.transform ("translate(" ++ String.fromFloat transX ++ " " ++ String.fromFloat transY ++ ")")
            , SA.fontWeight fntstyle.weight
            , SA.fontSize fntstyle.size
            , SA.fontFamily fntstyle.family
            , SA.cursor "pointer"
            ]
            [ S.text label ]
        ]


viewDisplay : ( Float, Float ) -> ( Float, Float ) -> String -> String -> Svg msg
viewDisplay ( x, y ) ( w, h ) str disp =
    let
        ( transX, transY ) =
            ( x + w / 2, y + h / 2 )
    in
    S.g
        []
        [ S.rect
            [ SA.x (String.fromFloat x)
            , SA.y (String.fromFloat y)
            , SA.height (String.fromFloat h)
            , SA.width (String.fromFloat w)
            , SA.rx "5"
            , SA.ry "5"
            , SA.style ("fill:" ++ disp ++ ";stroke-width:0.5;stroke:rgb(0,0,0)")
            , SA.fillOpacity "0.5"
            ]
            []
        , S.text_
            [ SA.textAnchor "middle"
            , SA.dominantBaseline "central"
            , SA.transform ("translate(" ++ String.fromFloat transX ++ " " ++ String.fromFloat transY ++ ")")
            ]
            [ S.text str ]
        ]



{-
   This is the main view function for calculator
   It takes all the data required to show a calculator
   1. The attributes like height, width etc
   2. The history
   3. The final answer
   4. All the buttons to be shown

-}


viewCalc : List (Attribute CalcConfig) -> String -> String -> List ( String, msg ) -> Html msg
viewCalc edits history answer buttons =
    let
        config =
            List.foldl (\f a -> f a) defCalcConfig edits

        cols =
            config.columns

        buttonGroups =
            splitAtEvery cols buttons

        coordButtons =
            getCoordinatedList 50 height width buttonGroups

        maxW =
            (List.maximum (List.map (\( ( x, _ ), _ ) -> x) coordButtons)
                |> Maybe.withDefault 500
            )
                + toFloat width

        rows =
            (List.length buttons + cols - 1) // cols

        bgbody =
            config.displaycolor.body

        bgtop =
            config.displaycolor.displayTop

        bgbot =
            config.displaycolor.displayBottom

        height =
            Tuple.first config.dimentionsbtn

        width =
            Tuple.second config.dimentionsbtn
    in
    -- S.svg
    --     [ SA.viewBox ("0 0 " ++ String.fromFloat config.width ++ " " ++ String.fromFloat config.height)
    --     , SA.height "75vh"
    --     , SA.style "border-style:solid;padding:25px;"
    --     ]
    --     (viewDisplay ( 0, 0 ) ( maxW, 20 ) history
    --         :: viewDisplay ( 0, 25 ) ( maxW, 20 ) answer
    --         :: List.map
    --             (\( c, ( l, m ) ) -> viewButtons c l config.padding config.fcolor config.nofcol m)
    --             coordButtons
    --     )
    S.svg
        [ SA.viewBox ("-5 -5 " ++ String.fromInt ((width + 10) * cols) ++ " " ++ String.fromInt ((height + 10) * rows + 50))
        , SA.height "75vh"
        , SA.style ("border-style:solid;padding:" ++ config.padding ++ ";background:" ++ bgbody ++ ";")
        ]
        (viewDisplay ( 0, 0 ) ( maxW, 20 ) history bgtop
            :: viewDisplay ( 0, 25 ) ( maxW, 20 ) answer bgbot
            :: List.map
                (\( c, ( l, m ) ) -> viewButtons c l m config.shape config.fcolor config.style height width)
                coordButtons
        )



-- I N T E R N A L      H E L P E R S


splitAtEvery : Int -> List a -> List (List a)
splitAtEvery index lst =
    case lst of
        [] ->
            []

        _ ->
            List.append [ List.take index lst ] (splitAtEvery index (List.drop index lst))


getCoordinatedList : Float -> Int -> Int -> List (List ( String, msg )) -> List ( ( Float, Float ), ( String, msg ) )
getCoordinatedList initY height width lst =
    let
        p =
            10

        assignXCoord y list =
            List.foldl (\e ( x, fe ) -> ( x + toFloat width + p, List.append fe [ ( ( x, y ), e ) ] )) ( 0, [] ) list
                |> Tuple.second

        ( _, yLists ) =
            List.foldl (\l ( y, ls ) -> ( y + toFloat height + p, List.append ls [ ( y, l ) ] )) ( initY, [] ) lst
    in
    List.map (\( y, l ) -> assignXCoord y l) yLists
        |> List.concat
