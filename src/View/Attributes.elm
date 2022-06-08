module View.Attributes exposing (..)


type alias Attribute c =
    c -> c


type Shape
    = Rect Int Int
    | Circle Int
    | Ellipse Int Int


type alias Style =
    { size : String
    , weight : String
    , family : String
    }


type alias BgColor =
    { displayTop : String
    , displayBottom : String
    , body : String
    }



-- type alias Config =
--     { width : Float
--     , height : Float
--     , padding : Int
--     , fcolor : String
--     , columns : Int
--     , bgcolor : String
--     }
{- Function to set the width attribute of CalcConfig -}


width : Float -> Attribute { c | width : Float }
width f =
    \cc ->
        { cc | width = f }



{- Function to set the height attribute of CalcConfig -}


height : Float -> Attribute { c | height : Float }
height f =
    \cc ->
        { cc | height = f }



{- Function to set the padding attribute of CalcConfig -}
-- padding : Float -> Attribute { c | padding : Float }
-- padding f =
--     \cc ->
--         { cc | padding = f }
{- Function to add style attribute of CalcConfig -}


shape : Shape -> Attribute { c | shape : Shape }
shape f =
    \cc ->
        { cc | shape = f }

{-Function add style attribute of CalcConfig -}


displaycolor : BgColor -> Attribute { c | displaycolor : BgColor }
displaycolor f =
    \cc ->
        { cc | displaycolor = f }
{-Function add display attribute of CalcConfig-}


fcolor : String -> Attribute { c | fcolor : String }
fcolor f =
    \cc ->
        { cc | fcolor = f }
{-function to add fcolor attribute of CalcConfig-}


columns : Int -> Attribute { c | columns : Int }
columns f =
    \cc ->
        { cc | columns = f }


padding : String -> Attribute { c | padding : String }
padding f =
    \cc ->
        { cc | padding = f }


dimentionsbtn : ( Int, Int ) -> Attribute { c | dimentionsbtn : ( Int, Int ) }
dimentionsbtn f =
    \cc ->
        { cc | dimentionsbtn = f }


style : Style -> Attribute { c | style : Style }
style f =
    \cc ->
        { cc | style = f }



{- Function to add style attribute of CalcConfig -}
