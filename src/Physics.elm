module Physics exposing (..)

import Playground exposing (Shape, green, move, rectangle)


type alias Body =
    { x : Float
    , y : Float
    , halfHeight : Float
    , halfWidth : Float
    , height : Float
    , width : Float
    }


createBody : Float -> Float -> Float -> Float -> Body
createBody x y width height =
    { x = x
    , y = y
    , width = width
    , height = height
    , halfWidth = width / 2
    , halfHeight = height / 2
    }


withX : Float -> Body -> Body
withX x body =
    { body | x = x }


withY : Float -> Body -> Body
withY y body =
    { body | y = y }


type alias PhysicalObject a =
    { a
        | x : Float
        , y : Float
        , body : Body
    }


updateBody : PhysicalObject a -> PhysicalObject a
updateBody object =
    { object
        | body =
            object.body
                |> withX (object.x - object.body.halfWidth)
                |> withY (object.y - object.body.halfHeight)
    }


{-| basic collision detection system. All bodies are rectangles
so we just detect the intersection between rectangles
see <https://developer.mozilla.org/en-US/docs/Games/Techniques/2D_collision_detection> for explanation
-}
isColliding : { a | body : Body } -> { b | body : Body } -> Bool
isColliding object1 object2 =
    let
        body1 =
            object1.body

        body2 =
            object2.body
    in
    (body1.x < body2.x + body2.width)
        && (body1.x + body1.width > body2.x)
        && (body1.y < body2.y + body2.height)
        && (body1.y + body1.height > body2.y)


viewBody : PhysicalObject a -> Shape
viewBody physicalObject =
    let
        body =
            physicalObject.body
    in
    rectangle green body.width body.height
        |> move (body.x + body.halfWidth) (body.y + body.halfHeight)
