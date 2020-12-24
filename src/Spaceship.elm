module Spaceship exposing (Model, height, init, moveToY, update, view)

import Physics exposing (Body)
import Playground exposing (Computer, Shape, group, image, move)


width =
    100


height =
    94


speed =
    5


type alias Model =
    { x : Float
    , y : Float
    , body : Body
    }


init : Model
init =
    { x = 0
    , y = 0
    , body = Physics.createBody 0 0 width height
    }


moveToY : Float -> Model -> Model
moveToY y spaceship =
    { spaceship | y = y }
        |> Physics.updateBody


update : Computer -> Model -> Model
update computer model =
    if computer.keyboard.left then
        moveLeft computer model
            |> Physics.updateBody

    else if computer.keyboard.right then
        moveRight computer model
            |> Physics.updateBody

    else
        model


leftMargin computer =
    computer.screen.left + width / 2


rightMargin computer =
    computer.screen.right - width / 2


moveLeft : Computer -> Model -> Model
moveLeft computer spaceship =
    if spaceship.x > leftMargin computer then
        { spaceship | x = spaceship.x - speed }

    else
        spaceship


moveRight : Computer -> Model -> Model
moveRight computer spaceship =
    if spaceship.x < rightMargin computer then
        { spaceship | x = spaceship.x + speed }

    else
        spaceship


view : Model -> Shape
view model =
    image width height "http://localhost:9000/captain/spaceship.png"
        |> move model.x model.y
