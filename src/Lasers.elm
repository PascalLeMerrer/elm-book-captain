module Lasers exposing (..)

import Physics exposing (Body)
import Playground exposing (Computer, Shape, group, image, move)


type alias Laser =
    { x : Float
    , y : Float
    , body : Body
    }


height =
    54


width =
    9


speed =
    5


type alias Model =
    List Laser


init : Model
init =
    []


update : Computer -> Model -> Model
update computer model =
    model
        |> List.map updateLaser
        |> List.filter (\laser -> laser.y < computer.screen.top)


updateLaser : Laser -> Laser
updateLaser laser =
    { laser
        | y = laser.y + speed
    }
        |> Physics.updateBody


intervalBetweenFireshots =
    20


{-| Emits a laser ray at a given x position, when space is pressed. The emission rate is limited.
-}
fire : Computer -> Int -> Float -> Float -> Model -> Model
fire computer ticks x y model =
    if computer.keyboard.space && modBy intervalBetweenFireshots ticks == 0 then
        let
            newLaser : Laser
            newLaser =
                { x = x
                , y = y
                , body = Physics.createBody x y width height
                }
        in
        newLaser :: model

    else
        model


view : Model -> Shape
view model =
    group <|
        List.map viewLaser model


viewLaser : Laser -> Shape
viewLaser laser =
    image width height "http://localhost:9000/captain/laser.png"
        |> move laser.x laser.y
