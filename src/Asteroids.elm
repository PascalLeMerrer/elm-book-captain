module Asteroids exposing (..)

import Physics exposing (Body)
import Playground exposing (Computer, Shape, group, image, move)


type alias Asteroid =
    { x : Float
    , y : Float
    , body : Body
    }


height =
    92


width =
    89


speed =
    3


{-| The number of ticks between two asteroid apparitions
-}
intervalBetweenAsteroids =
    200


type alias Model =
    List Asteroid


init : Model
init =
    []


update : Computer -> Model -> Model
update computer model =
    model
        |> List.map updateAsteroid
        |> List.filter (\asteroid -> asteroid.y > computer.screen.bottom)


updateAsteroid : Asteroid -> Asteroid
updateAsteroid asteroid =
    { asteroid | y = asteroid.y - speed }
        |> Physics.updateBody


{-| Adds a new asteroid at a pseudo-random place on top of the screen, at regular time intervals
-}
spawn : Computer -> Int -> Int -> Model -> Model
spawn computer ticks seed model =
    if modBy intervalBetweenAsteroids ticks == 0 then
        let
            x =
                (cos <| toFloat ticks * toFloat seed) * (computer.screen.width / 2 - width * 2) + width

            y =
                computer.screen.top - height

            newAsteroid =
                { x = x
                , y = y
                , body = Physics.createBody x y (width - 10) (height - 20)
                }
        in
        newAsteroid :: model

    else
        model


view : Model -> Shape
view model =
    group <|
        List.map viewAsteroid model


viewAsteroid : Asteroid -> Shape
viewAsteroid asteroid =
    group
        [ Physics.viewBody asteroid
        , image width height "http://localhost:9000/captain/asteroid.png"
            |> move asteroid.x asteroid.y
        ]
