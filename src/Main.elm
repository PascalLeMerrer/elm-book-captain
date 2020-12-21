module Main exposing (main)

import Asteroids
import Playground exposing (..)
import Spaceship


main =
    game view update init


type State
    = Home
    | Playing


type alias Model =
    { asteroids : Asteroids.Model
    , seed : Int
    , spaceship : Spaceship.Model
    , state : State
    , ticks : Int
    }


withAsteroids : Asteroids.Model -> Model -> Model
withAsteroids asteroids model =
    { model | asteroids = asteroids }


withNextTick : Model -> Model
withNextTick model =
    { model | ticks = model.ticks + 1 }


withSeed : Int -> Model -> Model
withSeed seed model =
    { model | seed = seed }


withSpaceship : Spaceship.Model -> Model -> Model
withSpaceship spaceship model =
    { model | spaceship = spaceship }


withState : State -> Model -> Model
withState state model =
    { model | state = state }


init : Model
init =
    { asteroids = Asteroids.init
    , spaceship = Spaceship.init
    , seed = 0
    , state = Home
    , ticks = 0
    }



-- UPDATE --


update : Computer -> Model -> Model
update computer model =
    case model.state of
        Home ->
            if computer.mouse.click then
                let
                    spaceshipNewY =
                        computer.screen.bottom + Spaceship.height / 2

                    randomValue =
                        computer.mouse.x * computer.mouse.y * toFloat model.ticks |> round
                in
                model
                    |> withState Playing
                    |> withSpaceship (Spaceship.moveToY spaceshipNewY model.spaceship)
                    |> withNextTick
                    |> withSeed randomValue

            else
                model |> withNextTick

        Playing ->
            let
                updatedSpaceship =
                    Spaceship.update computer model.spaceship

                updatedAsteroids =
                    model.asteroids
                        |> Asteroids.update computer
                        |> Asteroids.spawn computer model.ticks model.seed
            in
            model
                |> withSpaceship updatedSpaceship
                |> withAsteroids updatedAsteroids
                |> withNextTick



-- VIEW --


view : Computer -> Model -> List Shape
view computer model =
    case model.state of
        Home ->
            [ viewBackground computer
            , viewTitle
            , viewSubtitle
            ]

        Playing ->
            [ viewBackground computer
            , Spaceship.view model.spaceship
            , Asteroids.view model.asteroids
            ]


viewBackground : Computer -> Shape
viewBackground computer =
    group
        [ rectangle black computer.screen.width computer.screen.height
        , image computer.screen.width
            computer.screen.height
            "http://localhost:9000/captain/starfield.png"
        ]


viewTitle : Shape
viewTitle =
    words white "Captain Elm et les astéroïdes de la mort"
        |> scale 3


viewSubtitle : Shape
viewSubtitle =
    words lightGrey "Cliquez pour démarrer la partie"
        |> moveDown 100
