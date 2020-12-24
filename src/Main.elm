module Main exposing (main)

import Asteroids exposing (Asteroid)
import Lasers
import Physics exposing (isColliding)
import Playground exposing (..)
import Spaceship


main =
    game view update init


type State
    = Home
    | GameOver
    | Playing


type alias Model =
    { asteroids : Asteroids.Model
    , lasers : Lasers.Model
    , seed : Int
    , spaceship : Spaceship.Model
    , state : State
    , ticks : Int
    }


withAsteroids : Asteroids.Model -> Model -> Model
withAsteroids asteroids model =
    { model | asteroids = asteroids }


withLasers : Lasers.Model -> Model -> Model
withLasers lasers model =
    { model | lasers = lasers }


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
    , lasers = Lasers.init
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

        GameOver ->
            model

        Playing ->
            let
                updatedSpaceship =
                    Spaceship.update computer model.spaceship

                updatedAsteroids =
                    model.asteroids
                        |> Asteroids.update computer
                        |> Asteroids.spawn computer model.ticks model.seed

                updatedLasers =
                    model.lasers
                        |> Lasers.update computer
                        |> Lasers.fire computer model.ticks model.spaceship.x model.spaceship.y
            in
            model
                |> withSpaceship updatedSpaceship
                |> withAsteroids updatedAsteroids
                |> withLasers updatedLasers
                |> destroidAsteroids
                |> detectCollision
                |> withNextTick


{-| Remove from game the asteroids hit by a laser ray
-}
destroidAsteroids : Model -> Model
destroidAsteroids model =
    let
        remainingAsteroids : List Asteroid
        remainingAsteroids =
            model.asteroids
                |> List.map (detectHits model.lasers)
                |> List.filter (not << .isHit)
    in
    model |> withAsteroids remainingAsteroids


detectHits : Lasers.Model -> Asteroid -> Asteroid
detectHits lasers asteroid =
    let
        isHit =
            List.any (isColliding asteroid) lasers
    in
    { asteroid | isHit = isHit }


detectCollision : Model -> Model
detectCollision model =
    if isCrashing model then
        model |> withState GameOver

    else
        model


{-| Returns true when the spaceship collides with an asteroid
-}
isCrashing : Model -> Bool
isCrashing model =
    List.any (isColliding model.spaceship) model.asteroids



-- VIEW --


view : Computer -> Model -> List Shape
view computer model =
    case model.state of
        Home ->
            [ viewBackground computer
            , viewTitle
            , viewSubtitle
            ]

        GameOver ->
            [ viewBackground computer
            , viewGameOver
            ]

        Playing ->
            [ viewBackground computer
            , Lasers.view model.lasers
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


viewGameOver : Shape
viewGameOver =
    words white "Game Over"
        |> scale 3


viewTitle : Shape
viewTitle =
    words white "Captain Elm et les astéroïdes de la mort"
        |> scale 3


viewSubtitle : Shape
viewSubtitle =
    words lightGrey "Cliquez pour démarrer la partie"
        |> moveDown 100
