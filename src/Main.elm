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
    , spaceship : Spaceship.Model
    , state : State
    }


withAsteroids : Asteroids.Model -> Model -> Model
withAsteroids asteroids model =
    { model | asteroids = asteroids }


withSpaceship : Spaceship.Model -> Model -> Model
withSpaceship spaceship model =
    { model | spaceship = spaceship }


init : Model
init =
    { asteroids = Asteroids.init
    , spaceship = Spaceship.init
    , state = Home
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
                in
                { model
                    | state = Playing
                    , spaceship =
                        model.spaceship
                            |> Spaceship.moveToY spaceshipNewY
                }

            else
                model

        Playing ->
            let
                updatedSpaceship =
                    Spaceship.update computer model.spaceship

                updatedAsteroids =
                    Asteroids.update computer model.asteroids
            in
            model
                |> withSpaceship updatedSpaceship
                |> withAsteroids updatedAsteroids



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
