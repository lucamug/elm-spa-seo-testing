port module Main exposing (..)

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Navigation
import UrlParser


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


port urlChange : String -> Cmd msg


type Msg
    = ChangeLocation String
    | UrlChange Navigation.Location


type alias Model =
    { route : Route
    , history : List String
    }


type Route
    = Top
    | About
    | Sitemap
    | NotFound


capitalize : String -> String
capitalize str =
    case String.uncons str of
        Nothing ->
            str

        Just ( firstLetter, rest ) ->
            let
                newFirstLetter =
                    Char.toUpper firstLetter
            in
            String.cons newFirstLetter rest


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map Top UrlParser.top
        , UrlParser.map About (UrlParser.s "about")
        , UrlParser.map Sitemap (UrlParser.s "sitemap")
        ]


routeToPath : Route -> String
routeToPath route =
    case route of
        Top ->
            ""

        About ->
            "about"

        Sitemap ->
            "sitemap"

        NotFound ->
            "notFound"


locationToRoute : Navigation.Location -> Route
locationToRoute location =
    case UrlParser.parsePath matchers location of
        Just route ->
            route

        Nothing ->
            NotFound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "update" msg
    in
    case msg of
        ChangeLocation pathWithSlash ->
            ( model, Navigation.newUrl pathWithSlash )

        UrlChange location ->
            let
                newRoute =
                    locationToRoute location

                newHistory =
                    location.pathname :: model.history
            in
            ( { model | route = newRoute, history = newHistory }
            , urlChange (titleForJs newHistory location)
            )


onLinkClick : String -> Attribute Msg
onLinkClick path =
    onWithOptions "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Decode.succeed (ChangeLocation path))


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "view" model
    in
    div []
        [ node "style" [] [ text css ]
        , viewNavigation model
        , viewPage model
        , h2 [] [ text "History" ]
        , ol [ reversed True ] (List.map (\item -> li [] [ text item ]) model.history)
        ]


pathToName : String -> String
pathToName path =
    if path == "" then
        "Home"
    else
        capitalize path


viewLink : Model -> String -> Route -> Html Msg
viewLink model path route =
    let
        url =
            "/" ++ path
    in
    li
        [ classList [ ( "selected", model.route == route ) ] ]
        [ a [ href url, onLinkClick url ] [ text (pathToName path) ] ]


viewNavigation : Model -> Html Msg
viewNavigation model =
    ul [ class "navigation" ]
        [ viewLink model "" Top
        , viewLink model "about" About
        , viewLink model "sitemap" Sitemap
        ]


viewPage : Model -> Html Msg
viewPage model =
    h1 []
        [ model.route
            |> routeToPath
            |> pathToName
            |> text
        ]


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            initModel location
    in
    ( model
    , initCmd model location
    )


initModel : Navigation.Location -> Model
initModel location =
    { route = locationToRoute location
    , history = [ location.pathname ]
    }


titleForJs : List String -> Navigation.Location -> String
titleForJs history location =
    toString (List.length history) ++ ". " ++ location.pathname


initCmd : Model -> Navigation.Location -> Cmd Msg
initCmd model location =
    Cmd.batch
        [ -- Task.perform (\_ -> Types.FetchUserData) (Process.sleep (0.0 * Time.second))
          --Task.perform (\_ -> UrlChange location) (Process.sleep (0.0 * Time.second))
          urlChange (titleForJs model.history location)
        ]



--            ( { model | route = newRoute, history = location.pathname :: model.history }
--          , urlChange (toString (List.length model.history + 1) ++ ". " ++ location.pathname)
--        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


css : String
css =
    """
body {
  color: #555;
  margin: 10px;
  font-family: sans-serif;
  background-color: #eee;
}
.navigation li {
  display: inline-block;
}


.navigation a {
  display: inline-block;
  margin: 10px;
  padding: 10px;
}
.navigation .selected {
  background-color: orange;
}
h1 {
  color: green;
}
"""


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
