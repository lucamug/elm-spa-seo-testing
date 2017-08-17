port module Main exposing (..)

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Navigation
import UrlParser
import Process
import Time
import Http
import Task
import Regex


(=>) : a -> b -> ( a, b )
(=>) =
    (,)


port urlChange : String -> Cmd msg


type Msg
    = ChangeLocation String
    | UrlChange Navigation.Location
    | NewApi1Data (Result Http.Error Api1Data)
    | NewApi2Data (Result Http.Error Api2Data)
    | FetchApi1Data String
    | FetchApi2Data String


type alias Model =
    { route : Route
    , history : List String
    , api1Data : String
    , api2Data : String
    , location : Navigation.Location
    , version : String
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
    case msg of
        ChangeLocation pathWithSlash ->
            ( model, Navigation.newUrl pathWithSlash )

        UrlChange location ->
            let
                newRoute =
                    locationToRoute location

                newHistory =
                    location.pathname :: model.history

                newModel =
                    { model | route = newRoute, history = newHistory, location = location }
            in
                ( newModel
                , urlChange (titleForJs newModel)
                )

        NewApi1Data result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model | api1Data = data.url }
                    in
                        ( newModel
                        , urlChange (titleForJs newModel)
                        )

                Err data ->
                    ( model, Cmd.none )

        FetchApi1Data url ->
            ( model, Http.send NewApi1Data (Http.get url api1Decoder) )

        NewApi2Data result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model | api2Data = data.url }
                    in
                        ( newModel
                        , urlChange (titleForJs newModel)
                        )

                Err data ->
                    ( model, Cmd.none )

        FetchApi2Data url ->
            ( model, Http.send NewApi2Data (Http.get url api2Decoder) )


onLinkClick : String -> Attribute Msg
onLinkClick path =
    onWithOptions "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Decode.succeed (ChangeLocation path))


view : Model -> Html Msg
view model =
    div []
        [ node "style" [] [ text css ]
        , viewNavigation model
        , p [] [ text ("Title: " ++ (titleForJs model)) ]
        , p []
            [ text "History: "
            , span []
                (List.map (\item -> span [ class "history" ] [ text item ])
                    (List.reverse (model.history))
                )
            ]
        , viewPage model
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
    div []
        [ h1 []
            [ model.route
                |> routeToPath
                |> pathToName
                |> text
            ]
        , pre [ class "dante" ] [ text """Midway upon the journey of our life
I found myself within a forest dark,
For the straightforward pathway had been lost.

Ah me! how hard a thing it is to say
What was this forest savage, rough, and stern,
Which in the very thought renews the fear.

So bitter is it, death is little more;
But of the good to treat, which there I found,
Speak will I of the other things I saw there.

I cannot well repeat how there I entered,
So full was I of slumber at the moment
In which I had abandoned the true way.

But after I had reached a mountain's foot,
At that point where the valley terminated,
Which had with consternation pierced my heart,

Upward I looked, and I beheld its shoulders
Vested already with that planet's rays
Which leadeth others right by every road.

Then was the fear a little quieted
That in my heart's lake had endured throughout
The night, which I had passed so piteously

And even as he, who, with distressful breath,
Forth issued from the sea upon the shore,
Turns to the water perilous and gazes;

So did my soul, that still was fleeing onward,
Turn itself back to re-behold the pass
Which never yet a living person left.""" ]
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
    , api1Data = ""
    , api2Data = ""
    , location = location
    , version = "02"
    }


titleForJs model =
    let
        num1 =
            extractNumber model.api1Data

        num2 =
            extractNumber model.api2Data

        historyLength =
            toString (List.length model.history)
    in
        "V"
            ++ model.version
            ++ ",H"
            ++ historyLength
            ++ ",Loc"
            ++ num1
            ++ ",Rem"
            ++ num2
            ++ ","
            ++ model.location.pathname


initCmd : Model -> Navigation.Location -> Cmd Msg
initCmd model location =
    Cmd.batch
        [ urlChange (titleForJs model)
        , Task.perform (\_ -> FetchApi1Data "10.json") (Process.sleep (10.0 * Time.second))
        , Task.perform (\_ -> FetchApi1Data "06.json") (Process.sleep (6.0 * Time.second))
        , Task.perform (\_ -> FetchApi1Data "03.json") (Process.sleep (3.0 * Time.second))
        , Task.perform (\_ -> FetchApi1Data "01.json") (Process.sleep (1.0 * Time.second))
        , Task.perform (\_ -> FetchApi1Data "00.json") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/10") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/06") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/03") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/01") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/00") (Process.sleep (0.0 * Time.second))
        ]


extractNumber text =
    let
        number =
            Regex.find Regex.All (Regex.regex "\\d\\d") text
    in
        case List.head number of
            Nothing ->
                "[NaN]"

            Just data ->
                data.match


type alias Api1Data =
    { url : String }


type alias Api2Data =
    { url : String }


api1Decoder : Decode.Decoder Api1Data
api1Decoder =
    Decode.map Api1Data (Decode.at [ "url" ] Decode.string)


api2Decoder : Decode.Decoder Api2Data
api2Decoder =
    Decode.map Api2Data (Decode.at [ "url" ] Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


greenBright =
    "#7effca"


greenDark =
    "#67caa1"


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
.history {
    display: inline-block;
    border: 1px solid """ ++ greenDark ++ """;
    margin: 1px 5px;
    padding: 1px 5px;
}

.navigation a {
  display: inline-block;
  margin: 10px;
  padding: 10px;
}
.navigation .selected {
  background-color: """ ++ greenBright ++ """;
}
h1 {
  color: #67caa1;
}
.dante::first-letter {
    font-size: 6em;
    color: """ ++ greenDark ++ """;
}
pre {
    font-family: serif
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
