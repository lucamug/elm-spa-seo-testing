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
import Time.DateTime


port urlChange : String -> Cmd msg


port titleChanged : (String -> msg) -> Sub msg


type Msg
    = ChangeLocation String
    | UrlChange Navigation.Location
    | NewApi1Data (Result Http.Error Api1Data)
    | NewApi2Data (Result Http.Error Api2Data)
    | FetchApi1Data String
    | FetchApi2Data String
    | AddTimeToModel Time.Time
    | Tick Time.Time
    | OnTitleChanged String


type alias Model =
    { route : Route
    , history : List String
    , titleHistory : List String
    , api1Data : String
    , api2Data : String
    , location : Navigation.Location
    , version : String
    , initialTime : Time.Time
    , presentTime : Time.Time
    , title : String
    }


type Route
    = Top
    | Section1
    | Section2
    | Section3
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
        , UrlParser.map Section1 (UrlParser.s "section1")
        , UrlParser.map Section2 (UrlParser.s "section2")
        , UrlParser.map Section3 (UrlParser.s "section3")
        , UrlParser.map Sitemap (UrlParser.s "sitemap")
        ]


routeToPath : Route -> String
routeToPath route =
    case route of
        Top ->
            ""

        Section1 ->
            "section1"

        Section2 ->
            "section2"

        Section3 ->
            "section3"

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


updateTitleAndMetaDescription : Model -> Cmd msg
updateTitleAndMetaDescription model =
    urlChange (titleForJs model)


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
                , updateTitleAndMetaDescription newModel
                )

        NewApi1Data result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model | api1Data = data.url }
                    in
                        ( newModel
                        , updateTitleAndMetaDescription newModel
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
                        , updateTitleAndMetaDescription newModel
                        )

                Err data ->
                    ( model, Cmd.none )

        FetchApi2Data url ->
            ( model, Http.send NewApi2Data (Http.get url api2Decoder) )

        AddTimeToModel time ->
            let
                newModel =
                    { model | initialTime = time }
            in
                ( newModel, updateTitleAndMetaDescription newModel )

        OnTitleChanged title ->
            let
                newTitleHistory =
                    title :: model.titleHistory
            in
                ( { model | titleHistory = newTitleHistory }, Cmd.none )

        Tick newTime ->
            let
                newModel =
                    { model | presentTime = newTime }
            in
                ( newModel, updateTitleAndMetaDescription newModel )


onLinkClick : String -> Attribute Msg
onLinkClick path =
    onWithOptions "click"
        { stopPropagation = False
        , preventDefault = True
        }
        (Decode.succeed (ChangeLocation path))


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ node "style" [] [ text css ]
        , h1 [] [ text model.title ]
        , viewNavigation model
        , viewMetadata model
        , viewPage model
        ]


viewMetadata : Model -> Html msg
viewMetadata model =
    div [ id "metadata" ]
        [ p [ class "highlight" ] [ text ("Title: " ++ (titleForJs model)) ]
        , p [] [ text "V = Version, H = History length, A = Type A Ajax, B = Type B Ajax" ]
        , p []
            [ text "History: "
            , span []
                (List.map (\item -> span [ class "history" ] [ text item ])
                    (List.reverse (model.history))
                )
            ]
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
            []
            [ if model.route == route then
                div [ class "selected" ] [ text (pathToName path) ]
              else
                a [ href url, onLinkClick url ] [ text (pathToName path) ]
            ]


viewNavigation : Model -> Html Msg
viewNavigation model =
    ul [ class "navigation" ]
        [ viewLink model "" Top
        , viewLink model "section1" Section1
        , viewLink model "section2" Section2
        , viewLink model "section3" Section3
        , viewLink model "sitemap" Sitemap
        ]


viewPage : Model -> Html Msg
viewPage model =
    div []
        [ h2 []
            [ model.route
                |> routeToPath
                |> pathToName
                |> text
            ]
        , pre [ class "dante" ]
            [ case model.route of
                Section1 ->
                    text section1

                Section2 ->
                    text section2

                Section3 ->
                    text section3

                Top ->
                    viewTop

                Sitemap ->
                    textarea []
                        [ text
                            ((routeToSurgeUrl Top)
                                ++ "\n"
                                ++ (routeToSurgeUrl Section1)
                                ++ "\n"
                                ++ (routeToSurgeUrl Section2)
                                ++ "\n"
                                ++ (routeToSurgeUrl Section3)
                                ++ "\n"
                                ++ (routeToSurgeUrl Sitemap)
                            )
                        ]

                NotFound ->
                    text "Page not Found"
            ]
        ]


routeToSurgeUrl : Route -> String
routeToSurgeUrl route =
    "http://elm-spa-seo-testing.guupa.com/" ++ (routeToPath route)


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
    , titleHistory = []
    , api1Data = ""
    , api2Data = ""
    , location = location
    , version = "9"
    , initialTime = 0
    , presentTime = 0
    , title = "SPA and SEO Testing"
    }


titleForJs : Model -> String
titleForJs model =
    let
        num1 =
            extractNumber model.api1Data

        num2 =
            extractNumber model.api2Data

        historyLength =
            toString (List.length model.history)

        time =
            if model.presentTime > 0 then
                toString (round ((model.presentTime - model.initialTime) / 1000))
            else
                "0"
    in
        model.title
            ++ " - "
            ++ "V"
            ++ model.version
            ++ ",T"
            ++ time
            ++ ",H"
            ++ historyLength
            ++ ",A"
            ++ num1
            ++ ",B"
            ++ num2
            ++ ","
            ++ Time.DateTime.toISO8601 (Time.DateTime.fromTimestamp model.initialTime)
            ++ ","
            ++ model.location.pathname


initCmd : Model -> Navigation.Location -> Cmd Msg
initCmd model location =
    Cmd.batch
        [ Task.perform AddTimeToModel Time.now
        , Task.perform (\_ -> FetchApi1Data "10.json") (Process.sleep (10.0 * Time.second))
        , Task.perform (\_ -> FetchApi1Data "6.json") (Process.sleep (6.0 * Time.second))
        , Task.perform (\_ -> FetchApi1Data "3.json") (Process.sleep (3.0 * Time.second))
        , Task.perform (\_ -> FetchApi1Data "1.json") (Process.sleep (1.0 * Time.second))
        , Task.perform (\_ -> FetchApi1Data "0.json") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/10") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/6") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/3") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/1") (Process.sleep (0.0 * Time.second))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/0") (Process.sleep (0.0 * Time.second))
        ]


extractNumber : String -> String
extractNumber text =
    let
        number =
            Regex.find Regex.All (Regex.regex "\\d{1,2}") text
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
    Sub.batch
        [ titleChanged OnTitleChanged
        , Time.every Time.second Tick
        ]


mainBrightColor : String
mainBrightColor =
    "#7effca"


mainDarkColor : String
mainDarkColor =
    "#4c9275"


highlightColor : String
highlightColor =
    "#deff7e"


css : String
css =
    """body {
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
    background-color: """ ++ highlightColor ++ """;
    margin: 0 2px;
}

.navigation {
    padding: 0;
}
.navigation li {
    display: inline-block;
}
.navigation a, .navigation div {
    padding: 10px;
}
.navigation .selected {
    background-color: """ ++ mainBrightColor ++ """;
    color: black;
}
h2 {
    color: """ ++ mainDarkColor ++ """;
    margin-bottom: 2em;
}
h1 {
    color: """ ++ mainDarkColor ++ """;
    font-size: 1em;
    border-bottom: 2px solid """ ++ mainDarkColor ++ """;
}
a {
    text-decoration: none;
    color: """ ++ mainDarkColor ++ """;
}
a:hover {
    background-color: """ ++ mainBrightColor ++ """;
    color: black;
}
.dante::first-letter {
    font-size: 6em;
    color: """ ++ mainDarkColor ++ """;
    line-height: 30px;
}
pre {
    font-family: serif
}
.subAppHide .highlight{
    background-color: """ ++ highlightColor ++ """;
}
.subAppShow .highlight{
    transition: all 1000ms;
}
textarea {
    width: 100%;
    height: 80px;
}
#metadata {
    font-size: 2em;
    color: gray;
    font-family: monospace;
}
#metadata p {
    margin: 2px 0;
}
"""


viewTop : Html msg
viewTop =
    div []
        [ text """This is a test Single Page Application to
verify the Googlebot (and other Search Engine bots)
capability to execute Javascript and Ajax calls."""
        , ul []
            [ li []
                [ a [ href "https://medium.com/@l.mugnaini/spa-and-seo-is-googlebot-able-to-render-a-single-page-application-1f74e706ab11" ] [ text "Full Article" ]
                ]
            , li []
                [ a [ href "https://www.google.it/search?q=site:elm-spa-seo-testing.guupa.com" ] [ text "Search Result" ]
                ]
            , li []
                [ a [ href "https://github.com/lucamug/elm-spa-seo-testing" ] [ text "Code" ]
                ]
            ]
        ]


section1 : String
section1 =
    """Midway upon the journey of our life
I found myself within a forest dark,
For the straightforward pathway had been lost.

Ah me! how hard a thing it is to say
What was this forest savage, rough, and stern,
Which in the very thought renews the fear.

So bitter is it, death is little more;
But of the good to treat, which there I found,
Speak will I of the other things I saw there."""


section2 : String
section2 =
    """I cannot well repeat how there I entered,
So full was I of slumber at the moment
In which I had abandoned the true way.

But after I had reached a mountain's foot,
At that point where the valley terminated,
Which had with consternation pierced my heart,

Upward I looked, and I beheld its shoulders
Vested already with that planet's rays
Which leadeth others right by every road."""


section3 : String
section3 =
    """Then was the fear a little quieted
That in my heart's lake had endured throughout
The night, which I had passed so piteously

And even as he, who, with distressful breath,
Forth issued from the sea upon the shore,
Turns to the water perilous and gazes;

So did my soul, that still was fleeing onward,
Turn itself back to re-behold the pass
Which never yet a living person left."""


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
