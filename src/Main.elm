port module Main exposing (main)

import Browser
import Browser.Navigation
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Iso8601
import Json.Decode as Decode
import Process
import Regex
import Task
import Time
import Url
import Url.Parser


port urlChange : String -> Cmd msg


port titleChanged : (String -> msg) -> Sub msg


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NewApi1Data (Result Http.Error Api1Data)
    | NewApi2Data (Result Http.Error Api2Data)
    | FetchApi1Data String
    | FetchApi2Data String
    | AddTimeToModel Time.Posix
    | Tick Time.Posix
    | OnTitleChanged String


type alias Model =
    { route : Route
    , key : Browser.Navigation.Key
    , history : List String
    , titleHistory : List String
    , api1Data : String
    , api2Data : String
    , location : Url.Url
    , version : String
    , initialTime : Time.Posix
    , presentTime : Time.Posix
    , title : String
    , flags : Flags
    , origin : String
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


matchers : Url.Parser.Parser (Route -> a) a
matchers =
    Url.Parser.oneOf
        [ Url.Parser.map Top Url.Parser.top
        , Url.Parser.map Section1 (Url.Parser.s "section1")
        , Url.Parser.map Section2 (Url.Parser.s "section2")
        , Url.Parser.map Section3 (Url.Parser.s "section3")
        , Url.Parser.map Sitemap (Url.Parser.s "sitemap")
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


locationToRoute : Url.Url -> Route
locationToRoute location =
    case Url.Parser.parse matchers location of
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
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Browser.Navigation.load href
                    )

        UrlChanged location ->
            let
                newRoute =
                    locationToRoute location

                newHistory =
                    location.path :: model.history

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
                            { model
                                | api1Data = data.url
                            }
                    in
                    ( newModel
                    , updateTitleAndMetaDescription newModel
                    )

                Err data ->
                    ( model, Cmd.none )

        FetchApi1Data url ->
            ( model, Http.get { url = url, expect = Http.expectJson NewApi1Data api1Decoder } )

        NewApi2Data result ->
            case result of
                Ok data ->
                    let
                        newModel =
                            { model
                                | api2Data = data.url
                                , origin = data.origin
                            }
                    in
                    ( newModel
                    , updateTitleAndMetaDescription newModel
                    )

                Err data ->
                    ( model, Cmd.none )

        FetchApi2Data url ->
            ( model, Http.get { url = url, expect = Http.expectJson NewApi2Data api2Decoder } )

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



-- onLinkClick : String -> Attribute Msg
-- onLinkClick path =
--     onWithOptions "click"
--         { stopPropagation = False
--         , preventDefault = True
--         }
--         (Decode.succeed (ChangeLocation path))


view : Model -> Browser.Document Msg
view model =
    { title = model.title
    , body =
        [ div [ id "app" ]
            [ node "style" [] [ text css ]
            , h1 [] [ text model.title ]
            , viewNavigation model
            , viewMetadata model
            , p [] [ text model.flags.ua ]
            , p [] [ text model.flags.commit ]
            , p [] [ text model.origin ]
            , viewPage model
            ]
        ]
    }


viewMetadata : Model -> Html msg
viewMetadata model =
    div [ id "metadata" ]
        [ p [ class "highlight" ] [ text ("Title: " ++ titleForJs model) ]
        , p [] [ text "V = Version, H = History length, A = Type A Ajax, B = Type B Ajax" ]
        , p []
            [ text "History: "
            , span []
                (List.map (\item -> span [ class "history" ] [ text item ])
                    (List.reverse model.history)
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
            a [ href url ] [ text (pathToName path) ]
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
                            (routeToSurgeUrl Top
                                ++ "\n"
                                ++ routeToSurgeUrl Section1
                                ++ "\n"
                                ++ routeToSurgeUrl Section2
                                ++ "\n"
                                ++ routeToSurgeUrl Section3
                                ++ "\n"
                                ++ routeToSurgeUrl Sitemap
                            )
                        ]

                NotFound ->
                    text "Page not Found"
            ]
        ]


routeToSurgeUrl : Route -> String
routeToSurgeUrl route =
    "https://elm-spa-seo-testing.guupa.com/" ++ routeToPath route


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags location key =
    let
        model =
            initModel flags location key
    in
    ( model
    , initCmd model location
    )


initModel : Flags -> Url.Url -> Browser.Navigation.Key -> Model
initModel flags location key =
    { route = locationToRoute location
    , key = key
    , history = [ location.path ]
    , titleHistory = []
    , api1Data = ""
    , api2Data = ""
    , location = location
    , version = "9"
    , initialTime = Time.millisToPosix 0
    , presentTime = Time.millisToPosix 0
    , title = "[" ++ flags.commit ++ "] "
    , flags = flags
    , origin = "N/A"
    }


titleForJs : Model -> String
titleForJs model =
    let
        num1 =
            extractNumber model.api1Data

        num2 =
            extractNumber model.api2Data

        historyLength =
            String.fromInt (List.length model.history)

        time =
            if Time.posixToMillis model.presentTime > 0 then
                String.fromInt ((Time.posixToMillis model.presentTime - Time.posixToMillis model.initialTime) // 1000)

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
        ++ Iso8601.fromTime model.initialTime
        ++ ","
        ++ model.location.path


initCmd : Model -> Url.Url -> Cmd Msg
initCmd model location =
    Cmd.batch
        [ Task.perform AddTimeToModel Time.now
        , Task.perform (\_ -> FetchApi1Data "10.json") (Process.sleep (10.0 * 1000))
        , Task.perform (\_ -> FetchApi1Data "6.json") (Process.sleep (6.0 * 1000))
        , Task.perform (\_ -> FetchApi1Data "3.json") (Process.sleep (3.0 * 1000))
        , Task.perform (\_ -> FetchApi1Data "1.json") (Process.sleep (1.0 * 1000))
        , Task.perform (\_ -> FetchApi1Data "0.json") (Process.sleep (0.0 * 1000))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/10") (Process.sleep (0.0 * 1000))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/6") (Process.sleep (0.0 * 1000))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/3") (Process.sleep (0.0 * 1000))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/1") (Process.sleep (0.0 * 1000))
        , Task.perform (\_ -> FetchApi2Data "https://httpbin.org/delay/0") (Process.sleep (0.0 * 1000))
        ]


extractNumber : String -> String
extractNumber text =
    let
        number =
            Regex.find (Maybe.withDefault Regex.never (Regex.fromString "\\d{1,2}")) text
    in
    case List.head number of
        Nothing ->
            "[NaN]"

        Just data ->
            data.match


type alias Api1Data =
    { url : String
    }


type alias Api2Data =
    { url : String
    , origin : String
    }


api1Decoder : Decode.Decoder Api1Data
api1Decoder =
    Decode.map Api1Data
        (Decode.at [ "url" ] Decode.string)


api2Decoder : Decode.Decoder Api2Data
api2Decoder =
    Decode.map2 Api2Data
        (Decode.at [ "url" ] Decode.string)
        (Decode.at [ "origin" ] Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ titleChanged OnTitleChanged
        , Time.every 1000 Tick
        ]


mainBrightColor : String
mainBrightColor =
    "#7effca"


mainDarkColor : String
mainDarkColor =
    "black"


highlightColor : String
highlightColor =
    "#deff7e"


css : String
css =
    """body {
    margin: 10px;
    font-family: sans-serif;
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


type alias Flags =
    { commit : String
    , ua : String
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }
