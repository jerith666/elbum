module Sandbox.AnchorTest exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key, load, pushUrl)
import Html.Styled exposing (Attribute, Html, a, br, div, text, toUnstyled)
import Html.Styled.Attributes exposing (href)
import Html.Styled.Events exposing (onClick)
import RouteUrl exposing (AppWithFlags, HistoryEntry(..), RouteUrlProgram, UrlChange(..), WrappedModel(..), unwrapModel)
import String exposing (toInt)
import Url exposing (Url)
import Utils.ListUtils as ListUtils
import Utils.LocationUtils exposing (changeToString)


type alias AnchorTestModel =
    { key : Key
    , num : Int
    , urlsRequested : Int
    , setMessages : Int
    , incDecMessages : Int
    }


type AnchorTestMsg
    = Incr
    | Decr
    | Set Int
    | UrlRequested UrlRequest



--


type alias AnchorManagedAppWithFlags model msg flags =
    { delta2url : model -> model -> Maybe UrlChange
    , location2messages : Url -> List msg
    , init : flags -> Key -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> (msg -> List (Attribute msg) -> List (Html msg) -> Html msg) -> Document msg
    , onUrlRequest : UrlRequest -> msg
    }


handleUrlRequest : model -> Key -> UrlRequest -> ( model, Cmd msg )
handleUrlRequest model key urlReq =
    case urlReq of
        Internal url ->
            ( model, pushUrl key <| Url.toString url )

        External urlString ->
            ( model, load urlString )



--


main =
    RouteUrl.anchorManagedProgramWithFlags
        { delta2url = delta2url --: model -> model -> Maybe UrlChange
        , location2messages = location2messages --: Url -> List msg
        , init = init --\_ -> \key -> ( { key = key, num = 0 }, Cmd.none ) --: flags -> Key -> ( model, Cmd msg )
        , update = update --: msg -> model -> ( model, Cmd msg )
        , subscriptions = \_ -> Sub.none --: model -> Sub msg
        , view = view --: model -> Document msg
        , onUrlRequest = UrlRequested -- onUrlRequest --: UrlRequest -> msg
        , makeAnchor = makeAnchor
        }


makeAnchor : String -> Maybe AnchorTestMsg -> List (Attribute AnchorTestMsg) -> List (Html AnchorTestMsg) -> Html AnchorTestMsg
makeAnchor url onClickMsg =
    case onClickMsg of
        Nothing ->
            \attrs -> a (href url :: attrs)

        Just m ->
            \attrs -> a (href url :: onClick m :: attrs)


init : () -> Key -> ( AnchorTestModel, Cmd AnchorTestMsg )
init _ key =
    ( { key = key, num = 0, urlsRequested = 0, setMessages = 0, incDecMessages = 0 }, Cmd.none )


update : AnchorTestMsg -> AnchorTestModel -> ( AnchorTestModel, Cmd AnchorTestMsg )
update msg model =
    case msg of
        UrlRequested urlReq ->
            handleUrlRequest { model | urlsRequested = model.urlsRequested + 1 } model.key urlReq

        Incr ->
            ( { model | num = model.num + 1, incDecMessages = model.incDecMessages + 1 }, Cmd.none )

        Decr ->
            ( { model | num = model.num - 1, incDecMessages = model.incDecMessages + 1 }, Cmd.none )

        Set i ->
            ( { model | num = i, setMessages = model.setMessages + 1 }, Cmd.none )


view : AnchorTestModel -> (AnchorTestMsg -> List (Attribute AnchorTestMsg) -> List (Html AnchorTestMsg) -> Html AnchorTestMsg) -> Document AnchorTestMsg
view model a =
    { title = "title", body = [ toUnstyled <| viewImpl model a ] }


viewImpl model a =
    div [] <|
        List.intersperse (br [] [])
            [ text <| "current value: " ++ String.fromInt model.num
            , br [] []
            , text <| "urls requested: " ++ String.fromInt model.urlsRequested
            , br [] []
            , text <| "increment / decrement messages: " ++ String.fromInt model.incDecMessages
            , br [] []
            , text <| "set messages: " ++ String.fromInt model.setMessages
            , br [] []
            , a Incr [] [ text "increment" ]
            , a Decr [] [ text "decrement" ]
            , a (Set 0) [] [ text "reset" ]
            ]


delta2url : AnchorTestModel -> AnchorTestModel -> Maybe UrlChange
delta2url oldModel newModel =
    case oldModel.num == newModel.num of
        True ->
            Nothing

        False ->
            Just <| NewFragment { entry = NewEntry, key = newModel.key } <| String.fromInt newModel.num


location2messages : Url -> List AnchorTestMsg
location2messages url =
    url.fragment
        |> Maybe.andThen toInt
        |> Maybe.map Set
        |> ListUtils.fromMaybe
