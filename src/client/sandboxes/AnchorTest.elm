module AnchorTest exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key, load, pushUrl)
import Html.Styled exposing (Attribute, Html, br, div, text, toUnstyled)
import Html.Styled.Attributes exposing (href)
import Html.Styled.Events exposing (onClick)
import RouteUrl exposing (HistoryEntry(..), UrlChange(..))
import String exposing (toInt)
import Url exposing (Url)
import Utils.ListUtils as ListUtils
import Utils.LocationUtils exposing (changeToString)


type alias AnchorTestModel =
    { key : Key
    , num : Int
    }


type AnchorTestMsg
    = Incr
    | Decr
    | Set Int
    | UrlRequested UrlRequest



--


aForUpdateDelta : (msg -> model -> ( model, Cmd msg )) -> (model -> model -> Maybe UrlChange) -> model -> msg -> List (Attribute msg) -> List (Html msg) -> Html msg
aForUpdateDelta updateFn delta2urlFn oldModel msg attrs elts =
    let
        ( newModel, _ ) =
            updateFn msg oldModel

        urlChange =
            delta2urlFn oldModel newModel
    in
    case urlChange of
        Nothing ->
            Html.Styled.a (href "#" :: onClick msg :: attrs) elts

        Just change ->
            Html.Styled.a ((href <| changeToString change) :: attrs) elts


handleUrlRequest model urlReq =
    case urlReq of
        Internal url ->
            ( model, pushUrl model.key <| Url.toString url )

        External urlString ->
            ( model, load urlString )



--


main =
    RouteUrl.programWithFlags
        { delta2url = delta2url --: model -> model -> Maybe UrlChange
        , location2messages = location2messages --: Url -> List msg
        , init = init --\_ -> \key -> ( { key = key, num = 0 }, Cmd.none ) --: flags -> Key -> ( model, Cmd msg )
        , update = update --: msg -> model -> ( model, Cmd msg )
        , subscriptions = \_ -> Sub.none --: model -> Sub msg
        , view = view --: model -> Document msg
        , onUrlRequest = UrlRequested -- onUrlRequest --: UrlRequest -> msg
        }


aFromModel =
    aForUpdateDelta update delta2url


init : () -> Key -> ( AnchorTestModel, Cmd AnchorTestMsg )
init _ key =
    ( { key = key, num = 0 }, Cmd.none )


update : AnchorTestMsg -> AnchorTestModel -> ( AnchorTestModel, Cmd AnchorTestMsg )
update msg model =
    case msg of
        UrlRequested urlReq ->
            handleUrlRequest model urlReq

        Incr ->
            ( { model | num = model.num + 1 }, Cmd.none )

        Decr ->
            ( { model | num = model.num - 1 }, Cmd.none )

        Set i ->
            ( { model | num = i }, Cmd.none )


view : AnchorTestModel -> Document AnchorTestMsg
view model =
    { title = "title", body = [ toUnstyled <| viewImpl model ] }


viewImpl model =
    let
        a =
            aFromModel model
    in
    div [] <|
        List.intersperse (br [] [])
            [ text <| "current value: " ++ String.fromInt model.num
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
