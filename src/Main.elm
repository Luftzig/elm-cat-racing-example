module Main exposing (..)

import Html exposing (program, div, form, label, input, text, button, select, option)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode
import Json.Encode

main = program
    { view = view
    , update = update
    , subscriptions = subscriptions
    , init = (model, Cmd.none)
    }

-- MODEL

type alias Cat =
    { name : String
    , score : Maybe Int
    , id : Maybe String
    }

type Stateful a = NotChanged a
                | Changed a
                | Waiting a
                | Failed Http.Error a
                | Succeeded a

type alias Model =
    { cat : Stateful Cat }


getValue : Stateful a -> a
getValue stateful =
    case stateful of
        NotChanged v -> v
        Changed v -> v
        Waiting v -> v
        Failed error v -> v
        Succeeded v -> v


mapStateful : (a -> a) -> Stateful a -> Stateful a
mapStateful f stateful =
    case stateful of
        NotChanged v -> Changed <| f v
        Changed v -> Changed <| f v
        Waiting v -> Waiting v
        Failed error v -> Failed error v
        Succeeded v -> Changed <| f v


updateState : (a -> Stateful a) -> Stateful a -> Stateful a
updateState newState stateful =
    case stateful of
        NotChanged v -> newState v
        Changed v -> newState v
        Waiting v -> newState v
        Failed error v ->  newState v
        Succeeded v -> newState v


-- Helpers
setName : String -> Cat -> Cat
setName name cat = { cat | name = name }

model : Model
model =
    { cat = NotChanged { name = "", score = Nothing, id = Nothing } }

-- VIEW

view model =
    form [ onSubmit <| SaveChanges <| getValue model.cat ]
        [ div []
            [ label [] [text "Name"]
            , input [onInput ChangeName ] []
            ]
        , div []
            [ label [] [text "Score"]
            ]
        , div []
            [ button [ type_ "submit" ] [text "Update"]
            ]
        , div [] [text <| toString model.cat ]
        ]


-- UPDATE

noCmd : Model -> (Model, Cmd Message)
noCmd model = (model, Cmd.none)

type Message = ChangeName String
             | SaveChanges Cat
             | UpdateResponse (Result Http.Error Cat)

update : Message -> Model -> (Model, Cmd Message)
update msg model =
  case msg of
    ChangeName newName -> noCmd { model | cat = mapStateful (setName newName) model.cat }

    SaveChanges cat -> ({model | cat = updateState Waiting model.cat}, sendUpdateRequest cat)

    UpdateResponse (Err error) -> noCmd {model | cat = Failed error <| getValue model.cat }

    UpdateResponse (Ok newCat) -> noCmd { model | cat = Succeeded newCat }


sendUpdateRequest : Cat -> Cmd Message
sendUpdateRequest cat =
    Http.send UpdateResponse
        <| Http.post "http://localhost:3000/cats" (encodeCat cat) decodeResponse


encodeCat : Cat -> Http.Body
encodeCat cat =
    Http.jsonBody <|
        Json.Encode.object
            [ ("name", Json.Encode.string cat.name)
            , ("id", Maybe.withDefault Json.Encode.null <| Maybe.map Json.Encode.string <| cat.id )
            , ("score", Maybe.withDefault Json.Encode.null <| Maybe.map Json.Encode.int <| cat.score)
            ]


decodeResponse : Json.Decode.Decoder Cat
decodeResponse = Json.Decode.map3 Cat
    (Json.Decode.field "name" Json.Decode.string)
    (Json.Decode.field "score" <| Json.Decode.maybe Json.Decode.int)
    (Json.Decode.field "id" <| Json.Decode.maybe Json.Decode.string)


-- SUBSCRIPTIONS

subscriptions model = Sub.none
