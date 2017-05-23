module Main exposing (..)

import Http
import Html exposing (Html, program, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode

main : Program Never Model Msg
main =
  program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }

init : (Model, Cmd Msg)
init = ({
  counter = 0,
  person = defaultPerson,
  personQueryStatus = Parked
  }, Cmd.none)

subscriptions : Model -> Sub msg
subscriptions model =
  Sub.none

type HttpQueryStatus result
  = HttpSuccess result
  | HttpError String
  | AwaitingResponse
  | Parked

type alias Model = {
  counter: Int,
  person: Person,
  personQueryStatus: HttpQueryStatus Person
}

type alias Person = {
  name: String,
  height: Float,
  mass: Float
}

defaultPerson : Person
defaultPerson = {
  name = "default-name",
  height = 0,
  mass = 0
  }

model : Model
model =
  { counter = 0
  , person = defaultPerson
  , personQueryStatus = Parked
  }

-- whyIsThisNotBuiltIn : Decode.Decoder Float
-- whyIsThisNotBuiltIn =
--   Decode.map String.toFloat Decode.float

-- Elm doesn't have a built in means (or the documentation is super hazy to me)
-- of coercing values as part of deserializing. So we'll just introduce an
-- intermediate type and coerce from a conversion function.
type alias KindOfPerson = {
  name: String,
  height: String,
  mass: String
}

kindOfPersonToPerson : (KindOfPerson -> Result String Person)
kindOfPersonToPerson kindOfPerson =
  case String.toFloat kindOfPerson.height of
      Ok height ->
        case String.toFloat kindOfPerson.mass of
            Ok mass ->
              Ok {
                name = kindOfPerson.name,
                height = height,
                mass = mass
              }
            Err _ -> Err "mass is not a float"
      Err _ -> Err "height is not a float"

decodePerson : Decode.Decoder KindOfPerson
decodePerson =
  Decode.map3 KindOfPerson
    (Decode.field "name" Decode.string)
    (Decode.field "height" Decode.string)
    (Decode.field "mass" Decode.string)
    -- (Decode.decodeString (Decode.maybe (Decode.field "height" Decode.string)))
    -- (Decode.decodeString (Decode.maybe (Decode.field "mass" Decode.string)))
    -- (Decode.field "height" (Decode.decodeString Decode.float))
  -- object3 Person
    -- ("name" := string)
    -- ("height" := float)
    -- ("mass" := float)
  -- Decode.map3 Person
  --   (Decode.field "name" Decode.string)
  --   (Decode.field "height" Decode.maybe Decode.float)
  --   (Decode.field "mass" Decode.string)

getLuke : Int -> Http.Request KindOfPerson
getLuke id =
  Http.get ("http://swapi.co/api/people/" ++ toString id) decodePerson

type Msg =
  Increment |
  Decrement |
  Reset |
  GetLuke |
  GotLuke (Result Http.Error KindOfPerson)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment ->
          ( { model | counter = model.counter + 1 }, Cmd.none )
        Decrement ->
          ( { model | counter = model.counter - 1 }, Cmd.none )
        Reset ->
          ( { model | counter = 0 }, Cmd.none )
        GetLuke ->
          (
            { model | personQueryStatus = AwaitingResponse },
            Http.send GotLuke (getLuke 1)
          )
        GotLuke (Ok kindOfPerson) ->
          case kindOfPersonToPerson kindOfPerson of
              Ok person -> ( { model | person = person, personQueryStatus = HttpSuccess person }, Cmd.none)
              Err e -> ( { model | personQueryStatus = HttpError e }, Cmd.none)
        GotLuke (Err err) ->
          ( { model | personQueryStatus = HttpError (toString err) }, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (toString model.counter) ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Reset ] [ text "reset" ]
        , div [] [ text (toString model.personQueryStatus )]
        , button [ onClick GetLuke ] [ text "get luke" ]
        ]
