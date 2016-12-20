import Html exposing (Html, h1, button, div, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main =
        Html.beginnerProgram { model = model, view = view, update = update }


type Msg = Roll


type alias Model = 
    { dieFace : Int
    }


model : Model
model =
    Model 1


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Roll ->
            (model, Cmd.none)


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.dieFace) ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]


init : (Model, Cmd Msg)
init =
    (Model 1, Cmd.none)
