import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Regex exposing (regex)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL

type alias Error = 
    { color : String
    , message : String
    }

type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , errorMessage : Error
    }


model : Model
model =
    Model "" "" "" { color = "", message = "" } 


-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Submit



update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name , errorMessage = { color = "", message = "" } }

        Password password ->
            { model | password = password, errorMessage = { color = "", message = "" } }

        PasswordAgain password ->
            { model | passwordAgain = password, errorMessage = { color = "", message = "" } }

        Submit ->
            { model | errorMessage = validate model }



validate : Model -> Error
validate model =
    let (color, message) =
            if model.password /= model.passwordAgain then
                ("red", "Passwords do not match!")
            else if String.length model.password < 8 then
                ("red", "Password must be longer than 8 characters!")
            else if not ( Regex.contains (regex "[0-9]|[a-z]|[A-Z]") model.password ) then
                ("red", "Password must contain upper case, lower case, and numeric characters.")
            else
                ("green", "Ok")
    in
        { color = color , message = message }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "Name", onInput Name ] []
    , input [ type_ "password", placeholder "Password", onInput Password ] []
    , input [ type_ "password", placeholder "Re-enter Password", onInput PasswordAgain ] []
    , input [ type_ "button", onClick Submit, value "Submit" ] []
    , if String.length model.errorMessage.message > 0 then viewValidation model else div [][]
    ]


viewValidation : Model -> Html msg
viewValidation model =
    let
        (color, message) =
            if model.password /= model.passwordAgain then
                ("red", "Passwords do not match!")
            else if String.length model.password < 8 then
                ("red", "Password must be longer than 8 characters!")
            else if not ( Regex.contains (regex "[0-9]|[a-z]|[A-Z]") model.password ) then
                ("red", "Password must contain upper case, lower case, and numeric characters.")
            else
                ("green", "OK")
    in
        div [ style [("color", color)] ] [ text message ] 
