module InteractiveMarkdown exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Element exposing (Element)
import Html exposing (Html)
import Markdown.Block as Markdown
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown exposing (defaultHtmlRenderer)
import Quiz


type alias Model =
    { quizzes : Dict String Quiz.Model
    }


type Msg
    = QuizMsg Quiz.Model String Quiz.Msg


init : Model
init =
    { quizzes = Dict.empty }


blockToQuiz : Markdown.Block -> Maybe ( String, Quiz.Model )
blockToQuiz block =
    case block of
        Markdown.HtmlBlock (Markdown.HtmlElement "quiz" [ { name, value } ] [ Markdown.UnorderedList options ]) ->
            case ( name, value ) of
                ( "id", id ) ->
                    Just
                        ( id
                        , options
                            |> List.filterMap listItemToQuizAnswer
                            |> Quiz.init
                        )

                _ ->
                    Nothing

        _ ->
            Nothing


listItemToQuizAnswer : Markdown.ListItem Markdown.Inline -> Maybe Quiz.Answer
listItemToQuizAnswer (Markdown.ListItem task children) =
    case task of
        Markdown.IncompleteTask ->
            Just ( False, Markdown.extractInlineText children )

        Markdown.CompletedTask ->
            Just ( True, Markdown.extractInlineText children )

        _ ->
            Nothing


view : String -> Result String (Model -> Element Msg)
view markdownBody =
    markdownBody
        |> Markdown.parse
        |> Result.mapError (List.map Markdown.deadEndToString >> String.join "\n")
        |> Result.map viewForParsed


viewForParsed : List Markdown.Block -> Model -> Element Msg
viewForParsed blocks =
    let
        initModel : Model
        initModel =
            { quizzes =
                blocks
                    |> List.filterMap blockToQuiz
                    |> List.foldl (\( id, quiz ) acc -> Dict.insert id quiz acc) Dict.empty
            }
    in
    \state ->
        let
            model : Model
            model =
                { initModel
                    | quizzes = Dict.union state.quizzes initModel.quizzes
                }

            renderer =
                rendererForModel model
        in
        case Markdown.render renderer blocks of
            Ok htmlElements ->
                htmlElements
                    |> List.map Element.html
                    |> Element.paragraph []

            Err error ->
                ("Error: " ++ error)
                    |> Element.text
                    |> List.singleton
                    |> Element.paragraph []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        QuizMsg initModel quizId quizMsg ->
            ( { model
                | quizzes =
                    Dict.update
                        quizId
                        (Maybe.withDefault initModel >> Quiz.update quizMsg >> Just)
                        model.quizzes
              }
            , Cmd.none
            )


rendererForModel : Model -> Markdown.Renderer (Html Msg)
rendererForModel { quizzes } =
    { defaultHtmlRenderer
        | html =
            Markdown.Html.oneOf
                [ Markdown.Html.tag "quiz"
                    (\id children ->
                        case Dict.get id quizzes of
                            Just quiz ->
                                quiz
                                    |> Quiz.view
                                    |> Html.map (QuizMsg quiz id)

                            Nothing ->
                                Html.div [] children
                    )
                    |> Markdown.Html.withAttribute "id"
                ]
    }
