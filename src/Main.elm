module Main exposing (main)

import Color
import Data.Author as Author
import Date
import DocumentSvg
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Region
import Feed
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Attributes as Attr
import Index
import InteractiveMarkdown
import Json.Decode
import Layout
import Metadata exposing (Metadata)
import MySitemap
import Page.Article
import Pages exposing (images, pages)
import Pages.Directory as Directory exposing (Directory)
import Pages.Document
import Pages.ImagePath as ImagePath exposing (ImagePath)
import Pages.Manifest as Manifest
import Pages.Manifest.Category
import Pages.PagePath as PagePath exposing (PagePath)
import Pages.Platform exposing (Page)
import Pages.StaticHttp as StaticHttp
import Palette


manifest : Manifest.Config Pages.PathKey
manifest =
    { backgroundColor = Just Color.white
    , categories = [ Pages.Manifest.Category.education ]
    , displayMode = Manifest.Standalone
    , orientation = Manifest.Portrait
    , description = "elm-pages-starter - A statically typed site generator."
    , iarcRatingId = Nothing
    , name = "elm-pages-starter"
    , themeColor = Just Color.white
    , startUrl = pages.index
    , shortName = Just "elm-pages-starter"
    , sourceIcon = images.iconPng
    }


type alias Rendered =
    Model -> Element Msg



-- the intellij-elm plugin doesn't support type aliases for Programs so we need to use this line
-- main : Platform.Program Pages.Platform.Flags (Pages.Platform.Model Model Msg Metadata Rendered) (Pages.Platform.Msg Msg Metadata Rendered)


main : Pages.Platform.Program Model Msg Metadata Rendered
main =
    Pages.Platform.application
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , documents = [ markdownDocument ]
        , manifest = manifest
        , canonicalSiteUrl = canonicalSiteUrl
        , onPageChange = \_ -> PageChanged
        , generateFiles = generateFiles
        , internals = Pages.internals
        }


generateFiles :
    List
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        , body : String
        }
    ->
        List
            (Result String
                { path : List String
                , content : String
                }
            )
generateFiles siteMetadata =
    [ Feed.fileToGenerate { siteTagline = siteTagline, siteUrl = canonicalSiteUrl } siteMetadata |> Ok
    , MySitemap.build { siteUrl = canonicalSiteUrl } siteMetadata |> Ok
    ]


markdownDocument : ( String, Pages.Document.DocumentHandler Metadata Rendered )
markdownDocument =
    Pages.Document.parser
        { extension = "md"
        , metadata = Metadata.decoder
        , body =
            InteractiveMarkdown.view
                >> Result.map (\viewForModel model -> viewForModel model |> Element.map InteractiveMarkdownMsg)
        }


type alias Model =
    InteractiveMarkdown.Model


init : ( Model, Cmd Msg )
init =
    ( InteractiveMarkdown.init, Cmd.none )


type Msg
    = PageChanged
    | InteractiveMarkdownMsg InteractiveMarkdown.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PageChanged ->
            -- Reset interactive markdown model
            ( InteractiveMarkdown.init, Cmd.none )

        InteractiveMarkdownMsg mdMsg ->
            InteractiveMarkdown.update mdMsg model
                |> Tuple.mapSecond (Cmd.map InteractiveMarkdownMsg)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view :
    List ( PagePath Pages.PathKey, Metadata )
    ->
        { path : PagePath Pages.PathKey
        , frontmatter : Metadata
        }
    ->
        StaticHttp.Request
            { view : Model -> Rendered -> { title : String, body : Html Msg }
            , head : List (Head.Tag Pages.PathKey)
            }
view siteMetadata page =
    StaticHttp.succeed
        { view =
            \model viewForPage ->
                Layout.view (pageView model siteMetadata page viewForPage) page
        , head = head page.frontmatter
        }


pageView :
    Model
    -> List ( PagePath Pages.PathKey, Metadata )
    -> { path : PagePath Pages.PathKey, frontmatter : Metadata }
    -> Rendered
    -> { title : String, body : List (Element Msg) }
pageView model siteMetadata page viewForPage =
    case page.frontmatter of
        Metadata.Page metadata ->
            { title = metadata.title
            , body =
                [ viewForPage model
                ]

            --        |> Element.textColumn
            --            [ Element.width Element.fill
            --            ]
            }

        Metadata.Article metadata ->
            Page.Article.view metadata (viewForPage model)

        Metadata.Author author ->
            { title = author.name
            , body =
                [ Palette.blogHeading author.name
                , Author.view [] author
                , Element.paragraph [ Element.centerX, Font.center ] [ viewForPage model ]
                ]
            }

        Metadata.BlogIndex ->
            { title = "elm-pages blog"
            , body =
                [ Element.column [ Element.padding 20, Element.centerX ] [ Index.view siteMetadata ]
                ]
            }


articleImageView : ImagePath Pages.PathKey -> Element msg
articleImageView articleImage =
    Element.image [ Element.width Element.fill ]
        { src = ImagePath.toString articleImage
        , description = "Article cover photo"
        }


header : PagePath Pages.PathKey -> Element msg
header currentPath =
    Element.column [ Element.width Element.fill ]
        [ Element.el
            [ Element.height (Element.px 4)
            , Element.width Element.fill
            , Element.Background.gradient
                { angle = 0.2
                , steps =
                    [ Element.rgb255 0 242 96
                    , Element.rgb255 5 117 230
                    ]
                }
            ]
            Element.none
        , Element.row
            [ Element.paddingXY 25 4
            , Element.spaceEvenly
            , Element.width Element.fill
            , Element.Region.navigation
            , Element.Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }
            , Element.Border.color (Element.rgba255 40 80 40 0.4)
            ]
            [ Element.link []
                { url = "/"
                , label =
                    Element.row [ Font.size 30, Element.spacing 16 ]
                        [ DocumentSvg.view
                        , Element.text "elm-pages-starter"
                        ]
                }
            , Element.row [ Element.spacing 15 ]
                [ elmDocsLink
                , githubRepoLink
                , highlightableLink currentPath pages.blog.directory "Blog"
                ]
            ]
        ]


highlightableLink :
    PagePath Pages.PathKey
    -> Directory Pages.PathKey Directory.WithIndex
    -> String
    -> Element msg
highlightableLink currentPath linkDirectory displayName =
    let
        isHighlighted =
            currentPath |> Directory.includes linkDirectory
    in
    Element.link
        (if isHighlighted then
            [ Font.underline
            , Font.color Palette.color.primary
            ]

         else
            []
        )
        { url = linkDirectory |> Directory.indexPath |> PagePath.toString
        , label = Element.text displayName
        }


commonHeadTags : List (Head.Tag Pages.PathKey)
commonHeadTags =
    [ Head.rssLink "/blog/feed.xml"
    , Head.sitemapLink "/sitemap.xml"
    ]


{-| <https://developer.twitter.com/en/docs/tweets/optimize-with-cards/overview/abouts-cards>
<https://htmlhead.dev>
<https://html.spec.whatwg.org/multipage/semantics.html#standard-metadata-names>
<https://ogp.me/>
-}
head : Metadata -> List (Head.Tag Pages.PathKey)
head metadata =
    commonHeadTags
        ++ (case metadata of
                Metadata.Page meta ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages-starter"
                        , image =
                            { url = images.iconPng
                            , alt = "elm-pages logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = meta.title
                        }
                        |> Seo.website

                Metadata.Article meta ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages starter"
                        , image =
                            { url = meta.image
                            , alt = meta.description
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = meta.description
                        , locale = Nothing
                        , title = meta.title
                        }
                        |> Seo.article
                            { tags = []
                            , section = Nothing
                            , publishedTime = Just (Date.toIsoString meta.published)
                            , modifiedTime = Nothing
                            , expirationTime = Nothing
                            }

                Metadata.Author meta ->
                    let
                        ( firstName, lastName ) =
                            case meta.name |> String.split " " of
                                [ first, last ] ->
                                    ( first, last )

                                [ first, middle, last ] ->
                                    ( first ++ " " ++ middle, last )

                                [] ->
                                    ( "", "" )

                                _ ->
                                    ( meta.name, "" )
                    in
                    Seo.summary
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages-starter"
                        , image =
                            { url = meta.avatar
                            , alt = meta.name ++ "'s elm-pages articles."
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = meta.bio
                        , locale = Nothing
                        , title = meta.name ++ "'s elm-pages articles."
                        }
                        |> Seo.profile
                            { firstName = firstName
                            , lastName = lastName
                            , username = Nothing
                            }

                Metadata.BlogIndex ->
                    Seo.summaryLarge
                        { canonicalUrlOverride = Nothing
                        , siteName = "elm-pages"
                        , image =
                            { url = images.iconPng
                            , alt = "elm-pages logo"
                            , dimensions = Nothing
                            , mimeType = Nothing
                            }
                        , description = siteTagline
                        , locale = Nothing
                        , title = "elm-pages blog"
                        }
                        |> Seo.website
           )


canonicalSiteUrl : String
canonicalSiteUrl =
    "https://elm-pages-starter.netlify.com"


siteTagline : String
siteTagline =
    "Starter blog for elm-pages"


publishedDateView metadata =
    Element.text
        (metadata.published
            |> Date.format "MMMM ddd, yyyy"
        )


githubRepoLink : Element msg
githubRepoLink =
    Element.newTabLink []
        { url = "https://github.com/dillonkearns/elm-pages"
        , label =
            Element.image
                [ Element.width (Element.px 22)
                , Font.color Palette.color.primary
                ]
                { src = ImagePath.toString Pages.images.github, description = "Github repo" }
        }


elmDocsLink : Element msg
elmDocsLink =
    Element.newTabLink []
        { url = "https://package.elm-lang.org/packages/dillonkearns/elm-pages/latest/"
        , label =
            Element.image
                [ Element.width (Element.px 22)
                , Font.color Palette.color.primary
                ]
                { src = ImagePath.toString Pages.images.elmLogo, description = "Elm Package Docs" }
        }
