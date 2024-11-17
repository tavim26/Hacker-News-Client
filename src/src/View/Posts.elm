module View.Posts exposing (..)

import Html exposing (Html, div, text, table, tr, th, td, select, option, input, label, a)
import Html.Attributes exposing (href, class, checked, value, type_, id, selected, for)
import Html.Events exposing (onClick, onInput, onCheck)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time exposing (formatDate, formatTime, formatDuration, durationBetween)




{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html msg
postTable config currentTime posts =
    let
        --Sortare/filtrare post-uri conform configuratiei
        postsToShow = filterPosts config posts
    in
    -- tabelul HTML
    table []
        ( -- Antetul tabelului, cu fiecare denumire de coloana
          [ tr []
              [ th [] [ text "Score" ]
              , th [] [ text "Title" ]
              , th [] [ text "Type" ]
              , th [] [ text "Posted Date" ]
              , th [] [ text "Link" ]
              ]
          ]
          -- Concatenare antet si randurile postului
          ++ (List.map (viewPostRow config currentTime) postsToShow)
        )

-- Functie ajutatoare `viewPostRow` pentru a crea un rand in tabel pentru fiecare postare
viewPostRow : PostsConfig -> Time.Posix -> Post -> Html msg
viewPostRow config currentTime post =
    let
        -- Calculare data absoluta a postarii
        postTime = Util.Time.formatTime Time.utc post.time

        -- calculare timp trecut de la postare
        duration = 
            Util.Time.durationBetween post.time currentTime

        timeDisplay =
            case duration of
                Just d -> 
                    -- Combinare data absoluta cu durata relativa
                    postTime ++ " (" ++ Util.Time.formatDuration d ++ ")"
                Nothing -> postTime
    in
    tr []
        [ td [ class "post-score" ] [ text (String.fromInt post.score) ]
        , td [ class "post-title" ] [ text post.title ]
        , td [ class "post-type" ] [ text post.type_ ]
        , td [ class "post-time" ] [ text timeDisplay ]
        , td [ class "post-url" ] [ a [ href (Maybe.withDefault "#" post.url) ] [ text "Link" ] ]
        ]









{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [ -- select pentru nr de post-uri pe pagina
          div []
            [ label [ for "select-posts-per-page" ] [ text "Posts per page:" ]
            , select
                [ id "select-posts-per-page"
                , onInput (\str -> 
                    String.toInt str
                        |> Result.fromMaybe (Err "Invalid number")  
                        |> Result.map (\n -> ChangePostsPerPage n)
                        |> Result.withDefault (ChangePostsPerPage 10)
                        |> ConfigChanged
                    )
                ]
                [ option [ value "10", selected (config.postsToShow == 10) ] [ text "10" ]
                , option [ value "25", selected (config.postsToShow == 25) ] [ text "25" ]
                , option [ value "50", selected (config.postsToShow == 50) ] [ text "50" ]
                ]
            ]

        , -- Select pentru sortare
          div []
            [ label [ for "select-sort-by" ] [ text "Sort by:" ]
            , select
                [ id "select-sort-by"
                , onInput (ChangeSortBy >> ConfigChanged)
                ]
                (List.map (\sort -> option [ value (sortToString sort), selected (config.sortBy == sort) ] [ text (sortToString sort) ]) sortOptions)
            ]

        , -- Checkbox pentru joburi
          div []
            [ label [ for "checkbox-show-job-posts" ] [ text "Show job posts:" ]
            , input
                [ id "checkbox-show-job-posts"
                , type_ "checkbox"
                , checked config.showJobs
                , onCheck (ConfigChanged << ToggleJobPosts)
                ]
                []
            ]

        , -- Checkbox pentru posturi text-only
          div []
            [ label [ for "checkbox-show-text-only-posts" ] [ text "Show text-only posts:" ]
            , input
                [ id "checkbox-show-text-only-posts"
                , type_ "checkbox"
                , checked config.showTextOnly
                , onCheck (ConfigChanged << ToggleTextOnlyPosts)
                ]
                []
            ]
        ]


