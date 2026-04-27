# Shiny app for GLHLTH 562 Final Project
# Minimal MVP: MBTI + 3 movies -> OMDb lookup -> rule-based survival scores

library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)

# ---- Config ----
if (requireNamespace("dotenv", quietly = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  env_candidates <- c(
    if (!is.null(app_dir)) file.path(app_dir, ".env") else "",
    ".env"
  )
  env_path <- env_candidates[nzchar(env_candidates) & file.exists(env_candidates)][1]
  if (!is.na(env_path) && nzchar(env_path)) {
    dotenv::load_dot_env(file = env_path)
  }
}

source("R/omdb.R")
source("R/scoring.R")

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Creepster&family=Spectral:wght@400;600&display=swap');")),
    tags$style(HTML("
      :root {
        --bg-dark: #120b0f;
        --bg-mid: #1b1020;
        --card: #241723;
        --card-alt: #2a1c29;
        --text: #f3e9e3;
        --muted: #c7b3a8;
        --accent: #d1495b;
        --accent-2: #f4a261;
      }
      body {
        background: radial-gradient(1200px 600px at 20% -10%, #2b1630 0%, var(--bg-dark) 55%, #0d080c 100%);
        color: var(--text);
        font-family: 'Spectral', serif;
      }
      .title {
        font-family: 'Creepster', 'Spectral', serif;
        font-size: 34px;
        letter-spacing: 0.5px;
        margin-bottom: 4px;
        color: var(--accent-2);
        text-shadow: 0 2px 10px rgba(0,0,0,0.6);
      }
      .subtitle { color: var(--muted); margin-bottom: 16px; }
      .panel {
        background: linear-gradient(160deg, var(--card) 0%, var(--card-alt) 100%);
        border-radius: 12px;
        padding: 16px;
        box-shadow: 0 8px 24px rgba(0,0,0,0.35);
        border: 1px solid rgba(255,255,255,0.06);
      }
      .result-card {
        background: rgba(255,255,255,0.04);
        border: 1px solid rgba(255,255,255,0.08);
        border-radius: 10px;
        padding: 12px;
        margin-bottom: 10px;
      }
      .muted { color: var(--muted); font-size: 12px; }
      .pill { display: inline-block; background: var(--accent-2); color: #1b1020; padding: 4px 8px; border-radius: 999px; font-size: 12px; font-weight: 600; }
      .sources {
        background: rgba(255,255,255,0.04);
        border-radius: 10px;
        padding: 12px;
        margin-top: 10px;
        font-size: 12px;
        color: var(--muted);
        border: 1px solid rgba(255,255,255,0.08);
      }
      .btn-primary {
        background: var(--accent);
        border-color: var(--accent);
      }
      .btn-primary:hover {
        background: #b63c4c;
        border-color: #b63c4c;
      }
      .form-control,
      .selectize-input,
      .selectize-control.single .selectize-input {
        background: #1a111a;
        color: var(--accent-2);
        border: 1px solid rgba(244, 162, 97, 0.55);
        box-shadow: inset 0 1px 2px rgba(0,0,0,0.4);
      }
      .form-control::placeholder {
        color: rgba(244, 162, 97, 0.6);
      }
      .selectize-dropdown,
      .selectize-dropdown .option {
        background: #1a111a;
        color: var(--accent-2);
      }
      .selectize-dropdown .active {
        background: #2a1c29;
        color: var(--accent-2);
      }
    "))
  ),
  fluidRow(
    column(
      8,
      div(class = "title", "MBTI Survival In The Movies"),
      div(class = "subtitle", "Predicts how long your personality would last in three films")
    ),
    column(
      4,
      div(class = "muted", "Requires OMDB_API_KEY set in environment")
    )
  ),
  fluidRow(
    column(
      4,
      div(class = "panel",
          selectInput("mbti", "MBTI Type", choices = mbti_types, selected = "ENFP"),
          textInput("movie1", "Movie 1", placeholder = "Alien"),
          textInput("movie2", "Movie 2", placeholder = "Get Out"),
          textInput("movie3", "Movie 3", placeholder = "The Dark Knight"),
          checkboxInput("use_ai", "Use ChatGPT-based scoring (requires OPENAI_API_KEY)", value = FALSE),
          actionButton("go", "Run Prediction", class = "btn-primary"),
          br(),
          br(),
          div(class = "muted", "Tip: Use full titles for better OMDb matches.")
      )
    ),
    column(
      8,
      div(class = "panel",
          uiOutput("results")
      )
    )
  ),
  fluidRow(
    column(
      12,
      div(class = "sources",
          strong("Data sources: "),
          "Movie plots and metadata are pulled from the OMDb API `Plot` field via title search (`t=`). ",
          "MBTI trait descriptors are derived from preference-pair definitions (Myers & Briggs Foundation) ",
          "and type summaries (16Personalities)."
      )
    )
  ),
  fluidRow(
    column(
      12,
      div(class = "sources",
          strong("Debug:"), " ",
          verbatimTextOutput("debug_out")
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  output$debug_out <- renderText({
    paste(
      "OMDB_API_KEY set:", ifelse(get_omdb_key() != "", "yes", "no"),
      "| OPENAI_API_KEY set:", ifelse(get_openai_key() != "", "yes", "no"),
      "| Movies input:", paste(trimws(c(input$movie1, input$movie2, input$movie3)), collapse = " | ")
    )
  })

  movies <- eventReactive(input$go, {
    key <- get_omdb_key()
    shiny::validate(shiny::need(key != "", "Missing OMDB_API_KEY environment variable."))

    titles <- c(input$movie1, input$movie2, input$movie3)
    titles <- trimws(titles)
    shiny::validate(shiny::need(all(titles != ""), "Please enter three movie titles."))

    lapply(titles, function(t) fetch_movie(t, key))
  })

  output$results <- renderUI({
    req(movies())
    res <- movies()

    cards <- lapply(res, function(m) {
      if (is.null(m)) {
        return(div(class = "result-card", strong("Movie not found"), div(class = "muted", "Check spelling or include the year.")))
      }
      if (!is.null(m$error)) {
        return(div(class = "result-card", strong("Movie lookup failed"), div(class = "muted", m$error)))
      }

      score <- score_survival(input$mbti, m$plot, m$genre)
      ai_score <- NULL
      if (isTRUE(input$use_ai)) {
        ai_score <- score_survival_ai(input$mbti, m, score)
      }

      final_half <- if (!is.null(ai_score) && !is.na(ai_score$p_half)) ai_score$p_half else score$p_half
      final_end <- if (!is.null(ai_score) && !is.na(ai_score$p_end)) ai_score$p_end else score$p_end

      div(class = "result-card",
          div(strong(m$title), " ", span(class = "pill", input$mbti)),
          div(class = "muted", paste(m$year, m$genre, m$runtime, "IMDb:", m$imdb)),
          br(),
          div("Probability of dying in first half: ", strong(format_pct(final_half))),
          div("Probability of dying by end of film: ", strong(format_pct(final_end))),
          if (!is.null(ai_score) && !is.na(ai_score$p_half)) {
            div(class = "muted", "AI rationale: ", ai_score$rationale)
          },
          if (length(score$matched) > 0) {
            div(class = "muted", "Matched keywords: ", paste(score$matched, collapse = ", "))
          }
      )
    })

    do.call(tagList, cards)
  })
}

shinyApp(ui, server)
