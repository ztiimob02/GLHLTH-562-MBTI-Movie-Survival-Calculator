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

get_omdb_key <- function() {
  Sys.getenv("OMDB_API_KEY", unset = "")
}

get_openai_key <- function() {
  Sys.getenv("OPENAI_API_KEY", unset = "")
}

get_openai_model <- function() {
  Sys.getenv("OPENAI_MODEL", unset = "gpt-4o-mini")
}

safe_from_json <- function(txt) {
  if (!is.character(txt) || length(txt) < 1 || is.na(txt) || !nzchar(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
}

# ---- Data: MBTI trait dictionary ----
mbti_letters <- tibble::tribble(
  ~letter, ~trait,
  "E", "social",
  "E", "energetic",
  "E", "expressive",
  "I", "reserved",
  "I", "reflective",
  "I", "self_contained",
  "S", "practical",
  "S", "grounded",
  "S", "detail_focused",
  "N", "imaginative",
  "N", "future_focused",
  "N", "pattern_seeking",
  "T", "analytical",
  "T", "objective",
  "T", "detached",
  "F", "empathetic",
  "F", "values_driven",
  "F", "harmonizing",
  "J", "structured",
  "J", "decisive",
  "J", "organized",
  "P", "adaptable",
  "P", "improvisational",
  "P", "spontaneous"
)

mbti_type_overlays <- tibble::tribble(
  ~mbti, ~trait,
  "INTJ", "strategic",
  "INTJ", "independent",
  "INTP", "curious",
  "INTP", "inventive",
  "ENTJ", "assertive",
  "ENTJ", "leaderly",
  "ENTP", "brazen",
  "ENTP", "debating",
  "INFJ", "idealistic",
  "INFJ", "principled",
  "INFP", "sensitive",
  "INFP", "altruistic",
  "ENFJ", "protective",
  "ENFJ", "mentorlike",
  "ENFP", "optimistic",
  "ENFP", "inspiring",
  "ISTJ", "duty_bound",
  "ISTJ", "reliable",
  "ISFJ", "careful",
  "ISFJ", "supportive",
  "ESTJ", "commanding",
  "ESTJ", "practical",
  "ESFJ", "nurturing",
  "ESFJ", "social",
  "ISTP", "hands_on",
  "ISTP", "cool_headed",
  "ISFP", "flexible",
  "ISFP", "gentle",
  "ESTP", "risk_taking",
  "ESTP", "bold",
  "ESFP", "enthusiastic",
  "ESFP", "fun_loving"
)

mbti_types <- c(
  "INTJ","INTP","ENTJ","ENTP",
  "INFJ","INFP","ENFJ","ENFP",
  "ISTJ","ISFJ","ESTJ","ESFJ",
  "ISTP","ISFP","ESTP","ESFP"
)

build_mbti_traits <- function(mbti) {
  letters <- strsplit(mbti, "")[[1]]
  by_letter <- mbti_letters %>% filter(letter %in% letters) %>% select(trait)
  by_type <- mbti_type_overlays %>% filter(mbti == !!mbti) %>% select(trait)
  bind_rows(by_letter, by_type) %>% distinct()
}

trait_keywords <- tibble::tribble(
  ~trait, ~keyword, ~impact_half, ~impact_end,
  "analytical", "strategy", -0.04, -0.06,
  "analytical", "plan", -0.04, -0.06,
  "strategic", "tactic", -0.04, -0.06,
  "strategic", "ambush", -0.03, -0.05,
  "strategic", "intel", -0.03, -0.05,
  "independent", "solo", 0.02, 0.03,
  "independent", "alone", 0.04, 0.03,
  "detached", "cold", 0.03, 0.02,
  "detached", "isolated", 0.04, 0.03,
  "curious", "experiment", 0.03, 0.04,
  "curious", "unknown", 0.04, 0.05,
  "inventive", "device", -0.02, -0.03,
  "inventive", "hack", -0.02, -0.03,
  "assertive", "leader", -0.02, -0.03,
  "assertive", "command", -0.02, -0.03,
  "leaderly", "crew", -0.01, -0.02,
  "decisive", "decision", -0.03, -0.04,
  "decisive", "resolve", -0.03, -0.04,
  "improvisational", "improvise", -0.02, -0.03,
  "improvisational", "adapt", -0.02, -0.03,
  "adaptable", "escape", -0.02, -0.03,
  "adaptable", "evade", -0.02, -0.03,
  "empathetic", "sacrifice", 0.04, 0.05,
  "empathetic", "compassion", 0.04, 0.05,
  "values_driven", "promise", 0.02, 0.03,
  "values_driven", "oath", 0.02, 0.03,
  "idealistic", "innocent", 0.04, 0.05,
  "idealistic", "hope", 0.03, 0.04,
  "sensitive", "fear", 0.05, 0.06,
  "sensitive", "panic", 0.05, 0.06,
  "harmonizing", "peace", 0.02, 0.03,
  "harmonizing", "mediate", 0.02, 0.03,
  "optimistic", "hero", -0.01, 0.01,
  "optimistic", "brave", -0.02, -0.01,
  "social", "group", 0.02, 0.02,
  "social", "team", 0.02, 0.02,
  "impulsive", "reckless", 0.06, 0.07,
  "impulsive", "rash", 0.06, 0.07,
  "practical", "survive", -0.03, -0.04,
  "practical", "escape", -0.03, -0.04,
  "cautious", "hide", -0.02, -0.03,
  "cautious", "careful", -0.02, -0.03,
  "duty_bound", "mission", 0.01, 0.02,
  "duty_bound", "orders", 0.01, 0.02,
  "protective", "protect", 0.02, 0.03,
  "protective", "save", 0.02, 0.03,
  "brazen", "treacherous", -0.02, -0.01,
  "brazen", "fight", -0.02, -0.02,
  "risk_taking", "gamble", 0.04, 0.05,
  "risk_taking", "danger", 0.05, 0.06,
  "bold", "charge", 0.03, 0.04,
  "bold", "attack", 0.03, 0.04,
  "reserved", "quiet", -0.01, -0.01,
  "reserved", "hidden", -0.02, -0.02,
  "reflective", "remember", -0.01, -0.01,
  "reflective", "think", -0.01, -0.01,
  "self_contained", "lone", 0.03, 0.03,
  "grounded", "realistic", -0.02, -0.03,
  "detail_focused", "detail", -0.01, -0.02,
  "future_focused", "vision", -0.01, -0.01,
  "pattern_seeking", "pattern", -0.01, -0.02,
  "objective", "logic", -0.02, -0.03,
  "structured", "protocol", -0.02, -0.03,
  "organized", "schedule", -0.01, -0.02,
  "spontaneous", "spur", 0.03, 0.04,
  "energetic", "rush", 0.02, 0.03,
  "expressive", "shout", 0.02, 0.03,
  "inspiring", "rally", -0.01, -0.02,
  "supportive", "help", 0.02, 0.03,
  "careful", "cautious", -0.02, -0.03,
  "reliable", "steady", -0.02, -0.03,
  "hands_on", "repair", -0.02, -0.03,
  "cool_headed", "calm", -0.02, -0.03,
  "flexible", "pivot", -0.02, -0.02,
  "gentle", "mercy", 0.02, 0.03,
  "mentorlike", "guide", -0.01, -0.02,
  "nurturing", "care", 0.02, 0.03,
  "fun_loving", "party", 0.03, 0.04
)

# Trait priors: MBTI-level baseline adjustments (independent of plot keywords)
trait_priors <- tibble::tribble(
  ~trait, ~prior_half, ~prior_end,
  "impulsive", 0.05, 0.06,
  "risk_taking", 0.04, 0.05,
  "brazen", 0.03, 0.04,
  "bold", 0.03, 0.04,
  "decisive", -0.01, -0.01,
  "strategic", -0.02, -0.03,
  "analytical", -0.02, -0.03,
  "cautious", -0.03, -0.04,
  "careful", -0.03, -0.04,
  "practical", -0.02, -0.03,
  "empathetic", 0.02, 0.03,
  "sensitive", 0.03, 0.04
)

# Genre risk weighting
genre_risk <- tibble::tribble(
  ~genre, ~base_half, ~base_end,
  "Horror", 0.10, 0.12,
  "Thriller", 0.06, 0.08,
  "Sci-Fi", 0.05, 0.06,
  "Action", 0.05, 0.07,
  "Adventure", 0.03, 0.05,
  "Drama", 0.02, 0.03,
  "Crime", 0.03, 0.05,
  "Mystery", 0.04, 0.05,
  "War", 0.08, 0.10,
  "Western", 0.04, 0.05,
  "Fantasy", 0.03, 0.04,
  "Romance", -0.03, -0.04,
  "Comedy", -0.08, -0.10,
  "Family", -0.10, -0.12,
  "Animation", -0.12, -0.14,
  "Documentary", -0.04, -0.04,
  "Musical", -0.12, -0.14,
  "Music", -0.05, -0.06
)

# ---- Helpers ----
fetch_movie <- function(title, key) {
  if (title == "") return(NULL)
  url <- paste0("http://www.omdbapi.com/?t=", URLencode(title), "&plot=full&apikey=", key)
  res <- httr::GET(url)
  if (httr::http_error(res)) {
    return(list(title = title, error = paste("HTTP error:", httr::status_code(res))))
  }
  json <- safe_from_json(httr::content(res, as = "text", encoding = "UTF-8"))
  if (is.null(json)) return(list(title = title, error = "Could not parse OMDb response."))
  if (!is.null(json$Response) && json$Response == "False") {
    return(list(title = title, error = json$Error %||% "Movie not found."))
  }
  list(
    title = json$Title %||% title,
    plot = json$Plot %||% "",
    genre = json$Genre %||% "",
    runtime = json$Runtime %||% "",
    year = json$Year %||% "",
    imdb = json$imdbRating %||% ""
  )
}

`%||%` <- function(a, b) {
  if (is.null(a) || is.na(a) || !is.character(a) || !nzchar(a)) b else a
}

score_survival <- function(mbti, plot_text, genre_text) {
  if (is.null(plot_text) || is.na(plot_text)) plot_text <- ""
  if (is.null(genre_text) || is.na(genre_text)) genre_text <- ""
  traits <- build_mbti_traits(mbti)
  if (nrow(traits) == 0) return(list(p_half = NA_real_, p_end = NA_real_, matched = character(0)))
  
  keywords <- trait_keywords %>% inner_join(traits, by = "trait")
  
  genres <- if (nzchar(genre_text)) str_split(genre_text, ",\\s*")[[1]] else character(0)
  genre_base <- genre_risk %>% filter(genre %in% genres)
  base_half <- 0.5 + sum(genre_base$base_half, na.rm = TRUE)
  base_end <- 0.7 + sum(genre_base$base_end, na.rm = TRUE)
  
  # Extra penalty reduction for playful genres / rom-com combination
  if (all(c("Romance", "Comedy") %in% genres)) {
    base_half <- base_half - 0.08
    base_end <- base_end - 0.10
  }
  
  # Slice-of-life heuristic: drama + comedy + no high-risk genres
  high_risk <- c("Horror", "Thriller", "Action", "Crime", "War", "Mystery", "Sci-Fi")
  if (all(c("Drama", "Comedy") %in% genres) && !any(genres %in% high_risk)) {
    base_half <- base_half - 0.06
    base_end <- base_end - 0.08
  }
  
  # Lighthearted scaling: reduce baseline but preserve variation
  playful <- c("Comedy", "Romance", "Family", "Animation", "Musical", "Music")
  playful_count <- sum(genres %in% playful)
  if (!any(genres %in% high_risk) && playful_count > 0) {
    scale <- max(0.20, 0.35 - 0.03 * playful_count)
    base_half <- base_half * scale
    base_end <- base_end * scale
  }
  
  tokens <- tibble::tibble(text = plot_text) %>%
    tidytext::unnest_tokens(word, text) %>%
    filter(!is.na(word))
  
  matched <- tokens %>%
    inner_join(keywords, by = c("word" = "keyword"))
  
  priors <- trait_priors %>% inner_join(traits, by = "trait")
  prior_half <- sum(priors$prior_half, na.rm = TRUE)
  prior_end <- sum(priors$prior_end, na.rm = TRUE)
  
  score_half <- base_half + prior_half + sum(matched$impact_half, na.rm = TRUE)
  score_end <- base_end + prior_end + sum(matched$impact_end, na.rm = TRUE)
  
  # Clamp to [0.02, 0.98] for readability
  p_half <- min(max(score_half, 0.02), 0.98)
  p_end <- min(max(score_end, 0.02), 0.98)
  
  # Final cap for very light films: keep <10% but allow MBTI variation
  if (!any(genres %in% high_risk) && playful_count > 0) {
    p_half <- min(p_half, 0.09)
    p_end <- min(p_end, 0.12)
  }
  
  list(
    p_half = p_half,
    p_end = p_end,
    matched = unique(matched$word),
    traits = traits$trait
  )
}

format_pct <- function(x) {
  if (is.na(x)) return("NA")
  paste0(round(100 * x), "%")
}

score_survival_ai <- function(mbti, movie, rule_score) {
  key <- get_openai_key()
  if (key == "") return(list(p_half = NA_real_, p_end = NA_real_, rationale = "Missing OPENAI_API_KEY."))
  
  model <- get_openai_model()
  prompt <- paste0(
    "You are scoring survival probability for a fictional character with MBTI type ", mbti, ". ",
    "Use the movie plot and genre. Output JSON ONLY with keys: ",
    "`p_half` (0-1), `p_end` (0-1), `rationale` (short). ",
    "Constraints: p_end >= p_half. Keep rationale <= 40 words.\n\n",
    "MBTI traits: ", paste(rule_score$traits, collapse = ", "), "\n",
    "Movie title: ", movie$title, "\n",
    "Genre: ", movie$genre, "\n",
    "Plot: ", movie$plot, "\n",
    "Rule-based estimate: half=", round(rule_score$p_half, 2), ", end=", round(rule_score$p_end, 2), "\n",
    "Matched keywords: ", paste(rule_score$matched, collapse = ", ")
  )
  
  body <- list(
    model = model,
    messages = list(
      list(role = "developer", content = "Return only valid JSON with the required keys."),
      list(role = "user", content = prompt)
    ),
    temperature = 0.4,
    max_tokens = 200
  )
  
  res <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      Authorization = paste("Bearer", key),
      `Content-Type` = "application/json"
    ),
    body = jsonlite::toJSON(body, auto_unbox = TRUE)
  )
  
  if (httr::http_error(res)) {
    return(list(p_half = NA_real_, p_end = NA_real_, rationale = "OpenAI API error."))
  }
  
  json <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"))
  content <- json$choices[[1]]$message$content %||% ""
  
  if (!is.character(content)) {
    return(list(p_half = NA_real_, p_end = NA_real_, rationale = "AI response content was not text."))
  }
  
  cleaned <- gsub("^```json\\s*|```$", "", content)
  cleaned <- trimws(cleaned)
  parsed <- safe_from_json(cleaned)
  if (is.null(parsed)) {
    return(list(p_half = NA_real_, p_end = NA_real_, rationale = "AI response could not be parsed."))
  }
  
  list(
    p_half = as.numeric(parsed$p_half),
    p_end = as.numeric(parsed$p_end),
    rationale = as.character(parsed$rationale)
  )
}

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
