library(shiny)
library(httr)
library(jsonlite)
library(stringr)

MBTI_TYPES <- c(
  "INTJ","INTP","ENTJ","ENTP",
  "INFJ","INFP","ENFJ","ENFP",
  "ISTJ","ISFJ","ESTJ","ESFJ",
  "ISTP","ISFP","ESTP","ESFP"
)

omdb_lookup <- function(title, api_key) {
  if (is.null(api_key) || api_key == "") {
    return(list(
      Title = title,
      Plot = "Placeholder plot summary. Add OMDb API key to fetch real data.",
      Genre = "Unknown",
      Response = "True"
    ))
  }

  resp <- GET("http://www.omdbapi.com/", query = list(t = title, apikey = api_key))
  if (http_error(resp)) {
    return(list(Response = "False", Error = "HTTP error from OMDb."))
  }

  content <- content(resp, as = "text", encoding = "UTF-8")
  fromJSON(content)
}

score_movie <- function(mbti, plot, genre) {
  text <- tolower(paste(plot, genre))
  danger_terms <- "fight|kill|war|blood|murder|alien|monster|virus|apocalypse|zombie|crime|assassin"
  social_terms <- "family|friend|love|relationship|team|community"
  chaos_terms <- "chaos|betrayal|treachery|ambush|trap|survival"

  danger_score <- str_count(text, danger_terms)
  social_score <- str_count(text, social_terms)
  chaos_score <- str_count(text, chaos_terms)

  mbti_bias <- ifelse(grepl("N", mbti), 1, 0) + ifelse(grepl("P", mbti), 1, 0)

  raw <- 15 + (danger_score * 8) + (chaos_score * 6) - (social_score * 3) + (mbti_bias * 2)
  p_half <- min(max(raw, 5), 95)
  p_end <- min(max(p_half + 10 + (danger_score * 2), 10), 99)

  list(p_half = round(p_half, 1), p_end = round(p_end, 1))
}

ui <- fluidPage(
  titlePanel("MBTI Movie Survival Calculator"),

  sidebarLayout(
    sidebarPanel(
      selectInput("mbti", "MBTI Type", choices = MBTI_TYPES),
      textInput("movie1", "Movie 1"),
      textInput("movie2", "Movie 2"),
      textInput("movie3", "Movie 3"),
      actionButton("run", "Calculate Survival"),
      tags$hr(),
      tags$small("OMDb API key read from OMDB_API_KEY env var. Placeholder data is used if missing.")
    ),

    mainPanel(
      uiOutput("results")
    )
  )
)

server <- function(input, output, session) {
  output$results <- renderUI({
    req(input$run)

    movies <- c(input$movie1, input$movie2, input$movie3)
    movies <- movies[nzchar(movies)]

    if (length(movies) < 3) {
      return(tags$p("Please enter three movie titles."))
    }

    api_key <- Sys.getenv("OMDB_API_KEY")

    cards <- lapply(movies, function(title) {
      data <- omdb_lookup(title, api_key)

      if (!is.null(data$Response) && data$Response == "False") {
        err_msg <- ifelse(is.null(data$Error), "Movie not found.", data$Error)
        return(tags$div(
          tags$h4(title),
          tags$p(paste("Error:", err_msg))
        ))
      }

      plot <- ifelse(is.null(data$Plot) || data$Plot == "N/A", "No plot available.", data$Plot)
      genre <- ifelse(is.null(data$Genre) || data$Genre == "N/A", "Unknown", data$Genre)

      scores <- score_movie(input$mbti, plot, genre)

      tags$div(
        tags$h4(data$Title),
        tags$p(tags$strong("Genre:"), genre),
        tags$p(tags$strong("Plot:"), plot),
        tags$p(tags$strong("MBTI:"), input$mbti),
        tags$p(tags$strong("Probability of dying in first half:"), paste0(scores$p_half, "%")),
        tags$p(tags$strong("Probability of dying by end of film:"), paste0(scores$p_end, "%")),
        tags$hr()
      )
    })

    tagList(cards)
  })
}

shinyApp(ui, server)
