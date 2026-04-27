safe_from_json <- function(txt) {
  if (!is.character(txt) || length(txt) < 1 || is.na(txt) || !nzchar(txt)) return(NULL)
  tryCatch(jsonlite::fromJSON(txt), error = function(e) NULL)
}

`%||%` <- function(a, b) {
  if (is.null(a) || is.na(a) || !is.character(a) || !nzchar(a)) b else a
}

get_omdb_key <- function() {
  Sys.getenv("OMDB_API_KEY", unset = "")
}

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
