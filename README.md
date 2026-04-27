# GLHLTH_562 Final Project: MBTI Movie Survival Calculator

## Overview
This project is a Shiny web app that estimates how long a person with a given MBTI type would survive in three selected movies. The app pulls each movie’s plot and metadata from the OMDb API, extracts trait-relevant keywords, and computes survival probabilities for the first half and by the end of the film. An optional AI rationale can enrich the explanation.

## What It Does
- Accepts an MBTI type and three movie titles
- Retrieves plot summaries, genres, year, runtime, and IMDb rating from OMDb
- Scores survival using a transparent rule-based MBTI trait model
- Applies genre-based risk adjustments and MBTI trait priors
- Optionally uses OpenAI scoring to compute survival scores

## Key Design Choices
- **Rule-based scoring** for interpretability and reproducibility
- **Trait dictionary + MBTI overlays** to connect personality to survivability
- **Genre risk weights** to account for baseline movie danger
- **Optional AI rationale** behind a checkbox to keep scoring transparent by default

## Data Sources
- **OMDb API** for movie plot summaries and metadata
- **MBTI trait descriptors** derived from preference-pair definitions and public type summaries

## Data Pipeline
**Where the data comes from**
- User input: MBTI type + three movie titles (Shiny form)
- OMDb API: `http://www.omdbapi.com/?t=<TITLE>&plot=full&apikey=<KEY>`
- Optional OpenAI API for AI rationale (if enabled)

**How the data is ingested**
- `httr` sends HTTP requests to OMDb and OpenAI
- Authentication via environment variables (`OMDB_API_KEY`, `OPENAI_API_KEY`)
- Responses parsed from JSON with `jsonlite`

**How the data is processed**
- Plot text is tokenized with `tidytext`
- Tokens are matched to a trait keyword dictionary
- MBTI trait priors and genre risk weights adjust baseline risk
- Scores are summed and clamped to readable probabilities

**What the output is**
- A Shiny app that displays survival probabilities (halfway + end)
- Optional AI rationale explaining the score

## Repository Structure
```
project/
├── README.md              # Pipeline documentation
├── app.R                  # Shiny app entry point
├── R/                     # Supporting scripts or functions
│   ├── omdb.R             # OMDb fetch + helpers
│   └── scoring.R          # MBTI traits + scoring logic
├── data/                  # Raw or cached data (if applicable)
├── deck/                  # Presentation slides (qmd + html)
└── .gitignore             # Exclude API keys, large files, etc.
```

## How To Run
1. Install required R packages:
   - `shiny`, `httr`, `jsonlite`, `dplyr`, `tidyr`, `stringr`, `tidytext`
   - optional: `dotenv` (to load `.env`)
2. Create a `.env` file with your OMDb key:
   - `OMDB_API_KEY=YOUR_KEY_HERE`
3. (Optional) Add OpenAI credentials to enable AI rationale:
   - `OPENAI_API_KEY=YOUR_KEY_HERE`
   - `OPENAI_MODEL=gpt-4o-mini` (optional override)
4. Run the app:
   - `shiny::runApp("app.R")`

**Deployment (optional)**
- Run locally in RStudio or the R console
- For hosted demos, deploy the project directory to a Shiny server

If `OMDB_API_KEY` is not set, the app will prompt you to configure it.

## Notes
- AI rationale is optional and does not always change the numeric score.
- Plot summaries vary in quality; ambiguity can affect keyword matches.
