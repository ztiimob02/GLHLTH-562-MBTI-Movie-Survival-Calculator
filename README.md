# GLHLTH_562 Final Project: MBTI Movie Survival Calculator

## I. Product Description
This project is a Shiny web application that is able to predict how long a person with a given MBTI type would survive in three of their favorite movies. Users input their MBTI type along with three movie titles. The app then retrieves plot summaries and metadata from the OMDb API, extracts survival-relevant themes and character dynamics, and outputs a personalized survival estimate for each movie (e.g. probability of dying in the first half vs. by the end of the film). The product is interactive and tailored to the user’s specific input.

## II. Target Capabilities
**User Input**
Users interact with the tool by entering:
- Their MBTI personality type
- Their top three favorite movies

The application processes this input and returns a personalized estimate of how long they would survive in each movie scenario.

**API Integration**
After the user enters a movie title, the application queries the OMDb API to retrieve:
- Plot summary
- Genre
- Keywords from the description
- Character types or narrative context

This data will be used dynamically at runtime rather than relying on a static dataset. The use of an API is most appropriate as our product must respond to the choices inputted by the user in real time. If a static dataset were used, users would only have availability to a list of preselected movies. An API allows for a much more dynamic list that can obtain multiple queries regarding a given movie, providing more flexibility and a more interactive experience for the user.

**(Optional/at our discretion) GenAI Model**
A generative AI model could be used to:
- Summarize survival-relevant traits from the movie plot
- Interpret MBTI personality descriptions
- Generate a narrative explanation such as:
  “ENFPs tend to value compassion over accountability. In the film *Kill Bill (2003),* an ENFP would likely either allow him/herself to be killed prematurely to avoid the stress of being hunted or hesitate during a climactic fight and lose his/her life later on in the film.”

## III. Data Source
**Open Movie Database (OMDb)**
`http://www.omdbapi.com/?t=The+Matrix&apikey=[INSERT KEY]`
Provides access to movie metadata

Returns:
- Plot summary
- Genre
- Cast
- Ratings
- Runtime

**MBTI Personality Descriptions**
MBTI personality descriptions will come from publicly available summaries of the Myers-Briggs Type Indicator, which categorize behavioral traits for each type. These descriptions will be used to construct a trait dictionary (e.g., analytical, impulsive, empathetic). The dictionary will be used to map different personality traits and extend those to the context of the movie inputted by the user.

## IV. Technical Plan
**Possible R packages:**
- `shiny` – UI and user input
- `httr` – API requests
- `jsonlite` – parsing API responses
- `tidytext` – keyword extraction from plot summaries
- `dplyr` – data manipulation
- `stringr` – text processing
- `openai` – optional if we decide to incorporate AI functionality

**Rough sketch of pipeline:**

**Step 1. User Input (Shiny interface)**
- MBTI type
- Movie 1
- Movie 2
- Movie 3

**Step 2. API Retrieval**
For each movie:
- Movie title → OMDb API request → Plot summary retrieval
- Example in R: `url <- paste0("http://www.omdbapi.com/?t=", movie, "&apikey=KEY")`

**Step 3. Text Processing (tidytext)**
- Tokenize plot summary → extract keywords → compare with MBTI trait dictionary → generate survival scores
- Example: plot tokens → match keywords (optimistic, brazen, sympathetic) → score compatibility with MBTI traits

**Step 4. Survival Scoring Algorithm**
Each defined keyword will contribute points to the survival score, either adding or subtracting from the score. Two score categories will be calculated:
- Probability of death by first half
- Probability of death by movie end

**Example:**

Trait | Movie Keyword | Impact on Survival
--- | --- | ---
Optimistic | Gladiatorial | Shorter survival
Brazen | Treacherous | Longer survival
Sympathetic | Social conflict | Shorter survival

Column (3) then acts as the basis for the probability of death during the first half of the movie and by the end of it (%).

**Step 5. Output**
Shiny interface displays the movie title, user’s MBTI type, and survival probabilities.

Example:
- Movie: *Alien*
- MBTI: ENFP
- Probability of dying in first half: 82%
- Probability of dying by end of film: 97%

## V. Division of Labor
**Zada:**
- OMDb API integration
- Text Processing Pipeline
- Personality trait dictionary

**Khalid:**
- Shiny Interface
- Input validation / Handling Errors
- Output Display and explanation
- GitHub management
