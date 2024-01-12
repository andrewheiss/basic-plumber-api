library(tidyverse)
library(glue)
library(plumber)
library(jose)

options("plumber.port" = 6312)

# Error handling ----------------------------------------------------------

# Custom error handling
# https://web.archive.org/web/20240110015732/https://unconj.ca/blog/structured-errors-in-plumber-apis.html

# Helper function to replace stop()
api_error <- function(message, status) {
  err <- structure(
    list(message = message, status = status),
    class = c("api_error", "error", "condition")
  )
  signalCondition(err)
}

# General error handling function
error_handler <- function(req, res, err) {
  if (!inherits(err, "api_error")) {
    res$status <- 500
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = 500,
      message = "Internal server error."
    ))
    res$setHeader("content-type", "application/json")  # Make this JSON

    # Print the internal error so we can see it from the server side. A more
    # robust implementation would use proper logging.
    print(err)
  } else {
    # We know that the message is intended to be user-facing.
    res$status <- err$status
    res$body <- jsonlite::toJSON(auto_unbox = TRUE, list(
      status = err$status,
      message = err$message
    ))
    res$setHeader("content-type", "application/json")  # Make this JSON
  }

  res
}



# API details -------------------------------------------------------------

#* @apiTitle Plumber Example API
#* @apiDescription Fun times with R and plumber and APIs
#* @apiContact list(name = "Andrew Heiss", url = "https://www.andrewheiss.com/")
#* @apiLicense list(name = "MIT", url = "https://opensource.org/license/mit/")
#* @apiVersion 0.1.0
#* @apiTag Data Access different data things
#* @apiTag Debugging Endpoints for testing to make sure things are working
#* @apiTag Authentication Endpoints for illustrating authentication


# Overall plumber pipeline
#* @plumber
function(pr) {
  # Use custom error handler
  pr |> pr_set_error(error_handler)
}


#* Enable Cross-origin Resource Sharing
#* @filter cors
# This is more complex than what's in the official documentation
# (https://www.rplumber.io/articles/security.html#cross-origin-resource-sharing-cors)
# because it correctly allows requests to come from http://localhost too
# (via https://github.com/rstudio/plumber/issues/66#issuecomment-418660334)
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods", "*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }
}


# Endpoints ---------------------------------------------------------------

#* Plot a fancy histogram
#* @tag Debugging
#* @serializer png list(width = 500, height = 300)
#* @get /plot
function(n = 100) {
  # Make sure n isn't ever too big so that the server doesn't crash
  if (n >= 10000) {
    api_error("`n` is too big. Use a number less than 10,000.", 400)
  }

  my_plot <- ggplot(
    data = data.frame(x = rnorm(n)),
    aes(x = x)
  ) +
    geom_histogram(fill = "darkred", color = "white") +
    labs(title = glue("A histogram of {n} random numbers")) +
    theme_bw()

  print(my_plot)
}


#* Return clean penguins data
#* @tag Data
#* @seralizer json
#* @get /penguins
function() {
  library(palmerpenguins)

  penguins_clean <- penguins |> dplyr::filter(!is.na(sex))

  list(
    extra_details = "All missing values have been removed. You're welcome!",
    data = penguins_clean
  )
}


#* Get and clean Goodreads data
#* @tag Data
#* @serializer json
#* @get /books
function(year = 2024) {
  library(googlesheets4)

  gs4_deauth()  # The sheet is public so there's no need to log in
  local_gs4_quiet()  # Turn off the googlesheets messages

  books_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1oQqX4G4CJaa7cgfsEW4LeorcQwxVeYe0Q83WrJbcN6Y/edit#gid=0")

  books_clean <- books_raw |>
    # Convert the timestamp to an actual date
    mutate(timestamp = dmy_hms(user_read_at)) |>
    # Make some extra helper columns
    mutate(
      read_year = year(timestamp),
      read_month = month(timestamp),
      read_month_fct = month(timestamp, label = TRUE, abbr = FALSE)
    ) |>
    # Only keep books for the specified year
    filter(read_year == as.integer(year)) |>
    # Only include a few columns
    select(
      timestamp = user_read_at,
      book_title = title,
      book_author = author_name,
      rating = user_rating,
      read_year, read_month, read_month_fct
    )

  # Find the count of all the books
  total <- books_clean |> nrow()

  # Calculate the number of books by month
  monthly_count <- books_clean |>
    group_by(read_month_fct, .drop = FALSE) |>
    summarize(count = n())

  # Return the total count, a count by month, and the full data
  return(
    list(
      count = total,
      monthly_count = monthly_count,
      full_data = books_clean
    )
  )
}


#* Return JSON data from Google Sheets and FitBit
#* @tag Data
#* @serializer json
#* @post /fitbit_googlesheet
function(req, res, manual_token = NA) {
  # Require a JWT
  require_token(req, res, manual_token)

  library(googlesheets4)

  # Handle NAs correctly when using map_dbl()
  safe_map_dbl <- possibly(map_dbl, otherwise = NA_real_)

  gs4_deauth()  # The sheet is public so there's no need to log in
  local_gs4_quiet()  # Turn off the googlesheets messages

  # Load the Google Sheet as a dataframe and parse the JSON in the data column.
  # This creates a nested list column, and we can access the different elements
  # with purrr::map()
  fitbit_data_raw <- read_sheet("https://docs.google.com/spreadsheets/d/175djCkehC5OPN0wEbxPWSYML3JUcbGGVQ871ZLLCxGQ/") |>
    mutate(data = map(data, ~jsonlite::fromJSON(.)))

  # Create a tidy dataframe of activities
  activities <- fitbit_data_raw |>
    mutate(activity = map(data, ~.$activities)) |>
    # Handle days with no activities
    mutate(n_activities = map(activity, ~length(.x))) |>
    filter(n_activities > 0) |>
    unnest(activity) |>
    select(date, name, duration) |>
    mutate(duration = duration / 60 / 1000)  # duration = milliseconds

  # Get a count of exercise/activity minutes per day
  activities_daily <- activities |>
    group_by(date) |>
    summarize(exercise_minutes = sum(duration)) |>
    ungroup()

  # Calculate the total distance (in miles) per day
  distances <- fitbit_data_raw |>
    mutate(distance = map(data, ~.$summary$distances)) |>
    unnest(distance) |>
    select(date, activity, distance) |>
    filter(activity == "total") |>
    group_by(date) |>
    summarize(distance = sum(distance)) |>
    ungroup()

  # Create a dataframe with all sorts of data from the Fitbit JSON
  fitbit_summary <- fitbit_data_raw |>
    mutate(
      steps = safe_map_dbl(data, ~.$summary$steps),
      floors = safe_map_dbl(data, ~.$summary$floors),
      restingHeartRate = safe_map_dbl(data, ~.$summary$restingHeartRate),
      marginalCalories = safe_map_dbl(data, ~.$summary$marginalCalories)
    ) |>
    select(-data) |>
    left_join(activities_daily, by = "date") |>
    left_join(distances, by = "date") |>
    replace_na(list(exercise_minutes = 0, distance = 0, steps = 0)) |>
    mutate(
      date_actual = ymd(date),
      month = month(date_actual, label = TRUE, abbr = FALSE),
      weekday = wday(date_actual, label = TRUE, abbr = FALSE)
    )

  # Return the summary data and the activities data
  return(
    list(
      summary = fitbit_summary,
      activities = activities
    )
  )
}


# Authentication endpoints ------------------------------------------------

# Load secret variables as environment variables. Alternatively, use .Renviron for this.
source("secrets.R")

#* Super unsafe secret thing
#* @tag Authentication
#* @seralizer text
#* @get /secret_data
function(username, password) {
  if (username == "your_name" & password == "secret") {
    return("Here's some secret data")
  } else {
    api_error("Wrong username or password!", 401)
  }
}

#* Slightly better secret thing
#* @tag Authentication
#* @seralizer text
#* @post /secret_data_better
function(username, password) {
  if (username == "your_name" & password == "secret") {
    return("Here's some secret data")
  } else {
    api_error("Wrong username or password!", 401)
  }
}

#* Get a token
#* @tag Authentication
#* @post /get_token
function(req, res, username = "", password = "") {
  if (username == Sys.getenv("API_USERNAME") & password == Sys.getenv("API_PASSWORD")) {
    # If the user submits the correct login details, generate a token for them
    claim <- jwt_claim(valid_user = TRUE)
    key <- charToRaw(Sys.getenv("API_JWT_SECRET"))
    jwt <- jwt_encode_hmac(claim, key)

    return(list(token = jwt))
  } else {
    api_error(message = "Invalid username or password", status = 401)
  }
}

# Function for making an endpoint require a token. This isn't an endpoint or
# filter or anything special. Use it inside an endpoint.
require_token <- function(req, res, manual_token) {
  if (!is.na(manual_token)) {
    # If a manual token is passed, use that
    token <- as.character(manual_token)
  } else {
    # Otherwise use the one in the HTTP header
    token <- req$HTTP_AUTHORIZATION |> str_remove("^Bearer ")
  }

  # If there isn't a token, that's wrong
  if (is.null(token) || length(token) == 0) {
    api_error(message = "No token provided", status = 401)
  }

  # Decode the token. If it matches what's on the server, yay. If not, it's wrong.
  tryCatch({
    jwt_decode_hmac(token, secret = Sys.getenv("API_JWT_SECRET"))
    return(TRUE)
  }, error = function(e) {
    api_error(message = "Token is wrong", status = 401)
  })
}

#* JWT secret thing
#* @tag Authentication
#* @seralizer text
#* @post /secret_data_jwt
function(req, res, manual_token = NA) {
  require_token(req, res, manual_token)

  return("Here's some secret data")
}
