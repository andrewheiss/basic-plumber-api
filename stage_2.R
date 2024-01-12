library(tidyverse)
library(glue)
library(plumber)

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
