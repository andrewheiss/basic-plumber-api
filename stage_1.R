library(tidyverse)
library(glue)
library(plumber)

options("plumber.port" = 6312)

#* @apiTitle Plumber Example API

#* Plot a fancy histogram
#* @serializer png list(width = 500, height = 300)
#* @get /plot
function(n = 100) {
  # Make sure n isn't ever too big so that the server doesn't crash
  if (n >= 10000) {
    stop("`n` is too big. Use a number less than 10,000.")
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
