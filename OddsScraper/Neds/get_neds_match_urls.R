library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

input_file <- "OddsScraper/Neds/neds_response.json"
output_file <- "OddsScraper/Neds/neds_afl_match_urls.csv"

if (!file.exists(input_file)) {
  stop("Neds response JSON is missing: ", input_file)
}

neds_response <- jsonlite::fromJSON(input_file, simplifyVector = FALSE)

if (is.null(neds_response$events) || length(neds_response$events) == 0) {
  stop("Neds response JSON did not contain any events.")
}

value_or_na <- function(value) {
  if (is.null(value) || length(value) == 0) {
    NA_character_
  } else {
    as.character(value)
  }
}

event_rows <-
  purrr::map_dfr(neds_response$events, function(event) {
    tibble(
      event_name = value_or_na(event$name),
      event_id = value_or_na(event$id),
      competition_name = value_or_na(event$competition$name)
    )
  })

slugify <- function(value) {
  value |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("(^-)|(-$)", "")
}

df <-
  event_rows |>
  filter(!is.na(event_name), !is.na(event_id), !is.na(competition_name)) |>
  filter(str_detect(event_name, "\\s+vs\\s+")) |>
  filter(competition_name == "AFL") |>
  mutate(
    url = paste0(
      "https://www.neds.com.au/sports/australian-rules/afl/",
      slugify(event_name),
      "/",
      event_id
    )
  )

if (nrow(df) == 0) {
  stop("No AFL match URLs were found in the Neds response JSON.")
}

write_csv(df, output_file)
message("Saved ", nrow(df), " Neds AFL match URLs to ", output_file)
