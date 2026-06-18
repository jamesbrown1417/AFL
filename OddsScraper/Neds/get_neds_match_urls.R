library(tidyverse)
library(rvest)
library(httr2)
library(jsonlite)

input_file <- "OddsScraper/Neds/neds_response.json"
output_file <- "OddsScraper/Neds/neds_afl_match_urls.csv"

exit_gracefully <- function(message) {
  message(message)
  quit(status = 0)
}

if (!file.exists(input_file)) {
  exit_gracefully("Neds response JSON is missing (upstream step likely failed): ", input_file)
}

neds_response <- tryCatch(
  jsonlite::fromJSON(input_file, simplifyVector = FALSE),
  error = function(error) {
    exit_gracefully(paste0("Could not parse Neds response JSON: ", conditionMessage(error)))
  }
)

if (is.null(neds_response$events) || length(neds_response$events) == 0) {
  exit_gracefully("Neds response JSON did not contain any events. Exiting gracefully.")
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
  exit_gracefully("No AFL match URLs were found in the Neds response JSON. Exiting gracefully.")
}

write_csv(df, output_file)
message("Saved ", nrow(df), " Neds AFL match URLs to ", output_file)
