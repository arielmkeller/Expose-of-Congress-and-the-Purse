library(httr2)
library(dplyr)
library(tibble)

get_legislators_reference <- function() {
  req <- request("https://www.govtrack.us/api/v2/role") |>
    req_url_query(current = "true", limit = 600)

  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp)) {
    return(fallback_legislators_reference())
  }

  payload <- tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(payload) || is.null(payload$objects) || length(payload$objects) == 0) {
    return(fallback_legislators_reference())
  }

  rows <- lapply(payload$objects, function(x) {
    list(
      legislator = as.character(x$person$name),
      party = as.character(x$party),
      state = as.character(x$state),
      chamber = ifelse(as.character(x$role_type) == "senator", "Senate", "House")
    )
  })

  out <- bind_rows(rows) |>
    filter(!is.na(legislator), legislator != "") |>
    distinct(legislator, .keep_all = TRUE) |>
    arrange(legislator)

  if (nrow(out) == 0) {
    return(fallback_legislators_reference())
  }

  out
}

fallback_legislators_reference <- function() {
  tibble(
    legislator = c("Alexandria Ocasio-Cortez", "Chuck Schumer", "Ted Cruz", "Cory Booker"),
    party = c("Democrat", "Democrat", "Republican", "Democrat"),
    state = c("NY", "NY", "TX", "NJ"),
    chamber = c("House", "Senate", "Senate", "Senate")
  )
}
