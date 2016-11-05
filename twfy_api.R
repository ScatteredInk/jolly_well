library(httr)
library(dplyr)
library(lubridate)
library(forcats)

key_from_environ <- function(keyname) {
  key <- Sys.getenv(keyname)
  if (identical(key, "")) {
    stop(sprintf("Please set env var %s in your .Renviron file", keyname),
         call. = FALSE)
  }
  key
}

hansard_search <- function(search_term, key, ua, page = "1") {
  base_hansard_url <- "https://www.theyworkforyou.com"
  url <- modify_url(base_hansard_url, path = paste("api/getHansard?key=", key,
                                                   "&search=", search_term,
                                                   "&page=", page,
                                                   "&output=js", 
                                                   collapse = "", sep = ""))
  
  resp <- GET(url,  ua)
  
  #if (http_type(resp) != "application/json") {
  #  stop("API did not return json", call. = FALSE)
  #}
  
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyDataFrame =TRUE)
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "They Work For You API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  structure(
    list(
      content = parsed,
      path = search_term,
      response = resp
    ),
    class = "they_work_for_you_api"
  )
  
  
  parsed
}

get_all_hansard_search <- function(search_term, key, ua) {
  first_result <- hansard_search(search_term, key = key, ua = ua)
  num_pages <- ceiling(first_result[[1]]$total_results/20)
  if (num_pages == 1) return (first_result)
  
  hansard_results <- list()
  hansard_results[[1]] <- first_result[["rows"]]
  
  for (page in seq(2, num_pages)) {
    res <- hansard_search(search_term, key = key, ua = ua, page = page)
    hansard_results[[page]] <- res[["rows"]]
  }
  
  hansard_results
}

process_result_df <- function(df) {
  speaker_df <- df$speaker %>%
    select(member_id, house, constituency, 
           party, name)
  debate_df <- df %>%
    select(hdate, extract)
  extract_df <- df$parent %>%
    select("debate" = body)
  as.data.frame(do.call(cbind, c(speaker_df, debate_df, extract_df)))
}