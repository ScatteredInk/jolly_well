library(shiny)
library(httr)
library(dplyr)
library(lubridate)

development <- TRUE

key_from_environ <- function(keyname) {
  key <- Sys.getenv(keyname)
  if (identical(key, "")) {
    stop(sprintf("Please set env var %s in your .Renviron file", keyname),
         call. = FALSE)
  }
  key
}

hansard_search <- function(search_term, key = twfy_api_key, page = "1") {
  base_hansard_url <- "https://www.theyworkforyou.com"
  url <- modify_url(base_hansard_url, path = paste("api/getHansard?key=", key,
                                                   "&search=", search_term,
                                                   "&page=", page,
                                                   "&output=js", 
                                                   collapse = "", sep = ""))
  
  resp <- GET(url, ua)
  
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

get_all_hansard_search <- function(search_term, key = twfy_api_key) {
  first_result <- hansard_search(search_term, key)
  num_pages <- ceiling(first_result[[1]]$total_results/20)
  if (num_pages == 1) return (first_result)
  
  hansard_results <- list()
  hansard_results[[1]] <- first_result[["rows"]]
  
  for (page in seq(2, num_pages)) {
    res <- hansard_search(search_term, key, page = page)
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

ua <- user_agent("http://github.com/scatteredink")
api_query = "%22jolly+well%22"
twfy_api_key <- key_from_environ("TWFY_API_KEY")

if (development) {
  process_results <- readRDS("pr_cache.RDS")
} else {
  results <- get_all_hansard_search(api_query)
  process_results <- do.call(rbind, (lapply(results, process_result_df)))
  process_results$extract <- as.character(process_results$extract)
  process_results$date <- ymd(process_results$hdate)
  process_results <- process_results %>%
    filter(!is.na(name))
}


server <- function(input, output) {

  output$speaker_extract <- renderUI(lapply(1:nrow(process_results),
                                            function(i) {
                                              tags$div(class=paste("my-row", gsub(" ", "", as.character(process_results$party[i]))),
                                                tags$div(class="col-sm-4 jpanel", process_results$name[i]),
                                              tags$div(class="col-sm-8 jpanel", HTML(process_results$extract[i])) 
                                                         )}))
  
}

ui <- fixedPage(
  includeCSS("www/cosmo.css"),
  includeCSS("www/jolly.css"),
  titlePanel("Jolly Well"),
  fixedRow(
    column(12,
       htmlOutput("speaker_extract")
        )
    )
  )

shinyApp(ui = ui, server = server)
