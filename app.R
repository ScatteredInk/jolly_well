library(shiny)
library(shinyjs)

source("twfy_api.R")

Sys.setenv("BUILD" = "PRODUCTION")
#Sys.setenv("BUILD" = "DEVELOP")
develop_build <- ifelse(Sys.getenv("BUILD")=="DEVELOP", TRUE, FALSE)

ua <- user_agent("http://github.com/scatteredink")
api_query = "%22jolly+well%22"
twfy_api_key <- key_from_environ("TWFY_API_KEY")

if (develop_build) {
  process_results <- readRDS("pr_cache.RDS")
} else {
  results <- get_all_hansard_search(api_query, twfy_api_key, ua)
  process_results <- do.call(rbind, (lapply(results, process_result_df)))
  process_results$extract <- as.character(process_results$extract)
  process_results$date <- ymd(process_results$hdate)
  other_parties <-  setdiff(unique(process_results$party), 
                                   c("Conservative", "Labour", "Liberal Democrat", "Plaid Cymru")) 
  process_results <- process_results %>%
    filter(!is.na(name)) %>%
    mutate(party = fct_collapse(party, Other = other_parties))
}


server <- function(input, output) {
  

  output$speaker_extract <- renderUI(lapply(1:nrow(process_results),
                                            function(i) {
                                              tags$tr(class=gsub(" ", "", as.character(process_results$party[i])),
                                                tags$th(class="thname", tags$h4(process_results$name[i], align="center")),
                                              tags$th(class="thextract", HTML(process_results$extract[i])) 
                                                         )}))
  observeEvent(input$aboutBtn, {
    toggle("data")
    toggle("about")
    toggle("aboutBtn")
    toggle("dataBtn")
  })
  
  observeEvent(input$dataBtn, {
    toggle("data")
    toggle("about")
    toggle("aboutBtn")
    toggle("dataBtn")
  })

  
}

ui <- fixedPage(
  useShinyjs(),
  includeCSS("www/cosmo.css"),
  includeCSS("www/jolly.css"),
  tags$h2("Jolly Well", align="center"),

    column(12,
        fixedRow(actionButton("aboutBtn", "About"),
                 hidden(actionButton("dataBtn", "Data"))),
        hidden(tags$div(class="row", id="about",
          column(3),
          column(6, fixedRow(tags$div("Jolly Well is a measuring device for the malaise of post-imperial nostalgia.")),
                 fixedRow(tags$a(href="https://github.com/ScatteredInk/jolly_well", target = "_blank", 
                                 tags$img(src = "images/github.png", class = "centerimg", alt="Github logo")))),
          column(3))),
        tags$div(class="row", id="data",
              htmlOutput("speaker_extract", inline = FALSE)
          )
      )
)
      
shinyApp(ui = ui, server = server)
