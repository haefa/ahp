server <- function(input, output) {
  criteria <- NULL
  observeEvent(input$addCriteria, {
    i <- length(criteria)+1
    criteria[i] <- input$criteria
    output$outputCriteria <- renderUI({
      lapply(1:length(criteria), function(i) {tags$li(criteria[i])})
    })
  })
  
}