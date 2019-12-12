fields <- c("namaPakar", "jabatanPakar")

saveData <- function(data) {
  data <- as.data.frame(t(data))
  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}

loadData <- function() {
  if (exists("responses")) {
    responses
  }
}


server <- function(input, output) {
  criteria <- NULL
  observeEvent(input$addCriteria, {
    i <- length(criteria)+1
    criteria[i] <- input$criteria
    #output$outputCriteria <- renderUI({
    #  lapply(1:length(criteria), function(i) {tags$li(criteria[i])})
    #})
  })
  
  formData <- reactive({
    datas <- c(input$namaPakar, input$jabatanPakar)
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  namaPakar <- reactive(({
    namaPakar
  }))
  
  observeEvent(input$addPakar, {
    saveData(formData())
  })
  
  output$responses <- DT::renderDataTable({
    input$addPakar
    loadData()
  })
  
}