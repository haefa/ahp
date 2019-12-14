fields <- c("namaPakar", "jabatanPakar")
arr <- c("1/9", "1/8", "1/7", "1/6", "1/5", "1/4", "1/3", "1/2", "1", "2", "3", "4", "5", "6", "7", "8", "9")

outputDir <- "C:/Users/Eng-Ryan/Documents/Kuliah/ahp"
filePath <- file.path(outputDir, "expertData.csv")

saveData <- function(data) {
  data <- t(data)
  data <- data
  # Create a unique file name
  fileName <- "expertData.csv"
  dir.create(file.path(outputDir), showWarnings = FALSE)
  # Write the file to the local system
  
  write.table(data, filePath, sep = ",", col.names = !file.exists(filePath), append = T)
}

loadData <- function() {
  if (file.exists(filePath)) {
    responses <- read.csv(filePath, header = T, row.names = NULL )
    namaPakar <<- levels(responses$namaPakar)
    namaPakar <<- as.data.frame(namaPakar)
    namaPakar
    responses$row.names <- NULL
    responses
  }
  else responses <- NULL
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
  
  output$perbandingan <- reactive({
    perbandingan <- c("Semua Kriteria", "Kriteria Approval Pemerintah", "Kriteria Sumber Daya", "Kriteria Stakeholder", "Kriteria Dukungan Publik",
                      "Subkriteria Administrasi", "Subkriteria Hukum", "Subkriteria Politik", "Subkriteria Teknologi", "Subkriteria Ahli", "Subkriteria Finansial"                  
    )
  })
  
  slider <- reactive({
    if(input$selectPerbandingan == "Kriteria Approval Pemerintah"){
      slider<-c("Subkriteria Administrasi vs Subkriteria Hukum", "Subkriteria Administrasi vs Subkriteria Politik", "Subkriteria Hukum vs Politik"
                )
      lapply(1:3, function(i) {
        sliderTextInput(
          inputId = paste0("slider", i),
          label = paste0(slider[i]),
          choices = arr,
          selected = arr[9]
        )})
      } else if(input$selectPerbandingan == "Kriteria Sumber Daya"){
        slider<-c("Subkriteria Teknologi vs Subkriteria Ahli", "Subkriteria Teknologi vs Subkriteria Finansial", "Subkriteria Ahli vs Finansial"
        )
        lapply(1:3, function(i) {
          sliderTextInput(
            inputId = paste0("slider", i),
            label = paste0(slider[i]),
            choices = arr,
            selected = arr[9]
          )})
      } else if(input$selectPerbandingan == "Semua Kriteria"){
        slider<-c("Kriteria Approval Pemerintah vs Kriteria Sumber Daya", "Kriteria Approval Pemerintah vs Kriteria Penolakan Stakeholder", "Kriteria Approval Pemerintah vs Kriteria Dukungan Publik",
                  "Kriteria Sumber Daya vs Kriteria Penolakan Stakeholder", "Kriteria Sumber Daya vs Kriteria Dukungan Publik", "Kriteria Penolakan Stakeholder vs Kriteria Dukungan Publik"
        )
        lapply(1:6, function(i) {
          sliderTextInput(
            inputId = paste0("slider", i),
            label = paste0(slider[i]),
            choices = arr,
            selected = arr[9]
          )})
      } else {
        slider<-c("Alternatif Mengurangi Dampak vs Alternatif Reforestasi", "Alternatif Mengurangi Dampak vs Alternatif Proteksi", "Alternatif Reforestasi vs Alternatif Proteksi"
        )
        lapply(1:3, function(i) {
          sliderTextInput(
            inputId = paste0("slider", i),
            label = paste0(slider[i]),
            choices = arr,
            selected = arr[9]
          )})
    }
  })
  
  output$slider_ui <- renderUI({ slider() })
  
  sliderValues <- reactive({
    
    data.frame(
      Name = c("slider1",
               "slider2",
               "slider3"),
      Value = as.character(c(input$slider1,
                             input$slider2,
                             input$slider3)),
      stringsAsFactors = FALSE)
    
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}