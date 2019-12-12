fields <- c("namaPakar", "jabatanPakar")
arr <- c("9", "8", "7", "6", "5", "4", "3", "2", "1", "2", "3", "4", "5", "6", "7", "8", "9")

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
  
}