fields <- c("namaPakar", "jabatanPakar")
arr <- c("1/9", "1/8", "1/7", "1/6", "1/5", "1/4", "1/3", "1/2", "1", "2", "3", "4", "5", "6", "7", "8", "9")

outputDir <- "C:/Users/Eng-Ryan/Documents/Kuliah/ahp"
filePath <- file.path(outputDir, "expertData.csv")
filePath2 <- file.path(outputDir, "recordData.csv")

saveData <- function(data) {
  data <- t(data)
  data <- data
  # Create a unique file name
  fileName <- "expertData.csv"
  dir.create(file.path(outputDir), showWarnings = FALSE)
  # Write the file to the local system
  
  write.table(data, filePath, sep = ",", col.names = !file.exists(filePath), append = T)
}

saveRecord <- function(data){
  data <- t(data)
  data <- data
  # Create a unique file name
  fileName <- "recordData.csv"
  dir.create(file.path(outputDir), showWarnings = FALSE)
  # Write the file to the local system
  
  write.table(data, filePath2, sep = ",", col.names = !file.exists(filePath2), append = T)
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

convertString <- function(x){
  if(x=="1"){
    return(1)
  } else if(x=="2"){
    return(2)
  } else if(x=="3"){
    return(3)
  } else if(x=="4"){
    return(4)
  } else if(x=="5"){
    return(5)
  } else if(x=="6"){
    return(6)
  } else if(x=="7"){
    return(7)
  } else if(x=="8"){
    return(8)
  } else if(x=="9"){
    return(9)
  } else if(x=="1/2"){
    return(1/2)
  } else if(x=="1/3"){
    return(1/3)
  } else if(x=="1/4"){
    return(1/4)
  } else if(x=="1/5"){
    return(1/5)
  } else if(x=="1/6"){
    return(1/6)
  } else if(x=="1/7"){
    return(1/7)
  } else if(x=="1/8"){
    return(1/8)
  } else if(x=="1/9"){
    return(1/9)
  }
}

server <- function(input, output, session) {
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
  
  formRecord <- reactive({
    data1 <<- c(input$namaPakar,
               input$jabatanPakar,
               1,
               convertString(input$goal1),
               convertString(input$goal2),
               convertString(input$goal3),
               1/convertString(input$goal1),
               1,
               convertString(input$goal4),
               convertString(input$goal5),
               1/convertString(input$goal2),
               1/convertString(input$goal4),
               1,
               convertString(input$goal6),
               1/convertString(input$goal3),
               1/convertString(input$goal5),
               1/convertString(input$goal6),
               1)
    data2 <<- c(
      1,
      convertString(input$kriteria11),
      convertString(input$kriteria12),
      1/convertString(input$kriteria11),
      1,
      convertString(input$kriteria13),
      1/convertString(input$kriteria12),
      1/convertString(input$kriteria13),
      1
    )
    data3 <<- c(
      1,
      convertString(input$subkriteria111),
      convertString(input$subkriteria112),
      1/convertString(input$subkriteria111),
      1,
      convertString(input$subkriteria113),
      1/convertString(input$subkriteria112),
      1/convertString(input$subkriteria113),
      1
    )
    data4 <<- c(
      1,
      convertString(input$subkriteria121),
      convertString(input$subkriteria122),
      1/convertString(input$subkriteria121),
      1,
      convertString(input$subkriteria123),
      1/convertString(input$subkriteria122),
      1/convertString(input$subkriteria123),
      1
    )
    data5 <<- c(
      1,
      convertString(input$subkriteria131),
      convertString(input$subkriteria132),
      1/convertString(input$subkriteria131),
      1,
      convertString(input$subkriteria133),
      1/convertString(input$subkriteria132),
      1/convertString(input$subkriteria133),
      1
    )
    data6 <<- c(
      1,
      convertString(input$kriteria21),
      convertString(input$kriteria22),
      1/convertString(input$kriteria21),
      1,
      convertString(input$kriteria23),
      1/convertString(input$kriteria22),
      1/convertString(input$kriteria23),
      1
    )
    data7 <<- c(
      1,
      convertString(input$subkriteria211),
      convertString(input$subkriteria212),
      1/convertString(input$subkriteria211),
      1,
      convertString(input$subkriteria213),
      1/convertString(input$subkriteria212),
      1/convertString(input$subkriteria213),
      1
    )
    data8 <<- c(
      1,
      convertString(input$subkriteria221),
      convertString(input$subkriteria222),
      1/convertString(input$subkriteria221),
      1,
      convertString(input$subkriteria223),
      1/convertString(input$subkriteria222),
      1/convertString(input$subkriteria223),
      1
    )
    data9 <<- c(
      1,
      convertString(input$subkriteria231),
      convertString(input$subkriteria232),
      1/convertString(input$subkriteria231),
      1,
      convertString(input$subkriteria233),
      1/convertString(input$subkriteria232),
      1/convertString(input$subkriteria233),
      1
    )
    data10 <<- c(
      1,
      convertString(input$kriteria31),
      convertString(input$kriteria32),
      1/convertString(input$kriteria31),
      1,
      convertString(input$kriteria33),
      1/convertString(input$kriteria32),
      1/convertString(input$kriteria33),
      1
    )
    data11 <<- c(
      1,
      convertString(input$kriteria41),
      convertString(input$kriteria42),
      1/convertString(input$kriteria41),
      1,
      convertString(input$kriteria43),
      1/convertString(input$kriteria42),
      1/convertString(input$kriteria43),
      1
    )
    
    #data <- c(input$selectPakar, input$selectPerbandingan, input$slider1, input$slider2, input$slider3)
    data <- c(data1, data2, data3, data4, data5, data6, data7, data8, data9, data10, data11)
    data
  })
  
  observeEvent(input$addPakar, {
    saveData(formData())
  })
  
  observeEvent(input$addRecord, {
    saveRecord(formRecord())
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
        if(length(data2) > 1){
          if(i==1) {
            x <- data2[2]
          } else if(i==2) {
            x <- data2[3]
          } else {
            x <- data2[6]  
          }
          
          if(x<1){
            if(x==1/2)
              x <- "1/2"
            else if(x==1/3)
              x <- "1/3"
            else if(x==1/4)
              x <- "1/4"
            else if(x==1/5)
              x <- "1/5"
            else if(x==1/6)
              x <- "1/6"
            else if(x==1/7)
              x <- "1/7"
            else if(x==1/8)
              x <- "1/8"
            else if(x==1/9)
              x <- "1/9"
          } else {
            x <- as.character(x)
          }
          
          updateSliderTextInput(
            session,
            inputId = paste0("kriteria1", i),
            choices = arr,
            selected = arr[which(arr==x)]
          )
        } else {
          x <- 1
        }
        
        
        sliderTextInput(
          inputId = paste0("kriteria1", i),
          label = paste0(slider[i]),
          choices = arr,
          selected = arr[which(arr==as.character(x))]
        )})
      } else if(input$selectPerbandingan == "Kriteria Sumber Daya"){
        slider<-c("Subkriteria Teknologi vs Subkriteria Ahli", "Subkriteria Teknologi vs Subkriteria Finansial", "Subkriteria Ahli vs Finansial"
        )
        lapply(1:3, function(i) {
          if(length(data6) > 1){
            if(i==1) {
              x <- data6[2]
            } else if(i==2) {
              x <- data6[3]
            } else {
              x <- data6[6]  
            }
            
            if(x<1){
              if(x==1/2)
                x <- "1/2"
              else if(x==1/3)
                x <- "1/3"
              else if(x==1/4)
                x <- "1/4"
              else if(x==1/5)
                x <- "1/5"
              else if(x==1/6)
                x <- "1/6"
              else if(x==1/7)
                x <- "1/7"
              else if(x==1/8)
                x <- "1/8"
              else if(x==1/9)
                x <- "1/9"
            } else {
              x <- as.character(x)
            }
            
            updateSliderTextInput(
              session,
              inputId = paste0("kriteria2", i),
              choices = arr,
              selected = arr[which(arr==x)]
            )
          } else {
            x <- 1
          }
          
          sliderTextInput(
            inputId = paste0("kriteria2", i),
            label = paste0(slider[i]),
            choices = arr,
            selected = arr[which(arr==as.character(x))]
          )})
      } else if(input$selectPerbandingan == "Semua Kriteria"){
        slider<-c("Kriteria Approval Pemerintah vs Kriteria Sumber Daya", "Kriteria Approval Pemerintah vs Kriteria Penolakan Stakeholder", "Kriteria Approval Pemerintah vs Kriteria Dukungan Publik",
                  "Kriteria Sumber Daya vs Kriteria Penolakan Stakeholder", "Kriteria Sumber Daya vs Kriteria Dukungan Publik", "Kriteria Penolakan Stakeholder vs Kriteria Dukungan Publik"
        )
        lapply(1:6, function(i) {
          if(length(data1) > 1){
            if(i==1) {
              x <- data1[2+2]
            } else if(i==2) {
              x <- data1[2+3]
            } else if(i==3){
              x <- data1[2+4]  
            } else if(i==4) {
              x <- data1[2+7]
            } else if(i==5){
              x <- data1[2+8]  
            } else {
              x <- data1[2+12]
            }
            
            if(x<1){
              if(x==1/2)
                x <- "1/2"
              else if(x==1/3)
                x <- "1/3"
              else if(x==1/4)
                x <- "1/4"
              else if(x==1/5)
                x <- "1/5"
              else if(x==1/6)
                x <- "1/6"
              else if(x==1/7)
                x <- "1/7"
              else if(x==1/8)
                x <- "1/8"
              else if(x==1/9)
                x <- "1/9"
            } else {
              x <- as.character(x)
            }
            
           updateSliderTextInput(
             session,
             inputId = paste0("goal", i),
             choices = arr,
             selected = arr[which(arr==x)]
           )
          } else {
            x <- 1
          }
          
          sliderTextInput(
            inputId = paste0("goal", i),
            label = paste0(slider[i]),
            choices = arr,
            selected = arr[which(arr==as.character(x))]
          )})
      } else {
        slider<-c("Alternatif Mengurangi Dampak vs Alternatif Reforestasi", "Alternatif Mengurangi Dampak vs Alternatif Proteksi", "Alternatif Reforestasi vs Alternatif Proteksi"
        )
        
        if(input$selectPerbandingan == "- Subkriteria Administrasi"){
          lapply(1:3, function(i) {
            if(length(data3) > 1){
              if(i==1) {
                x <- data3[2]
              } else if(i==2) {
                x <- data3[3]
              } else {
                x <- data3[6]  
              }
              
              if(x<1){
                if(x==1/2)
                  x <- "1/2"
                else if(x==1/3)
                  x <- "1/3"
                else if(x==1/4)
                  x <- "1/4"
                else if(x==1/5)
                  x <- "1/5"
                else if(x==1/6)
                  x <- "1/6"
                else if(x==1/7)
                  x <- "1/7"
                else if(x==1/8)
                  x <- "1/8"
                else if(x==1/9)
                  x <- "1/9"
              } else {
                x <- as.character(x)
              }
              
              updateSliderTextInput(
                session,
                inputId = paste0("subkriteria11", i),
                choices = arr,
                selected = arr[which(arr==x)]
              )
            } else {
              x <- 1
            }
            
            sliderTextInput(
              inputId = paste0("subkriteria11", i),
              label = paste0(slider[i]),
              choices = arr,
              selected = arr[which(arr==as.character(x))]
            )})
        } else if(input$selectPerbandingan == "- Subkriteria Hukum"){
          lapply(1:3, function(i) {
            if(length(data4) > 1){
              if(i==1) {
                x <- data4[2]
              } else if(i==2) {
                x <- data4[3]
              } else {
                x <- data4[6]  
              }
              
              if(x<1){
                if(x==1/2)
                  x <- "1/2"
                else if(x==1/3)
                  x <- "1/3"
                else if(x==1/4)
                  x <- "1/4"
                else if(x==1/5)
                  x <- "1/5"
                else if(x==1/6)
                  x <- "1/6"
                else if(x==1/7)
                  x <- "1/7"
                else if(x==1/8)
                  x <- "1/8"
                else if(x==1/9)
                  x <- "1/9"
              } else {
                x <- as.character(x)
              }
              
              updateSliderTextInput(
                session,
                inputId = paste0("subkriteria12", i),
                choices = arr,
                selected = arr[which(arr==x)]
              )
            } else {
              x <- 1
            }
            
            sliderTextInput(
              inputId = paste0("subkriteria12", i),
              label = paste0(slider[i]),
              choices = arr,
              selected = arr[which(arr==as.character(x))]
            )})
        } else if(input$selectPerbandingan == "- Subkriteria Politik"){
          lapply(1:3, function(i) {
            if(length(data5) > 1){
              if(i==1) {
                x <- data5[2]
              } else if(i==2) {
                x <- data5[3]
              } else {
                x <- data5[6]  
              }
              
              if(x<1){
                if(x==1/2)
                  x <- "1/2"
                else if(x==1/3)
                  x <- "1/3"
                else if(x==1/4)
                  x <- "1/4"
                else if(x==1/5)
                  x <- "1/5"
                else if(x==1/6)
                  x <- "1/6"
                else if(x==1/7)
                  x <- "1/7"
                else if(x==1/8)
                  x <- "1/8"
                else if(x==1/9)
                  x <- "1/9"
              } else {
                x <- as.character(x)
              }
              
              updateSliderTextInput(
                session,
                inputId = paste0("subkriteria13", i),
                choices = arr,
                selected = arr[which(arr==x)]
              )
            } else {
              x <- 1
            }
            
            sliderTextInput(
              inputId = paste0("subkriteria13", i),
              label = paste0(slider[i]),
              choices = arr,
              selected = arr[which(arr==as.character(x))]
            )})
        } else if(input$selectPerbandingan == "- Subkriteria Teknologi"){
          lapply(1:3, function(i) {
            if(length(data7) > 1){
              if(i==1) {
                x <- data7[2]
              } else if(i==2) {
                x <- data7[3]
              } else {
                x <- data7[6]  
              }
              
              if(x<1){
                if(x==1/2)
                  x <- "1/2"
                else if(x==1/3)
                  x <- "1/3"
                else if(x==1/4)
                  x <- "1/4"
                else if(x==1/5)
                  x <- "1/5"
                else if(x==1/6)
                  x <- "1/6"
                else if(x==1/7)
                  x <- "1/7"
                else if(x==1/8)
                  x <- "1/8"
                else if(x==1/9)
                  x <- "1/9"
              } else {
                x <- as.character(x)
              }
              
              updateSliderTextInput(
                session,
                inputId = paste0("subkriteria21", i),
                choices = arr,
                selected = arr[which(arr==x)]
              )
            } else {
              x <- 1
            }
            
            sliderTextInput(
              inputId = paste0("subkriteria21", i),
              label = paste0(slider[i]),
              choices = arr,
              selected = arr[which(arr==as.character(x))]
            )})
        } else if(input$selectPerbandingan == "- Subkriteria Ahli"){
          lapply(1:3, function(i) {
            if(length(data8) > 1){
              if(i==1) {
                x <- data8[2]
              } else if(i==2) {
                x <- data8[3]
              } else {
                x <- data8[6]  
              }
              
              if(x<1){
                if(x==1/2)
                  x <- "1/2"
                else if(x==1/3)
                  x <- "1/3"
                else if(x==1/4)
                  x <- "1/4"
                else if(x==1/5)
                  x <- "1/5"
                else if(x==1/6)
                  x <- "1/6"
                else if(x==1/7)
                  x <- "1/7"
                else if(x==1/8)
                  x <- "1/8"
                else if(x==1/9)
                  x <- "1/9"
              } else {
                x <- as.character(x)
              }
              
              updateSliderTextInput(
                session,
                inputId = paste0("subkriteria22", i),
                choices = arr,
                selected = arr[which(arr==x)]
              )
            } else {
              x <- 1
            }
            
            sliderTextInput(
              inputId = paste0("subkriteria22", i),
              label = paste0(slider[i]),
              choices = arr,
              selected = arr[which(arr==as.character(x))]
            )})
        } else if(input$selectPerbandingan == "- Subkriteria Finansial"){
          lapply(1:3, function(i) {
            if(length(data9) > 1){
              if(i==1) {
                x <- data9[2]
              } else if(i==2) {
                x <- data9[3]
              } else {
                x <- data9[6]  
              }
              
              if(x<1){
                if(x==1/2)
                  x <- "1/2"
                else if(x==1/3)
                  x <- "1/3"
                else if(x==1/4)
                  x <- "1/4"
                else if(x==1/5)
                  x <- "1/5"
                else if(x==1/6)
                  x <- "1/6"
                else if(x==1/7)
                  x <- "1/7"
                else if(x==1/8)
                  x <- "1/8"
                else if(x==1/9)
                  x <- "1/9"
              } else {
                x <- as.character(x)
              }
              
              updateSliderTextInput(
                session,
                inputId = paste0("subkriteria23", i),
                choices = arr,
                selected = arr[which(arr==x)]
              )
            } else {
              x <- 1
            }
            
            sliderTextInput(
              inputId = paste0("subkriteria23", i),
              label = paste0(slider[i]),
              choices = arr,
              selected = arr[which(arr==as.character(x))]
            )})
        } else if(input$selectPerbandingan == "Kriteria Penolakan Stakeholder"){
          lapply(1:3, function(i) {
            if(length(data10) > 1){
              if(i==1) {
                x <- data10[2]
              } else if(i==2) {
                x <- data10[3]
              } else {
                x <- data10[6]  
              }
              
              if(x<1){
                if(x==1/2)
                  x <- "1/2"
                else if(x==1/3)
                  x <- "1/3"
                else if(x==1/4)
                  x <- "1/4"
                else if(x==1/5)
                  x <- "1/5"
                else if(x==1/6)
                  x <- "1/6"
                else if(x==1/7)
                  x <- "1/7"
                else if(x==1/8)
                  x <- "1/8"
                else if(x==1/9)
                  x <- "1/9"
              } else {
                x <- as.character(x)
              }
              
              updateSliderTextInput(
                session,
                inputId = paste0("kriteria3", i),
                choices = arr,
                selected = arr[which(arr==x)]
              )
            } else {
              x <- 1
            }
            
            sliderTextInput(
              inputId = paste0("kriteria3", i),
              label = paste0(slider[i]),
              choices = arr,
              selected = arr[which(arr==as.character(x))]
            )})
        } else if(input$selectPerbandingan == "Kriteria Dukungan Publik"){
          lapply(1:3, function(i) {
            if(length(data11) > 1){
              if(i==1) {
                x <- data11[2]
              } else if(i==2) {
                x <- data11[3]
              } else {
                x <- data11[6]  
              }
              
              if(x<1){
                if(x==1/2)
                  x <- "1/2"
                else if(x==1/3)
                  x <- "1/3"
                else if(x==1/4)
                  x <- "1/4"
                else if(x==1/5)
                  x <- "1/5"
                else if(x==1/6)
                  x <- "1/6"
                else if(x==1/7)
                  x <- "1/7"
                else if(x==1/8)
                  x <- "1/8"
                else if(x==1/9)
                  x <- "1/9"
              } else {
                x <- as.character(x)
              }
              
              updateSliderTextInput(
                session,
                inputId = paste0("kriteria4", i),
                choices = arr,
                selected = arr[which(arr==x)]
              )
            } else {
              x <- 1
            }
            
            sliderTextInput(
              inputId = paste0("kriteria4", i),
              label = paste0(slider[i]),
              choices = arr,
              selected = arr[which(arr==as.character(x))]
            )})
        } 
    }
  })
  
  output$slider_ui <- renderUI({ slider() })
  
  sliderValues <- reactive({
    #data <- c(input$kriteria11, input$kriteria12, input$kriteria13)
    data2
  })
  
  
    output$debugPakar <- renderText({
      formRecord()
    })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
}