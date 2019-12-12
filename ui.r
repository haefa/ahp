library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shinyWidgets)

arr = c("9", "8", "7", "6", "5", "4", "3", "2", "1", "2", "3", "4", "5", "6", "7", "8", "9")
navbarPage("Analytical Hierarchy Process",
           theme = shinytheme("cerulean"),
           tabPanel("Tambah Pakar",
                    fluidRow(
                      column(4,
                             textInput("namaPakar", "Nama Pakar:"),
                             textInput("jabatanPakar", "Jabatan Pakar:"),
                             actionButton("addPakar", "Tambahkan", class = "btn-primary"),
                             verbatimTextOutput("outputCriteria")
                      ),
                      column(6,
                             DT::dataTableOutput("responses", width = 500)
                      )
                    )   
           ),
           tabPanel("Tambah Record",
                    sidebarPanel(
                      selectInput('comparison', 'Pilih Pakar', names(iris)),
                      selectInput('comparison', 'Pilih Perbandingan', names(iris))
                    ),
                    mainPanel(
                      sliderTextInput(
                        inputId = "slider1",
                        label = "Kriteria 1 vs Kriteria 2",
                        choices = arr,
                        selected = arr[9]
                      ),
                      sliderTextInput(
                        inputId = "slider2",
                        label = "Kriteria 2 vs Kriteria 3",
                        choices = arr,
                        selected = arr[9]
                      ),
                      sliderTextInput(
                        inputId = "slider3",
                        label = "Kriteria 1 vs Kriteria 3",
                        choices = arr,
                        selected = arr[9]
                      ),
                      actionButton("addRecord", "Tambahkan Record", class = "btn-primary"),
                      textOutput("selected_var")
                    )  
           ),
           tabPanel("Hasil Agregasi",
                    h3("Rank Kriteria"),
                    p("Kriteria 1"),
                    p("Kriteria 2"),
                    p("Kriteria 3"),
                    h3("Rank Alternatif"),
                    p("Alternatif 1"),
                    p("Alternatif 2"),
                    p("Alternatif 3")
           )
)
