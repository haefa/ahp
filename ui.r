library(shiny)
library(shinycssloaders)
library(shinythemes)

navbarPage("Analytical Hierarchy Process",
           theme = shinytheme("cerulean"),
           tabPanel("Susunan Hierarki",
                    fluidRow(
                      column(4,
                             textInput("criteria", "Tambah Kriteria:"),
                             actionButton("addCriteria", "Tambahkan", class = "btn-primary"),
                             br(),
                             br(),
                             htmlOutput("outputCriteria")
                      ),
                      column(4,
                             textInput("subcriteria", "Tambah Sub-Kriteria:"),
                             selectInput("sub-c", "Pilih Kriteria",
                                         c("Typical angina" = 1,
                                           "Atypical angina" = 2,
                                           "Non-anginal pain" = 3,
                                           "Asymptomatic" = 4
                                         )),
                             actionButton("addSubCriteria", "Tambahkan", class = "btn-primary") 
                      ),
                      column(4,
                             textInput("alternative", "Tambah Alternatif:"),
                             actionButton("addAlternative", "Tambahkan", class = "btn-primary") 
                      )
                    )   
           ),
           tabPanel("Tambah Record",
                    mainPanel(
                      h3('Pakar'),
                      textInput("criteria", "Nama Pakar:"),
                      h3('Perbandingan'),
                      sliderInput("range", 
                                  label = "Kriteria 1 vs Kriteria 2",
                                  -9, 9, value = 0, step = 1),
                      sliderInput("range", 
                                  label = "Kriteria 2 vs Kriteria 3",
                                  -9, 9, value = 0, step = 1),
                      sliderInput("range", 
                                  label = "Kriteria 1 vs Kriteria 3",
                                  -9, 9, value = 0, step = 1),
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
