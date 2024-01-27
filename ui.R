#Created by: Anita Mila Oktafani
#Dosen Pembimbing:
#     Iut Tri Utami, S.Si., M.Si.
#     Puspita Kartikasari, S.Si., M.Si.

ui <- fluidPage(theme = shinytheme("flatly"),
                #Judul Aplikasi
                titlePanel("Aplikasi RShiny untuk Klasifikasi Penyakit ISPA Pada Balita"),
                h4("Naive Bayes Classifier dengan seleksi variabel Backward Elimination"),
                navbarPage("App",

                  #Tab Home
                  tabPanel("Home",
                           br(),
                           div(h3("Aplikasi RShiny untuk Klasifikasi Penyakit ISPA Pada Balita",
                                  style="text-align:center")),
                           div(h4("Menggunakan Algoritma Naive Bayes Classifier dengan seleksi variabel Backward Elimination",
                                  style="text-align:center")),
                           br(),
                           br(),
                           h5("Created by:", style="text-align:center"),
                           h5("Iut Tri Utami, S.Si., M.Si.",
                              style="text-align:center"),
                           h5("Puspita Kartikasari, S.Si., M.Si.",
                              style="text-align:center"),
                           h5("Anita Mila Oktafani",
                              style="text-align:center"),
                           br(),
                           br(),
                           h5("Departemen Statistika",
                              style="text-align:center"),
                           h5("Fakultas Sains dan Matematika",
                              style="text-align:center"),
                           h5("Universitas Diponegoro",
                              style="text-align:center"),
                           h5("2023",
                              style="text-align:center")
                           
                           ), #kurung Home
                  
                  #Tab Database
                  tabPanel("Data Awal",
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("data", "Choose CSV File",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               #Horizontal line
                               tags$hr(),
                               
                               #Input checkbox header
                               checkboxInput("header",
                                             "Header",
                                             TRUE),
                               
                               radioButtons("sep", "Separator",
                                            choices = c(Comma=",",
                                                        Semicolon=";",
                                                        Tab="\t"),
                                            selected = ","),
                               
                               radioButtons("quote", "Quote",
                                            choices = c(None="",
                                                        "Double Quote"='"',
                                                        "Single Quote"="'"),
                                            selected = '"'),
                               
                               #Horizontal line
                               tags$hr(),
                               
                               radioButtons("disp",
                                            "Display",
                                            choices = c(Head = "head",
                                                        All = "all"),
                                            selected = "head")
                             ), #kurung sidebarpanel
                             mainPanel(DTOutput("data")),
                             position = "left"
                           ) #kurung sidebarlayout
                           ), #kurung Input Data
                  
                  #Tab Backward Elimination
                  tabPanel("Seleksi Fitur",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Seleksi Fitur Backward Elimination",
                                  style="text-align:left"),
                               actionButton("run6", "RUN"),
                             ), #kurung sidebarpanel
                             
                             mainPanel(verbatimTextOutput("hasil.be")
                             ), #kurung mainpanel
                           ), #kurung sidebarlayout
                  ), #kurung Seleksi Fitur
                  
                  #Tab NBC
                  tabPanel("Naive Bayes Classifier",
                           sidebarLayout(
                             sidebarPanel(
                               h4("NBC",
                                  style="text-align:left"),
                               numericInput("set.seed",
                                            "Input random seed:",
                                            100),
                               
                               actionButton("run", "RUN"),
                               h4("NBC Backward",
                                  style="text-align:left"),
                               numericInput("set.seed",
                                            "Input random seed:",
                                            100),
                               
                               actionButton("run3", "RUN")
                             ), #kurung sidebarpanel
                             mainPanel(
                               tabsetPanel(type = "tabs",
                                           tabPanel("NBC",
                                                    verbatimTextOutput("klasifikasi.nbc")),
                                           tabPanel("NBC Backward",
                                                    verbatimTextOutput("klasifikasi.nbcb"))
                                           )
                             ), #kurung mainpanel
                           ), #kurung sidebarlayout
                           
                           ), #kurung NBC
                  
                  #Tab Kinerja Klasifikasi
                  tabPanel("Kinerja Klasifikasi",
                           sidebarLayout(
                             sidebarPanel(
                               h4("Matriks Konfusi NBC",
                                  style="text-align:left"),
                               actionButton("run2", "PRINT"),
                               tags$br(),
                               h4("Matriks Konfusi NBC Backward",
                                  style="text-align:left"),
                               actionButton("run4", "PRINT"),
                               tags$br(),
                             ), #kurung sidebarpanel
                             mainPanel(
                               tabsetPanel(type = "tabs",
                                           tabPanel("Matriks Konfusi NBC",
                                                    verbatimTextOutput("conf.nbc")),
                                           tabPanel("Matriks Konfusi NBC Backward",
                                                    verbatimTextOutput("conf.nbcb"))
                                           )
                             ), #kurung mainpanel
                           ) #kurung sidebarlayout
                           ), #kurung Akurasi
                  
                  #Tab Prediksi
                  tabPanel("Prediksi",
                           sidebarLayout(
                             sidebarPanel(
                               fileInput("data2", "Choose CSV File",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               #Horizontal line
                               tags$hr(),
                               
                               #Input checkbox header
                               checkboxInput("header",
                                             "Header",
                                             TRUE),
                               
                               radioButtons("sep", "Separator",
                                            choices = c(Comma=",",
                                                        Semicolon=";",
                                                        Tab="\t"),
                                            selected = ","),
                               
                               radioButtons("quote", "Quote",
                                            choices = c(None="",
                                                        "Double Quote"='"',
                                                        "Single Quote"="'"),
                                            selected = '"'),
                               
                               #Horizontal line
                               tags$hr(),
                               
                               radioButtons("disp",
                                            "Display",
                                            choices = c(Head = "head",
                                                        All = "all"),
                                            selected = "head"),
                               
                               actionButton("run5","RUN"),
                             ), #kurung sidebarpanel
                             mainPanel(
                               tabsetPanel(type = "tabs",
                                           tabPanel("Data",
                                                    DTOutput("data2")),
                                           tabPanel("Hasil Prediksi",
                                                    verbatimTextOutput("prediksi.nbc"))
                             ) #kurung tabsetpanel
                           ) #kurung mainpanel
                           ) #kurung sidebarlayout
                           ) #kurung Prediksi
                ) #kurung navbarpage
) #kurung fluidpage