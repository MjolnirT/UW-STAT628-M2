library(shiny)
library(shinythemes)
shinyUI(fluidPage( theme=shinytheme("readable"), 
                   titlePanel(paste("Get male body fat in 2 seconds!")),
                   fluidRow(
                     column(4,
                            h4("Step1"),
                            p("Choose unit of measurement")
                     ),
                     column(4,
                            h4("Step2"),
                            p("Input you weight and Abdomen Circumference")
                     ),
                     column(4,
                            h4("Step3"),
                            p("Press 'Get you Bodyfat!' ")
                            
                     )
                   ),
                   sidebarPanel( selectInput("WeightUnit", "Weight Measurement Unit", c(Pounds = "lbs", Kilogram = "kg")),
                                 numericInput("weight", label = "Weight", value = 100),
                                 selectInput("AbdomenUnit", "Abdomen Measurement Unit:",c(Inches = "inches", Centimeter = "cm")),
                                 numericInput("abdomen", label = "Abdomen Circumference", value = 90),
                                 submitButton("Get you Bodyfat!"),
                                 tags$hr(),
                                 helpText("Thanks for using it!"),
                                 helpText("If there exists a problem, feel free to contact us!"),
                                 helpText("E-mail: jmiao24@wisc.edu")),
                   mainPanel(
                     tabsetPanel(
                       tabPanel("Results",
                                h4(textOutput("Bodyfat_P")),
                                h5(textOutput("Test_BDFat")),
                                h5(textOutput("text_out")),
                                plotOutput("plot"),
                                uiOutput("tab"))))
                   
  )
)