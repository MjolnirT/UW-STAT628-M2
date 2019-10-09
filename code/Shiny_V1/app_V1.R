###This is for the shiny app for stat628 module 2

rm(list = ls())
library(shiny)
library(shinythemes)
### x is weight

Get_BDFat = function(x, y) {
    return(-41.9 - 0.123* x + 0.896 * y)
}


Test_BDFat = function(x){
  if (x > 2 & x <= 5) {
    return("Your body fat is essential fat. You are so great!")
  } else if (x > 5 & x <= 13) {
    return("You are athletic. Just keep it!")
  } else if (x > 13 & x <= 17) {
    return("You are fit. Just keep it!")
  } else if (x > 17 & x <= 25) {
    return("You are average. And !")
  } else if ( x > 25 & x <100) {  ### Set a upper bound depends on you
    return("You are obese. We recommend you to lose some weight!")
  } else {
    return("Please double check your input!")
  }
}



ggplot_Text = function(x){
  if (x > 2 & x <= 5) {
    return("Essential fat    : D")
  } else if (x > 5 & x <= 13) {
    return("Athletic    : D")
  } else if (x > 13 & x <= 17) {
    return("Fit    : D ")
  } else if (x > 17 & x <= 25) {
    return("Average    : D")
  } else if ( x > 25 & x <100) {  ### Set a upper bound depends on you
    return("Obese    : ( ")
  } else {
    return("Double check your input!")
  }
}

ui =fluidPage( theme=shinytheme("readable"), 
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


shinyServer = function(input, output) {
  # setwd("/Users/michael/Desktop/Fall_2019/Stat_628/")
  data = read.csv("./clean_dataset.csv")
  
  url <- a("How to reduce your bodyfat ?", href="https://www.healthline.com/nutrition/best-ways-to-burn-fat")
  output$tab <- renderUI({
    tagList("Recommendation:", url)
  })
  
  ### Body fat part
  bodyfat = reactive({
    if (input$WeightUnit == "lbs"){
      w = as.numeric(input$weight)
    } else if (input$WeightUnit == "kgs"){
      w = as.numeric(input$weight) * 2.20462
    }
    if (input$AbdomenUnit == "inches"){
      cir = as.numeric(input$abdomen)
    } else if (input$AbdomenUnit == "cm"){
      cir = as.numeric(input$abdomen) * 2.54
    }
    BDFat = round(Get_BDFat(w,cir),3)
    return(BDFat)
  })
  
  output$Bodyfat_P = renderText({
    if(bodyfat() > 0 & bodyfat() < 100) {
      Res1 = paste0("Your bodyfat percentage is: ", bodyfat(), "%")
    } else {
      Res1 = paste0("Your bodyfat percentage is abnormal")
    }
    return(Res1)
  })
  
  output$Test_BDFat = renderText({
    Res2 = Test_BDFat( bodyfat())
    return(Res2)
  })

  
  Shadow  = data.frame(xmin = c(0,6,13,18,25), xmax = c(6,13,18,25,50), 
                       ymin = -Inf, ymax = Inf, 
                       Region= c("Essential 2%~5%", "Athletes 6%~13%", "Fitness 14%~17%", "Average 18%~25%","Obese 25+%"))
  
  
  output$plot  = renderPlot({
    if (bodyfat()<100 & bodyfat()>0){
      ggplot(data,aes(x = BODYFAT)) + geom_density(size = 0.3, linetype = "dashed") + 
        geom_rect(data = Shadow,inherit.aes=F,aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill= Region))+
        scale_fill_manual("Region",values = adjustcolor(c("#C4B743","#A35029","#82A866","#93A0CB","#2BAA92"),alpha.f = 0.3))+
        xlab("Body fat percentages") + geom_vline(xintercept = bodyfat(),linetype = "dashed",color="#C2151B", size=2) +
        annotate(geom="text",fontface="italic", x = (bodyfat()-6), y =  0.04, label = as.character(paste0(bodyfat(),"%")),size = 6,color ="#C2151B") +
        annotate(geom="text",fontface="italic", x = (bodyfat()+6), y =  0.04, label = ggplot_Text(bodyfat()) ,size = 6, color = "#C2151B")+
        ggtitle("Body fat percentage and where you are",subtitle = "Reference: American Council on Exercise Standard for bodyfat percentages ") + theme_light()
    }else{
      NULL
    }
  })
  
  output$text_out = renderText({    
    if (bodyfat()>100 | bodyfat()<0){
      "Plot not valid"
    }else{ NULL
    }
  })
  
  
  
}




shinyApp(ui = ui, server = server)

