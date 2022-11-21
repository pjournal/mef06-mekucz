library(shiny)
library(dplyr)
library(readxl)
library(tidyverse)

foreign_students <- read_excel("www/foreign_students.xlsx")
foreign_students <-foreign_students%>%filter(foreign_students$University=="BOĞAZİÇİ ÜNİVERSİTESİ")
nationalities<-unlist(foreign_students$Nationality)
maxstudent<-max(as.numeric(foreign_students$T))
minstudent<-min(as.numeric(foreign_students$T))
foreign_students$T<-as.integer(as.numeric(foreign_students$T))




# Define UI ----
ui <- fluidPage(
  titlePanel(strong("Foreigners registered in Boğaziçi Üniversitesi")),
  titlePanel("Analysis for 2021-2022 Season"),
  sidebarLayout(
    sidebarPanel(
  
           checkboxGroupInput("gender", "Please select gender:", choices = list("Man","Woman"),selected = "Man"),
           selectInput("nation", "Please select Nationality:",  choices = nationalities),
           sliderInput("slider", "Number of students:",min = minstudent, max = maxstudent, value = c(1,38))
    ),
    
   
    
    
  mainPanel(
    HTML('<center>  . </center>'),
    HTML('<center><img src="students.jpg" width="150"></center>'),
    HTML('<center>  . </center>'),
    HTML('<center>~~~~~~~~~~~~||||||||||||||~~~~~~~~~~~~</center>'),
    HTML('<center>  . </center>'),
    h4("Number of Students according to selected nationality and gender:"),
    HTML('<center>.</center>'),
    tableOutput("result1"),
    HTML('<center>  . </center>'),
    HTML('<center>~~~~~~~~~~~~||||||||||||||~~~~~~~~~~~~</center>'),
    HTML('<center>  . </center>'),
    h4("Top 4 nationalities according to selected nationality:"),
    HTML('<center>.</center>'),
    plotOutput("plot"),
    tableOutput("result2"),
    
    
  )
  
  )
)

# Define server logic ----
server <- function(input, output) {
  
  output$result1 <- renderTable({
      if (is.null(input$gender) ){return(print("Please select gender!"))}
      if (length(input$gender) ==1){
      if (input$gender == "Man"){
        foreign_students%>%filter(foreign_students$Nationality==input$nation)%>%select(University,Nationality,M)
        }
      else if (input$gender == "Woman"){
        foreign_students%>%filter(foreign_students$Nationality==input$nation)%>%select(University,Nationality,W)
      }
    } else{
      foreign_students%>%filter(foreign_students$Nationality==input$nation)%>%select(University,Nationality,T)
    } 
    
    })
  
  sliderValues <- reactive({
    a<-as.integer(input$slider[1])
    b<-as.integer(input$slider[2])
    
    foreign_students%>%filter(foreign_students$T>=a & foreign_students$T<=b )%>%select(University,Nationality,T)%>%arrange(desc(T))
    
  })
  
  output$result2 <- renderTable({
    sliderValues()
  })
  
  output$plot<-renderPlot({
    a<-as.integer(input$slider[1])
    b<-as.integer(input$slider[2])
    plottable<-foreign_students%>%filter(foreign_students$T>=a & foreign_students$T<=b )%>%select(Nationality,T)%>%arrange(desc(T))
    finaltable<-head(plottable,4)
    ggplot(finaltable,aes(x=Nationality,y=T,color=Nationality))+geom_col()
    
  })
  
  
  
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

