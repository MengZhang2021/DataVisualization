install.packages("shiny")
install.packages("readr")
install.packages("tidyverse")
library(readr)
library(shiny)
library(RColorBrewer)
library(ggplot2)
require("readr")
require("shiny")
library(tidyverse)
gr <- read_csv("https://raw.githubusercontent.com/MengZhang2021/DataVisualization/master/Growthrate.csv")
ui <- fluidPage(
  titlePanel(title = h4("Business dynamics among California, Texas and the U.S. 2010-2018", align="center")),
  sidebarPanel(
    textInput(inputId = "caption",
              label = "Caption:",
              value = "Sectors"),
    radioButtons("Sector", "Select the Sector",
                 choices = c("Average Growth Rate",
                             "Mining,quarring and oil and gas extraction",
                             "Utilities",
                             "Construction",
                             "Manufacturing",
                             "Wholesale trade",
                             "Retail trade",
                             "Transportation and warehousing",
                             "Information",
                             "Finance and insurance",
                             "Real estate and rental and leasing",
                             "Professional, scientific, and technical services",
                             "Management of companies and enterprises",
                             "Administrative and support and waste management and remediation services",
                             "Educational services",
                             "Health care and social assistance",
                             "Arts, entertainment, and recreation",
                             "Accommodation and food services",
                             "Other services (except public administration)"),
                 selected = "Average Growth Rate")),
  mainPanel(
    plotOutput("bar",height=500))
)

server <- function(input,output){
  reactive_data = reactive({
    selected_Sector = input$Sector
    return(gr[gr$Sectors==selected_Sector,])
    
  })
  
  output$caption <- renderText({
    input$caption
  })
  output$bar <- renderPlot({
    color <- c(rgb(0.3,0.1,0.4,0.6) , rgb(0.2,0.4,0.6,0.6) , rgb(0.8,0.1,0.1,0.6))
    our_data <- reactive_data()
    bp <- barplot(colSums(our_data[,c("US","California","Texas")]),
                  ylab="Growth Rate",
                  ylim =c(-20,50),
                  xlab="Region",
                  names.arg = c("US","California","Texas"),
                  col = color)
    text(bp,5,labels=our_data[c("US","California","Texas")])
    text(3,100,labels = "Unit=Percent")
    legend("topright", legend = c("US","California","Texas"),
           col = c(rgb(0.3,0.1,0.4,0.6) , rgb(0.2,0.4,0.6,0.6) , rgb(0.8,0.1,0.1,0.6)) , 
           bty = "n", pch=20 , pt.cex = 2, cex = 1.5, horiz = FALSE, inset = c(0.05, 0.05))
  })
}
shinyApp(ui=ui, server=server)
