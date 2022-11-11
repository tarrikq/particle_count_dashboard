#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#C:\Users\Tarri\Desktop\portfolio_projects\particle_count_dashboard\dashboard_app

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
allData <- read_csv(paste(getwd(), "/data/counts.csv", sep=""))
# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="Particle count data"),
    dashboardSidebar(
      fluidRow(
    # Sidebar with a slider input for number of bins 
        selectInput("filter",
            label="Select a filter to display",
            choices=list("Filter 9","Filter 10","Filter 11","Filter 12","Filter 13","Filter 14","Filter 15",
                         "Filter 16","Filter 17","Filter 19","Filter 21","Filter 22","Filter 23","Filter 24",
                         "Filter 25","Filter 26")))
    ),
    dashboardBody(
      fluidRow(
        column(width=12,
               box(width=6,
                 background = "black",
                 title = h3(textOutput("filter"), style = 'font-size:50px')

               ),
               box(
                 title = "Particle counts over run time",
                 status= "primary",
                 # Show a plot of the generated distribution
                 plotOutput("countsPlot")
               )





      )),
      fluidRow(
      column(width=12,
             box(
               title="Distribution of particle counts",
               status="primary",
               plotOutput("density")
             ),

             box(
               title="Turbidity over runtime",
               status="primary",
               plotOutput("turbidity")
             )
      )

      )
      )
    )
    


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$countsPlot <- renderPlot({
      data <- switch(input$filter, 
                     "Filter 9"=subset(allData, filter == 9),
                     "Filter 10"=subset(allData, filter == 10),
                     "Filter 11"=subset(allData, filter == 11),
                     "Filter 12"=subset(allData, filter == 12),
                     "Filter 13"=subset(allData, filter == 13),
                     "Filter 14"=subset(allData, filter == 14),
                     "Filter 15"=subset(allData, filter == 15),
                     "Filter 16"=subset(allData, filter == 16),
                     "Filter 17"=subset(allData, filter == 17),
                     "Filter 19"=subset(allData, filter == 19),
                     "Filter 21"=subset(allData, filter == 21),
                     "Filter 22"=subset(allData, filter == 22),
                     "Filter 23"=subset(allData, filter == 23),
                     "Filter 24"=subset(allData, filter == 24),
                     "Filter 25"=subset(allData, filter == 25),
                     "Filter 26"=subset(allData, filter == 26))

      
       ggplot(data) + 
        geom_point(aes(runDifference, Bin1, color = filterRunDate)) + 
         labs(x='Filter run time (hours)', y= '2-6 um particle counts (Counts/ml)', color = "Filter - Run start date") +
         theme(axis.text=element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=12))+
        ylim(0,300)

    })
    output$turbidity <- renderPlot({
      data <- switch(input$filter, 
                     "Filter 9"=subset(allData, filter == 9),
                     "Filter 10"=subset(allData, filter == 10),
                     "Filter 11"=subset(allData, filter == 11),
                     "Filter 12"=subset(allData, filter == 12),
                     "Filter 13"=subset(allData, filter == 13),
                     "Filter 14"=subset(allData, filter == 14),
                     "Filter 15"=subset(allData, filter == 15),
                     "Filter 16"=subset(allData, filter == 16),
                     "Filter 17"=subset(allData, filter == 17),
                     "Filter 19"=subset(allData, filter == 19),
                     "Filter 21"=subset(allData, filter == 21),
                     "Filter 22"=subset(allData, filter == 22),
                     "Filter 23"=subset(allData, filter == 23),
                     "Filter 24"=subset(allData, filter == 24),
                     "Filter 25"=subset(allData, filter == 25),
                     "Filter 26"=subset(allData, filter == 26))
      
      ggplot(data) + 
        geom_line(aes(runDifference, turbidity, color = filterRunDate)) + 
        labs(x="Filter run time (hours)",y="Turbidity",color = "Filter - Run start date")+
        theme(axis.text=element_text(size=12), axis.title=element_text(size=14), legend.text=element_text(size=12))+
        ylim(0.025, 0.1)
      
    })
    output$density <- renderPlot({
      data <- switch(input$filter, 
                     "Filter 9"=subset(allData, filter == 9),
                     "Filter 10"=subset(allData, filter == 10),
                     "Filter 11"=subset(allData, filter == 11),
                     "Filter 12"=subset(allData, filter == 12),
                     "Filter 13"=subset(allData, filter == 13),
                     "Filter 14"=subset(allData, filter == 14),
                     "Filter 15"=subset(allData, filter == 15),
                     "Filter 16"=subset(allData, filter == 16),
                     "Filter 17"=subset(allData, filter == 17),
                     "Filter 19"=subset(allData, filter == 19),
                     "Filter 21"=subset(allData, filter == 21),
                     "Filter 22"=subset(allData, filter == 22),
                     "Filter 23"=subset(allData, filter == 23),
                     "Filter 24"=subset(allData, filter == 24),
                     "Filter 25"=subset(allData, filter == 25),
                     "Filter 26"=subset(allData, filter == 26))
      
      ggplot(data, aes(x=Bin1)) + 
        geom_histogram(aes(y=..density..), colour="black", fill="white")+
        geom_density(alpha=.2, fill="blue") +
        labs(x="2-6 um particle counts",y="Density")+
        theme(axis.text=element_text(size=12), axis.title=element_text(size=14))+
        xlim(0,400)
      
    })
    
    output$filter <- renderText({
        data <- switch(input$filter, 
                       "Filter 9"=subset(allData, filter == 9),
                       "Filter 10"=subset(allData, filter == 10),
                       "Filter 11"=subset(allData, filter == 11),
                       "Filter 12"=subset(allData, filter == 12),
                       "Filter 13"=subset(allData, filter == 13),
                       "Filter 14"=subset(allData, filter == 14),
                       "Filter 15"=subset(allData, filter == 15),
                       "Filter 16"=subset(allData, filter == 16),
                       "Filter 17"=subset(allData, filter == 17),
                       "Filter 19"=subset(allData, filter == 19),
                       "Filter 21"=subset(allData, filter == 21),
                       "Filter 22"=subset(allData, filter == 22),
                       "Filter 23"=subset(allData, filter == 23),
                       "Filter 24"=subset(allData, filter == 24),
                       "Filter 25"=subset(allData, filter == 25),
                       "Filter 26"=subset(allData, filter == 26))
      paste("Filter", data$filter[1], sep = " ")
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)

#rsconnect::deployApp("C:/Users/Tarri/Desktop/portfolio_projects/particle_count_dashboard/dashboard_app")
