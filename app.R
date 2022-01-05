#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggvenn)

# Define UI for application that draws a Venn Diagram ---------------------
ui <- fluidPage(id= "test",
                tags$style('#test{
                             background-color: #540007;
                             font-family: "Rockwell";
              }'),

# Title panel -------------------------------------------------------------
  titlePanel(div(id="panelTitle",
                 tags$style('#panelTitle{
                            color: #faf9de;
                            font-family: "Book Antiqua";
                 }'),
                 HTML("Venn diagram - <em><b>Marshall's tools</b></em>"))),
  sidebarLayout(
# Sidebar panel -----------------------------------------------------------
   sidebarPanel(
     fileInput("file1", "Choose CSV File",
               accept = c(
                 "text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
     ),
     hr(),
     fluidRow(
       h5("Manual enter",align = "center")
     ),
     fluidRow(
       column(width = 6,
              textAreaInput(inputId = "set1",
                            label = "set 1",
                            value = NULL)),
       column(width = 6,
              textAreaInput(inputId = "set2",
                            label = "set 2",
                            value = NULL)
              )
       ),
     fluidRow(
       column(width = 6,
              textAreaInput(inputId = "set3",
                            label = div(HTML("set 3 <em>(optional)</em>")),
                            value = NULL)),
       column(width = 6,
              textAreaInput(inputId = "set4",
                            label = div(HTML("set 4 <em>(optional)</em>")),
                            value = NULL)
       )
     )
   ),

# Main Panel --------------------------------------------------------------
   mainPanel(
      tabsetPanel(
         tabPanel("Venn",plotOutput("Venn")),#Plot Venn
         tabPanel("Table",tableOutput("Tbl"))
      )


   )
)
)

# Define server logic required to draw a Venn Diagram
server <- function(input, output) {



   #Reactive file
   inFile <- reactive({
      input$file1
   })

   #Reactive data
   data <- reactive({
      if (is.null(inFile()))
         return(NULL)

      read_csv2(file = inFile()$datapath)
   })





   output$Venn <- renderPlot({

      if(is.null(data())){
         return(NULL)
      } else {
         #
         #List for organize data
         ls <- list()
         for(i in 1:ncol(data())){
            vec <- data()[,i]
            ls[[i]] <- vec[!is.na(vec)]
         }
         names(ls) <- colnames(data())
         ggvenn(ls)
      }
   })

   output$Tbl <- renderText({
      if(is.null(data())){
         return(NULL)
      } else {
         paste(str_detect(string = data(),pattern = "NA") %>% sum())


      }
   })

}

# Run the application
shinyApp(ui = ui, server = server)
