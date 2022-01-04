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
   mainPanel()
)
)

# Define server logic required to draw a Venn Diagram
server <- function(input, output) {

}

# Run the application
shinyApp(ui = ui, server = server)
