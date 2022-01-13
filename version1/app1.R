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
      markdown(mds = "# Let's start!"),
      hr(),
     fileInput("file1",
               h4(strong("Choose CSV File"),align = "center"),
               accept = c(
                 "text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
     ),
     hr(),
     fluidRow(
       h4(strong("Or enter manually"),align = "center")
     ),
     fluidRow(
       column(width = 6,
              textAreaInput(inputId = "set1",
                            label = "set 1",
                            value = "")),
       column(width = 6,
              textAreaInput(inputId = "set2",
                            label = "set 2",
                            value = "")
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
     ),
     hr(),
     fluidRow(
        div(downloadButton('download',"Download your Venn diagram!"),
            align = "center")
     ),
   ),

# Main Panel --------------------------------------------------------------
   mainPanel(id="tabPanel",
             tags$style('#tabPanel{
                            color: #faf9de;
                 }'),
      tabsetPanel(
         tabPanel("Venn",plotOutput("Venn")),#Plot Venn
         #tabPanel("Table",tableOutput("Tbl")),#Table test
         tabPanel("About",div(h1(em("Marshall's tools"),
                                 align = "center"),
                              p("Hello, my name is Marcel Ferreira (",
                                em("Marshall"),").","Welcome to the Venn diagram building tool!",
                                align = "justify"),
                              p("The purpose of Marshall tools is to facilitate the construction of high-level graphics for users not familiar with programming languages.",
                                "All tools are and will ALWAYS be free.
                                The applications are currently in the testing phase, if you find any problems during use, please contact us via",
                                em("marcel.ferreira@unesp.br"),".",
                                align = "justify"),
                              #hr(),
                              h3("How to use",
                                 align = "center"),
                              p("You can choose to upload a file with the CSV extension or manually place your datasets in the boxes.",
                                "CSV files must contain up to 4 columns (one for each set), the first line will be used as set titles.",
                                "While it is possible to create Venn diagrams for numerous data sets,
                                diagrams above 4 sets lose visual efficiency. If you want to work with a larger number of sets,
                                we recommend exploring other graphics options.",
                                strong("(Coming soon on",em("Marshall Tools"),")."),
                                align = "justify"),
                              h3("Acknowledgment",
                                 align = "center"),
                              p("I am grateful to the São Paulo State Research Support Foundation",
                                a("(FAPESP)",href="https://fapesp.br/"),
                                ", for its financial assistance.",
                                strong("Process number 2018/05731-7"),".",
                                "This app would not be possible without",
                                a(em("tidyverse"),href = "https://www.tidyverse.org/"),
                                ",",a(em("shiny"),href = "https://shiny.rstudio.com/"),
                                " and",a(em(" ggvenn"),href ="https://github.com/yanlinlin82/ggvenn"),
                                " packages."),
                              h4("Where find me?",
                                 align = "center"),
                              p("I am currently a PhD student at the Bioassays and Dynamics Laboratory at the
                                 Biosciences Institute of Botucatu under the supervision of dr. ", a("Willian Zambuzzi",
                                                                                                     href = "https://scholar.google.com.br/citations?user=bMhT1QsAAAAJ&hl=pt-BR"),
                                ".","I'm on", a("Twitter",href = "https://twitter.com/marceelrf")," and ",a("Instagram", href = "https://www.instagram.com/marceelrf/")," on personal @marceelrf accounts.",
                                "Please check out my ",a("Github",href="https://github.com/marceelrf"),"."),
                              h6("See you!",
                                 align="center")
                              )
                  )
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
   ls <- list()
   data <- reactive({

      if (is.null(inFile())){
         if (str_detect(input$set1,pattern = "^$") | str_detect(input$set2,pattern = "^$")) {
            return(NULL)
         } else {
            ls[["Set 1"]] <- str_split(string = input$set1,
                                       pattern = "[:space:]")[[1]]
            ls[["Set 2"]] <- str_split(string = input$set2,
                                       pattern = "[:space:]")[[1]]
            if (!str_detect(input$set3,pattern = "^$")) {
               ls[["Set 3"]] <- str_split(string = input$set3,
                                          pattern = "[:space:]")[[1]]
            }

            if (!str_detect(input$set4,pattern = "^$")) {
               ls[["Set 4"]] <- str_split(string = input$set4,
                                          pattern = "[:space:]")[[1]]
            }


            return(ls)
         }
      } else {

         df <- read_csv2(file = inFile()$datapath)

         for(i in 1:ncol(df)){
            vec <- df[,i]
            ls[[i]] <- vec[!is.na(vec)]
         }
         names(ls) <- colnames(df)
         return(ls)
      }


   })





   output$Venn <- renderPlot({

      if(is.null(data())){
         return(NULL)
      } else {
         # #
         # #List for organize data
         # ls <- list()
         # for(i in 1:ncol(data())){
         #    vec <- data()[,i]
         #    ls[[i]] <- vec[!is.na(vec)]
         # }
         # names(ls) <- colnames(data())
         p1 <- ggvenn(data())
         p1
      }
   })

# Download area -----------------------------------------------------------

   output$download <- downloadHandler(
      filename = paste('VennDiagram', Sys.Date(), '.png', sep=''),
      content = function(file) {

         device <- function(..., width, height) {
            grDevices::png(..., width = width, height = height,
                           res = 300, units = "in")
         }
         ggsave(filename = file,
                plot = ggvenn(data()),
                device = device)
         }

   )

}

# Run the application
shinyApp(ui = ui, server = server)
