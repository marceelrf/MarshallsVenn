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
library(viridis)

# Define UI for application that draws a histogram
ui <- fluidPage(
    id= "test",
    tags$style('#test{
                             background-color: #540007;
                             font-family: "Rockwell";}'
               ),
    tags$head(tags$style(HTML("a {color: #917c00}"))),#Colors of links
    # Application title
    titlePanel(
        div(
            id="panelTitle",
            tags$style('#panelTitle{
            color: #faf9de;
            font-family: "Book Antiqua";}'),
            HTML("Venn diagram - <em><b>Marshall's tools</b></em>")
            )
    ),

    # Fluid row
    fluidRow(
        #Sidebar
        column(width = 4,
               sidebarPanel(width = 12,
                            id = "sidePanel",
                            tags$style('#sidePanel{
                                              color: #000000;}'),
                            tabsetPanel(type = "pills",
                                        #Tab - dados
                                        tabPanel("Data",
                                                 div(
                                                     h1(strong("Let's start!"),align = "center"),
                                                     hr(),
                                                     fluidRow(
                                                         h4(strong("Choose CSV File"),align = "center")
                                                     ),
                                                     fileInput("file1",label = "",
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
                                                                              value = "",
                                                                              placeholder = "Enter the set 1 elements")),
                                                         column(width = 6,
                                                                textAreaInput(inputId = "set2",
                                                                              label = "set 2",
                                                                              value = "",
                                                                              placeholder = "Enter the set 2 elements")
                                                         )
                                                     ),
                                                     fluidRow(
                                                         column(width = 6,
                                                                textAreaInput(inputId = "set3",
                                                                              label = div(HTML("set 3 <em>(optional)</em>")),
                                                                              value = NULL,
                                                                              placeholder = "Enter the set 3 elements")),
                                                         column(width = 6,
                                                                textAreaInput(inputId = "set4",
                                                                              label = div(HTML("set 4 <em>(optional)</em>")),
                                                                              value = NULL,
                                                                              placeholder = "Enter the set 4 elements")
                                                         )
                                                     ),
                                                     hr(),
                                                     fluidRow(
                                                         div(downloadButton('download',"Download your Venn diagram!"),
                                                             align = "center")
                                                     ),
                                                 )
                                        ),
                                        #Tab - layout
                                        tabPanel(
                                            "Layout",
                                            #Colors
                                            fluidRow(
                                                #Color pallete
                                                column(
                                                    width = 6,
                                                    selectInput(inputId = "colors",
                                                                label = "Fill colors",
                                                                choices = c("Default",
                                                                            "viridis",
                                                                            "plasma",
                                                                            "inferno",
                                                                            "cividis"),
                                                                selected = "Default")
                                                ),
                                                #Color alpha
                                                column(
                                                    width = 6,
                                                    sliderInput(inputId = "colorAlpha",
                                                                label = "Colors opacity",
                                                                value = .5,
                                                                min = .1,
                                                                max = 1,
                                                                step = .1)
                                                )
                                            ),
                                            #Lines
                                            fluidRow(
                                                #Line type
                                                column(width = 6,
                                                       selectInput(inputId = "ltype",
                                                                   label = "Line type",
                                                                   choices = c("solid",
                                                                               "dashed",
                                                                               "dotted",
                                                                               "longdash",
                                                                               "blank"),
                                                                   selected = "solid")
                                                ),
                                                #Line size
                                                column(width = 6,
                                                       numericInput(inputId = "lsize",
                                                                    label = "Line Size",
                                                                    value = 1,
                                                                    min = .25,
                                                                    max = 2,
                                                                    step = .25)
                                                )
                                            ),
                                            hr(),
                                            fluidRow(
                                                column(width = 6,
                                                       textInput(inputId = "setName1",
                                                                 label = "Set 1 name",
                                                                 placeholder = "Enter the name of set 1"),
                                                       textInput(inputId = "setName3",
                                                                 label = "Set 3 name",
                                                                 placeholder = "Enter the name of set 3")
                                                ),
                                                column(width = 6,
                                                       textInput(inputId = "setName2",
                                                                 label = "Set 2 name",
                                                                 placeholder = "Enter the name of set 2"),
                                                       textInput(inputId = "setName4",
                                                                 label = "Set 4 name",
                                                                 placeholder = "Enter the name of set 4")
                                                )
                                            ),
                                            #Set elements
                                            # textAreaInput(inputId = "setNames",
                                            #               label = "Set Names",
                                            #               placeholder = "Enter the names of the sets separated by space/enter"),
                                            fluidRow(
                                                #Names size
                                                column(
                                                    width = 6,
                                                    numericInput(inputId = "nsize",
                                                                 label = "Set names size",
                                                                 value = 5,
                                                                 min = 1,
                                                                 max = 10)
                                                ),
                                                #Names colors
                                                column(
                                                    width = 6,
                                                    selectInput(
                                                        inputId = "ncolors",
                                                        label = "Names colors",
                                                        choices = c("Black",
                                                                    "Same as fill"),
                                                        selected = "black"
                                                    )
                                                )
                                            ),
                                            #element size
                                            fluidRow(
                                                column(
                                                    width = 6,
                                                    numericInput(inputId = "esize",
                                                                 label = "Element size",
                                                                 value = 5,min = 1,max = 10)
                                                ),
                                                column(
                                                    width = 6,
                                                    checkboxInput(inputId = "Percent",
                                                                  label = "Show percentage?",
                                                                  value = T)
                                                )
                                            ),
                                            hr(),
                                            #Title and subtible
                                            textInput(inputId = "title",
                                                      label = "Title",
                                                      value = "",width = "100%"),
                                            textInput(inputId = "subtitle",
                                                      label = "Subtitle",
                                                      value = "",width = "100%")
                                        )

                                        )
                            )
               ),
        #MainPanel
        column(width = 8,
               id="tabPanel",
               tags$style(
               '#tabPanel{
               color: #faf9de;}'
               ),
               tabsetPanel(
                   tabPanel("Venn",plotOutput("Venn")),
                   tabPanel("About",includeHTML(path = "about.html"))
                   )
               ),
        )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    #Reactive file
    inFile <- reactive({
        input$file1
    })

    #Set names
    sn <- reactive({
        str_split(string = input$setNames,
                  pattern = "[:space:]")[[1]]
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
                if (str_detect(input$setName1,pattern = "^$",negate = T) & str_detect(input$setName2,pattern = "^$",negate = T)) {
                    names(ls)[1:2] <- c(input$setName1,input$setName2)
                }
                if (!str_detect(input$set3,pattern = "^$")) {
                    ls[["Set 3"]] <- str_split(string = input$set3,
                                               pattern = "[:space:]")[[1]]
                    if (str_detect(input$setName3,pattern = "^$",negate = T)) {
                        names(ls)[3] <- input$setName3
                    }
                }

                if (!str_detect(input$set4,pattern = "^$")) {
                    ls[["Set 4"]] <- str_split(string = input$set4,
                                               pattern = "[:space:]")[[1]]
                    if (str_detect(input$setName4,pattern = "^$",negate = T)) {
                        names(ls)[4] <- input$setName4
                    }
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

    #Colors
    colors <- reactive({
        vecCols <- c()
        if (input$colors == "Default") {
            vecCols<- c("#f8766d","#7cae00","#00bfc4","#c77cff")
        } else {
            if (input$colors == "viridis") {
                vecCols <- viridis(n = 4,option = "D")
            } else {
                if (input$colors == "plasma") {
                    vecCols <- viridis(n = 4,option = "C")
                } else {
                    if (input$colors == "inferno") {
                        vecCols <- viridis(n = 4,option = "B")
                    } else {
                        vecCols <- viridis(n = 4,option = "A")
                    }
                }

            }
        }
        return(vecCols)
    })
    #Colors names
    colors_names <- reactive({
        obj_cnames <- c()
        if (input$ncolors == "Black") {
            obj_cnames <- "black"
        } else {
            obj_cnames <- colors()[1:length(data())]
        }
    })


    plot1 <- reactive({
        ggvenn(data(),
               show_percentage = input$Percent,
               fill_alpha = input$colorAlpha,
               stroke_size = input$lsize,
               stroke_linetype = input$ltype,
               set_name_size = input$nsize,
               text_size = input$esize,
               fill_color = colors(),
               set_name_color = colors_names()
               ) +
            ggtitle(label = input$title,
                    subtitle = input$subtitle) +
            theme(
                text = element_text(family = "Book Antiqua"),
                plot.title = element_text(hjust = 0.5,
                                          family = "Times New Roman",
                                          face = "bold",
                                          size = 20),
                plot.subtitle = element_text(hjust = 0.5,
                                             family = "Times New Roman",
                                             face = "italic",
                                             size = 14))
    })



    output$Venn <- renderPlot({

        if(is.null(data())){
            return(NULL)
        } else {
            plot1()
        }
    })

    # Download area -----------------------------------------------------------

    output$download <- downloadHandler(
        filename = paste('VennDiagram', Sys.Date(), '.png', sep=''),
        content = function(file) {
            ggsave(filename = file,
                   plot = plot1(),
                   device = "png",
                   scale = 2)
        }

    )

}

# Run the application
shinyApp(ui = ui, server = server)
