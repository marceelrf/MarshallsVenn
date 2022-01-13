

```r
library(shiny)
library(tidyverse)
library(ggvenn)
library(viridis)

# Define UI for application
ui <- fluidPage(
    id= "test",
    tags$style('#test{
    background-color: #540007;
               font-family: "Rockwell";}'
               ),
    tags$head(tags$style(HTML("a {color: #917c00}"))),#Colors of links
    # Application title
    titlePanel(windowTitle = "Marshall's Venn",
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
                vecCols <- viridis(n = length(data()),option = "D",direction = 1)
            } else {
                if (input$colors == "plasma") {
                    vecCols <- viridis(n = length(data()),option = "C",direction = 1)
                } else {
                    if (input$colors == "inferno") {
                        vecCols <- viridis(n = length(data()),option = "B",direction = 1)
                    } else {
                        vecCols <- viridis(n = length(data()),option = "A",direction = 1)
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
                text = element_text(family = "serif"),
                plot.title = element_text(hjust = 0.5,
                                          family = "serif",
                                          face = "bold",
                                          size = 20),
                plot.subtitle = element_text(hjust = 0.5,
                                             family = "serif",
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
# shinyApp(ui = ui, server = server)

# SessionInfo
sessionInfo()
```

```
## R version 4.0.3 (2020-10-10)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 10 x64 (build 19043)
## 
## Matrix products: default
## 
## locale:
## [1] LC_COLLATE=Portuguese_Brazil.1252  LC_CTYPE=Portuguese_Brazil.1252   
## [3] LC_MONETARY=Portuguese_Brazil.1252 LC_NUMERIC=C                      
## [5] LC_TIME=Portuguese_Brazil.1252    
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] viridis_0.5.1     viridisLite_0.3.0 ggvenn_0.1.9      forcats_0.5.1    
##  [5] stringr_1.4.0     dplyr_1.0.7       purrr_0.3.4       readr_1.4.0      
##  [9] tidyr_1.1.3       tibble_3.1.2      ggplot2_3.3.5     tidyverse_1.3.1  
## [13] shiny_1.6.0      
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.7        lubridate_1.7.10  ps_1.6.0          assertthat_0.2.1 
##  [5] digest_0.6.27     utf8_1.1.4        mime_0.10         R6_2.5.0         
##  [9] cellranger_1.1.0  backports_1.2.1   reprex_2.0.0      evaluate_0.14    
## [13] httr_1.4.2        highr_0.8         pillar_1.6.1      rlang_0.4.10     
## [17] readxl_1.3.1      rstudioapi_0.13   callr_3.6.0       jquerylib_0.1.3  
## [21] rmarkdown_2.11    webshot_0.5.2     munsell_0.5.0     broom_0.7.6      
## [25] compiler_4.0.3    httpuv_1.5.5      modelr_0.1.8      xfun_0.22        
## [29] pkgconfig_2.0.3   htmltools_0.5.1.1 tidyselect_1.1.0  gridExtra_2.3    
## [33] fansi_0.4.2       withr_2.4.1       crayon_1.4.1      dbplyr_2.1.1     
## [37] later_1.3.0       jsonlite_1.7.2    xtable_1.8-4      gtable_0.3.0     
## [41] lifecycle_1.0.0   DBI_1.1.1         magrittr_2.0.1    scales_1.1.1     
## [45] cachem_1.0.4      cli_2.4.0         stringi_1.5.3     fs_1.5.0         
## [49] promises_1.2.0.1  xml2_1.3.2        bslib_0.2.4       ellipsis_0.3.2   
## [53] generics_0.1.0    vctrs_0.3.8       Cairo_1.5-12.2    tools_4.0.3      
## [57] glue_1.4.2        markdown_1.1      hms_1.0.0         processx_3.5.1   
## [61] fastmap_1.1.0     yaml_2.2.1        colorspace_2.0-0  rvest_1.0.0      
## [65] knitr_1.31        haven_2.3.1       sass_0.3.1
```

