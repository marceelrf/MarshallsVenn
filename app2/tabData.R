obj1 <- tabPanel("Data",
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
                 )

