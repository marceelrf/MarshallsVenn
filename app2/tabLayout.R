obj2 <- tabPanel(
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
