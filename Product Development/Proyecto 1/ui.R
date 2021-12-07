# Define UI
shinyUI(navbarPage("Product recommendation",
                   theme = shinytheme("cerulean"), #united
                   tabPanel("New product analysis",
                            sidebarPanel(
                              selectInput("targetMonth", "Studied month",
                                          selected = orderedDateFormat[5], multiple = FALSE,
                                          choices = orderedDateFormat),
                              fluidRow(column(5, actionButton("prevMonth", "Previous",
                                                              icon = icon("arrow-left"))),
                                       column(5, actionButton("nextMonth", "Next",
                                                              icon = icon("arrow-right")),
                                              offset=1)
                              ),
                              br(),
                              conditionalPanel(condition="input.mainPanelProdAnalysis == 'Monthly products analysis'",
                                               h4(textOutput("posFlanksDescriptionMonth"))),
                              conditionalPanel(condition="input.mainPanelProdAnalysis != 'Monthly products analysis'",
                                               uiOutput("productUI")),
                              width = 3
                            ),
                            
                          
                            mainPanel(
                              tabsetPanel(id = "mainPanelProdAnalysis",
                                          tabPanel("Monthly products analysis",
                                                   br(),
                                                   fluidRow(
                                                     column(5, radioButtons("yAxisMonthly", "Y-axis",
                                                                            choices = c("Count", "Fraction"),
                                                                            selected = "Fraction",
                                                                            inline = TRUE), offset = 5)
                                                   ),
                                                   plotlyOutput("productDistrPlotly")
                                          ),
                                          tabPanel("Combined new products analysis",
                                                   br(),
                                                   fluidRow(
                                                     column(4, checkboxInput("excludeSelfCombined", "Exclude analysed product",
                                                                             value = TRUE), offset = 1),
                                                     column(5, radioButtons("yAxisCombined", "Y-axis",
                                                                            choices = c("Count", "Fraction"),
                                                                            selected = "Fraction",
                                                                            inline = TRUE), offset = 1)
                                                   ),
                                                   plotlyOutput("productDistrCombinedPlotly")
                                          ),
                                          tabPanel("Continuous analysis",
                                                   br(),
                                                   fluidRow(
                                                     column(5, radioButtons("contVarY", "Y-axis",
                                                                            choices = contVars,
                                                                            inline = TRUE),
                                                            offset = 1),
                                                     column(4,
                                                            radioButtons("densityTypeCont", "Plot type",
                                                                         choices = c("Histogram", "Density"),
                                                                         inline = TRUE),
                                                            offset = 1
                                                     )), 
                                                   plotlyOutput("densityContPlotly"),
                                                   fluidRow(column(6,
                                                                   sliderInput("nbDensityBinsCont", "Number of distribution histogram bins",
                                                                               min = 5, max = 100, value = 50)
                                                                   , offset=3
                                                   ))
                                          ),
                                          tabPanel("Categorical analysis",
                                                   br(),
                                                   sidebarPanel(
                                                     selectInput("catVarSel", "Categorical variable selection",
                                                                 selected = catVars[1], multiple = FALSE,
                                                                 choices = catVars),
                                                     fluidRow(column(5, actionButton("prevCat", "Previous",
                                                                                     icon = icon("arrow-left"))),
                                                              column(5, actionButton("nextCat", "Next",
                                                                                     icon = icon("arrow-right")),
                                                                     offset=1)
                                                     ),
                                                     h5(textOutput("catVarDescription")),
                                                     br(),
                                                    
                                                     sliderInput("minCatCount", "Minimum group count",
                                                                 min = 1,
                                                                 max = 100,
                                                                 step = 1,
                                                                 value = 5),
                                                     h5(textOutput("catSubsetDescription"))
                                                   ),
                                                   mainPanel(
                                                     br(),
                                                     plotlyOutput("catPlotly"),
                                                     br(),
                                                     dataTableOutput('catTable')
                                                   )
                                          )
                              )
                            )
                   ),
                  
                   tabPanel("About",
                            h4(HTML(aboutString))
                   )
))
