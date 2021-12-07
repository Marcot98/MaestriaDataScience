shinyServer(function(input, output, session) {
  # Logic to subset the positive flanks data based on the month selection
  positiveFlankDataMonth <- reactive({
    out <- data
    
    # Subset on the analysed month
    out <- data[monthFormat == input$targetMonth, ]
    
    out
  })
  
  # Logic to subset the positive flanks data based on the month and product
  # selection
  positiveFlankData <- reactive({
    out <- data
    
    # Subset on the analysed month and product
    out <- data[monthFormat == input$targetMonth &
                  newProdSimple == input$targetProduct, ]
    
    out
  })
  
  # Returns records of the selected month for users that contain both the
  # selected product but also other products
  otherPositiveFlankData <- reactive({
    positiveFlankData <- positiveFlankData()
    positiveFlankDataUsers <- unique(positiveFlankData$ncodpers)
    out <- data
    
    # Subset on the analysed month, product and users
    excludeSelfCombined <- input$excludeSelfCombined
    out <- data[monthFormat == input$targetMonth &
                  (!excludeSelfCombined |
                     newProdSimple != input$targetProduct) &
                  ncodpers %in% positiveFlankDataUsers, ]
    out
  })
  
  # Calculate the number of analysed positive flanks for the selected month
  nbPosFlanksMonthGeneral <- reactive({
    nrow(positiveFlankDataMonth())
  })
  
  # Calculate the number of positive flanks for the analysed month
  nbPosFlanksMonth <- reactive({
    nrow(positiveFlankData())
  })
  
  # Calculate the number of analysed positive flanks
  nbPosFlanks <- reactive({
    nrow(positiveFlankData())
  })
  
  # Calculate the number of joined positive flanks
  nbJoinedPosFlanks <- reactive({
    sum(otherPositiveFlankData()$newProdSimple != input$targetProduct)
  })
  
  # Descriptive text of the number of positive flanks for the analysed month
  output$posFlanksDescriptionMonth <- renderText({
    nbPosFlanks <- nbPosFlanksMonthGeneral()
    paste0(nbPosFlanks, " month positive flank",
           ifelse(nbPosFlanks==1, "", "s"))
  })
  
  # Descriptive text of the number of positive flanks
  output$posFlanksDescription <- renderText({
    nbPosFlanks <- nbPosFlanks()
    paste0(nbPosFlanks, " product month positive flank",
           ifelse(nbPosFlanks==1, "", "s"))
  })
  
  # Descriptive text of the number of joined positive flanks
  output$joinedPosFlanksDescription <- renderText({
    nbJoinedPosFlanks <- nbJoinedPosFlanks()
    paste0(nbJoinedPosFlanks, " joined product month positive flank",
           ifelse(nbJoinedPosFlanks==1, "", "s"))
  })
  
  # Previous target month logic
  observeEvent(input$prevMonth,{
    currentId <- which(input$targetMonth == orderedDateFormat)
    newId <- max(c(1, currentId - 1))
    
    # Update target variable
    updateSelectInput(session, "targetMonth",
                      selected = orderedDateFormat[newId])
  })
  
  # Next target variable logic
  observeEvent(input$nextMonth,{
    currentId <- which(input$targetMonth == orderedDateFormat)
    newId <- min(c(length(orderedDateFormat), currentId + 1))
    
    # Update target variable
    updateSelectInput(session, "targetMonth",
                      selected = orderedDateFormat[newId])
  })
  
  # Previous target product logic
  observeEvent(input$prevProduct,{
    currentId <- which(input$targetProduct == productsSimple)
    newId <- max(c(1, currentId - 1))
    
    # Update target product
    updateSelectInput(session, "targetProduct",
                      selected = productsSimple[newId])
  })
  
  # Next target product logic
  observeEvent(input$nextProduct,{
    currentId <- which(input$targetProduct == productsSimple)
    newId <- min(c(length(productsSimple), currentId + 1))
    
    # Update target product
    updateSelectInput(session, "targetProduct",
                      selected = productsSimple[newId])
  })
  
  # Render the product UI
  output$productUI <- renderUI({
    targetProduct <- ifelse(is.null(input$targetProduct),
                            productsSimple[3],
                            input$targetProduct)
    out <- list(
      selectInput("targetProduct", "Studied product",
                  selected = targetProduct, multiple = FALSE,
                  choices = productsSimple),
      fluidRow(column(5, actionButton("prevProduct", "Previous",
                                      icon = icon("arrow-left"))),
               column(5, actionButton("nextProduct", "Next",
                                      icon = icon("arrow-right")),
                      offset=1)
      ),
      h4(textOutput("posFlanksDescription")),
      conditionalPanel(condition="input.mainPanelProdAnalysis == 'Combined new products analysis'",
                       h4(textOutput("joinedPosFlanksDescription"))),
      br()
    )
    out
  })
  
  # Generate the product barplot for the monthly positive flank data
  output$productDistrPlotly <- renderPlotly({
    positiveFlankData <- positiveFlankDataMonth()
    nbPosFlanks <- nbPosFlanksMonthGeneral()
    positiveFlankCount <- positiveFlankData[, .N, .(newProdSimple)]
    
    # Add products with a zero count
    zeroProds <- productsSimple[!productsSimple %in%
                                  positiveFlankCount[, newProdSimple]]
    if(length(zeroProds)>0){
      positiveFlankCount <- rbind(positiveFlankCount,
                                  data.table(newProdSimple = zeroProds, N = 0))
    }
    names(positiveFlankCount)[2] <- "Count"
    positiveFlankCount$Fraction <- positiveFlankCount$Count/nbPosFlanks
    
    # Generate the ggplot based on the plot type selection
    p <- ggplot(positiveFlankCount, aes_string(x="newProdSimple",
                                               y=input$yAxisMonthly,
                                               fill="newProdSimple")) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE)
    
    # Limit y to [0, 1/3] when considering fractions
    if(input$yAxisMonthly == "Fraction"){
      p <- p + ylim(c(0, 1/3))
    }
    
    # Convert to plotly format
    ggplotly(p)
  })
  
  # Generate the other product barplot for the positive flank data
  output$productDistrCombinedPlotly <- renderPlotly({
    positiveFlankData <- otherPositiveFlankData()
    nbPosFlanks <- nbPosFlanks()
    positiveFlankCount <- positiveFlankData[, .N, .(newProdSimple)]
    
    # Add products with a zero count
    zeroProds <- productsSimple[!productsSimple %in%
                                  positiveFlankCount[, newProdSimple]]
    if(length(zeroProds)>0){
      positiveFlankCount <- rbind(positiveFlankCount,
                                  data.table(newProdSimple = zeroProds, N = 0))
    }
    names(positiveFlankCount)[2] <- "Count"
    positiveFlankCount$Fraction <- positiveFlankCount$Count/nbPosFlanks
    
    # Generate the ggplot based on the plot type selection
    p <- ggplot(positiveFlankCount, aes_string(x="newProdSimple",
                                               y=input$yAxisCombined,
                                               fill="newProdSimple")) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE)
    
    # Limit y to [0, 1] when considering fractions
    if(input$yAxisMonthly == "Fraction"){
      p <- p + ylim(c(0, 1))
    }
    
    # Convert to plotly format
    ggplotly(p)
  })
  

  output$densityContPlotly <- renderPlotly({
    plotData <- positiveFlankData()
    
 
    if(input$densityTypeCont == "Histogram"){
      p <- ggplot(plotData, aes_string(input$contVarY)) +
        geom_histogram(bins = input$nbDensityBinsCont)
    } else{
      p <- ggplot(plotData, aes_string(input$contVarY)) +
        geom_density()
    }
    

    contId <- which(contVars == input$contVarY)
    p <- p +
      xlim(contVarRanges[contId, ])
    
 
    ggplotly(p)
  })
  

  observeEvent(input$prevCat,{
    currentId <- which(input$catVarSel == catVars)
    newId <- max(c(1, currentId - 1))
    

    updateSelectInput(session, "catVarSel",
                      selected = catVars[newId])
  })
  

  observeEvent(input$nextCat,{
    currentId <- which(input$catVarSel == catVars)
    newId <- min(c(length(catVars), currentId + 1))
    
  
    updateSelectInput(session, "catVarSel",
                      selected = catVars[newId])
  })
  

  output$catVarDescription <- renderText({ 
    nbLevels <- length(unique(positiveFlankData()[[input$catVarSel]]))
    paste0("Categorical variable with ", nbLevels, " level",
           ifelse(nbLevels==1, "", "s"), " for the analysed data")
  })
  

  catPlotData <- reactive({
    out <- positiveFlankData()
    
  
    freqs <- table(out[[input$catVarSel]])
    out <- out[out[[input$catVarSel]] %in%
                 names(freqs)[freqs >= input$minCatCount]]
    
    out
  })
  

  catPlotDataCount <- reactive({
    catData <- catPlotData()[, c(input$catVarSel), with=FALSE]
    names(catData)[1] <- "y"
    catData[y=="", y:= "Unknown"]
    out <- catData[, .N, y]
    out <- out[order(-N), ]
    out[, Fraction := round(100*N/nrow(catData), 2)]
    names(out)[1] <- input$catVarSel
    names(out)[-1] <- c("Count", "Fraction (%)")
    out
  })
  

  output$catSubsetDescription <- renderText({ 
    nbLevels <- length(unique(positiveFlankData()[[input$catVarSel]]))
    nbConsideredLevels <- length(unique(catPlotData()[[input$catVarSel]]))
    paste0(nbConsideredLevels, " categorical variable level",
           ifelse(nbConsideredLevels==1, "", "s"), " out of ",
           nbLevels, " contain", ifelse(nbConsideredLevels==1, "s", ""),
           " the minimum group count")
  })
  
  
  output$catPlotly <- renderPlotly({
    plotData <- catPlotDataCount()
    
    p <- ggplot(plotData, aes_string(x=input$catVarSel,
                                     y="Count",
                                     fill=input$catVarSel)) +
      geom_bar(stat="identity") +
      theme(axis.text.x=element_text(angle = -90, hjust = 0, vjust = 0.5)) +
      guides(fill=FALSE)
    
   
    ggplotly(p)
  })
  

  output$catTable <- renderDataTable({
    out <- catPlotDataCount()
    
    out
  }, options = list(searching = FALSE, paging = FALSE),
  rownames= FALSE)
})
