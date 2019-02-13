require(caret)
library(ggplot2)
library(AppliedPredictiveModeling) # used in caret plot
library(htmltools)
shinyServer(function(input,output,session){
  
  ######################## 1. Eneriss Logo for Dashboard help item:
  #                         --------------------------
  # vstavka logo Eneriss v dashboard help
  output$image2 <- renderImage({
    return(list(src = "images/Eneriss_logo.png", contentType = "image/png", align = "center"))
  }, deleteFile = FALSE)
  
  # vstavka logo Eneriss v dashboard help
  output$image3 <- renderImage({
    return(list(src = "images/Eneriss_logo.png", contentType = "image/png", align = "center"))
  }, deleteFile = FALSE)
  
  ######################## 2. Dla Data preparation item:
  #                           --------------------------
  # reactive object, responsible for loading the main data
  rawInputData = reactive({
    
    rawData = input$rawInputFile # tabPanel "Data"
    headerTag = input$headerUI;
    sepTag = input$sepUI;
    quoteTag = input$quoteUI;
    
    
    if(!is.null(rawData)) {
      data = read.csv(rawData$datapath,
                      header=headerTag,
                      sep=sepTag,
                      quote=quoteTag);
    } else {
      return(NULL);
    }
    
  });
  
  # Create table output in Data view tapPanel
  output$pre.data <- DT::renderDataTable( 
    
    rawInputData(),
    caption = "Dataset:",
    #rownames = T,
    filter = "top",
    extensions = 'Buttons',
    options = list(pageLength = 6, dom = 'Bfrtip',
                   buttons = list('copy',
                                  'print',
                                  list(extend = 'collection',
                                       buttons = c('csv', 'excel', 'pdf'),
                                       text = 'Download'
                                  )
                   )
                   
    )
  )
  
  
  # responsible for selecting the label you want to clasify on
  output$labelSelectUI = renderUI({
    
    data = rawInputData();
    #check if the data is loaded first
    if(is.null(data)){
      return(helpText("Choose a file to load"))
    } else {
      return(selectInput("modelLabelUI","Select Target Feature",colnames(data),colnames(data)[1]));
    }
  });
  
  # responsible for selecting the exploratary variable
  output$PredictorVarsUI = renderUI({
    
    data = rawInputData();
    #check if the data is loaded first
    if(is.null(data)){
      return(helpText("Choose predictors"))
    } else {
      
      return(selectInput("modelPredictorVarsUI","Predictors variables:",colnames(data), selected=names(colnames(data))[-1], multiple=TRUE));
    }
  });
  
  #a feature plot using the caret package
  
  output$caretPlotUI = renderPlot({
    data = rawInputData();
    column = input$modelLabelUI;
    
    #check if the data is loaded first
    if(is.null(data)){
      return()
    } else {
      columnElement = which(colnames(data) == column);
      
      trellis.par.set(theme = col.whitebg(), warn = FALSE)
      transparentTheme(trans = .4)
      p = featurePlot(x=data[,-columnElement],
                      y=data[,columnElement],
                      plot="pairs",
                      auto.key= TRUE,
                      jitter =TRUE);
      print(p);
      
      
    }
  })
  
  output$plotHist <- renderPlot({
    
    datas = rawInputData()
    
    #grab the column
    column = input$modelNumVarUI;
    
    columnElement = which(colnames(datas) == column);
    x    <- datas[,columnElement]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = column,
         main = "Univariate distribution")
  })
  
  # responsible for selecting the num variable  for Histogram
  output$varSelectUI = renderUI({
    
    data = isolate(rawInputData());
    dataNum <- dplyr::select_if(data, is.numeric)
    #check if the data is loaded first
    if(is.null(dataNum)){
      return(helpText("Choose variable"))
    } else {
      return(selectInput("modelNumVarUI","Select numeric Feature",colnames(dataNum),colnames(dataNum)[1]));
    }
  });
  
  ############### for interactives plots
  # pick the dataset
  dataset <- reactive({
    eval(parse(text = input$rawInputData()))
  })
  
  # Let user choose columns, and add plot.
  output$column_ui <- renderUI({
    choices <- c("Choose one" = "", names(rawInputData()))
    tagList(
      selectInput("xvar", "X variable", choices),
      selectInput("yvar", "Y variable", choices),
      conditionalPanel("input.xvar && input.yvar",
                       actionButton("addplot", "Add plot")
      )
    )
  })
  
  
  # One of the very few times you'll see me create a non-reactive
  # session-level variable, and mutate it from within an observer
  plot_count <- 0
  
  # Add a plot when addplot is clicked
  observeEvent(input$addplot, {
    plot_count <<- plot_count + 1
    
    id <- paste0("plot", plot_count)
    # Take a static snapshot of xvar/yvar; the renderPlot we're
    # creating here cares only what their values are now, not in
    # the future.
    xvar <- input$xvar
    yvar <- input$yvar
    
    output[[id]] <- renderPlot({
      df <- brushedPoints(rawInputData(), input$brush, allRows = TRUE)
      
      ggplot(df, aes_string(xvar, yvar, color = "selected_")) +
        geom_point(alpha = 0.6) +
        scale_color_manual(values = c("black", "green", "red")) +
        guides(color = FALSE) +
        xlab(xvar) + ylab(yvar)
    })
    insertUI("#plot_container", where = "beforeEnd",
             ui = div(style = css(display = "inline-block"),
                      plotOutput(id, brush = "brush", width = 275, height = 275)
             )
    )
  })
  
  
  ## Remove Elements ###
  observeEvent(input$remove_button, {
    removeUI(
      selector = paste0('#plot', plot_count)
    )
    plot_count <<- plot_count-1
  })
  
  
  # generate correlation matrix
  output$featurematrixUI = renderPlot({
    data = rawInputData()
    column = input$modelLabelUI;
    
    #check if the data is loaded first
    if(is.null(data)){
      return()
    } else {
      columnElement = which(colnames(data) == column);
      
      withProgress( 
        message = 'Data analysis in progress',
        detail = 'This may take a while...', value = 0, {
          for (i in 1:15) {
            incProgress(1/15)
            Sys.sleep(0.25)
            
          }
          
        })
      
      ggpairs(data, mapping = ggplot2::aes_string(color = input$modelLabelUI))+ theme_bw()  
    }
  })
  
  
  ################## end interactive plots
  
  
  
  }) # end shinyServer


