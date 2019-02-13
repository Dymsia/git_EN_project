require(caret)
library(ggplot2)
library(AppliedPredictiveModeling) # used in caret plot

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
         main = "Histogram for numeric variables")
  })
  
  # responsible for selecting the num variable  for Histogram
  output$varSelectUI = renderUI({
    
    data = isolate(rawInputData());
    data <- dplyr::select_if(data, is.numeric)
    #check if the data is loaded first
    if(is.null(data)){
      return(helpText("Choose a numeric variable"))
    } else {
      return(selectInput("modelNumVarUI","Select numeric Feature",colnames(data),colnames(data)[1]));
    }
  });
  
  
  }) # end shinyServer


