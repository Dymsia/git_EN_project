

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
    
    rawInputData()
  
  )
  
  
  }) # end shinyServer


