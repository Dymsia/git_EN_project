

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
  
  }) # end shinyServer
