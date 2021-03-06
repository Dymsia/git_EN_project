require(caret)
library(ggplot2)
library(AppliedPredictiveModeling) # used in caret plot
library(htmltools)
library(DT)


require(tools)
require(httr)
require(RWeka)
require(BBmisc)
require(checkmate)
require(ParamHelpers)
require(farff)
require(OpenML)
require(ggplot2)
require(DT)
require(parallelMap)
require(rmarkdown)
require(mlr)
require(stringi)
require(readr)
require(xtable)
require(plyr)
require(GGally)
require(plotly)


helper.files = list.files(path = "./helpers", pattern="*.R")
helper.files = paste0("helpers/", helper.files)

for (i in seq_along(helper.files)) {
  source(helper.files[i], local = TRUE)
}


# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

shinyServer(function(input,output,session){
  
  preproc.data = reactiveValues(data = NULL, data.collection = NULL)
  ##### data import #####
  
  output$import.ui = renderUI({
     type = input$import.type;
     if (is.null(type))
       type = "CSV"
     makeImportSideBar(type)
  })
  
  data = reactiveValues(data = NULL, data.test = NULL, data.name = NULL)
  
  
  observe({
    reqAndAssign(input$import.type, "import.type")
    if (is.null(import.type)) {
      data$data = NULL
    }  else if (import.type == "CSV") {
      f = input$import.csv$datapath
      if (is.null(f)) {
        data$data = NULL
      } else {
        data$data = read.csv(f, header = input$import.header, sep = input$import.sep,
                             quote = input$import.quote)
      }
    }
    preproc.data$data = isolate(data$data)
  })
  
  data.name = reactive({
    type = input$import.type
    
      if (type == "CSV") {
        return(input$import.csv$name)
      }
      
      
     
  })
  
  observe({
    reqAndAssign(input$import.type, "import.type")
    data$data.name = data.name()
  })
  
  
  output$import.preview = DT::renderDataTable({
    reqAndAssign(data$data, "d")
    colnames(d) = make.names(colnames(d))
    d
  }, options = list(scrollX = TRUE),
  caption = "You imported the following data set")
  
  
  ###### vyvod summary uploaded dataset
  output$data.summary.box = renderUI({
    # if (input$show_help)
    #   text = htmlOutput("summary.text")
    # else
    #   text = NULL
    # 
    # if (input$preproc_df == "training set")
    #   title = "Data Summary of Training Set"
    # else
    #   title = "Data Summary of Test Set"
    
    title = "Summary of uploaded dataset"
    ui = box(width = 12, title = title,
            # text,
             br(),
             htmlOutput("data.summary.caption"),
             DT::dataTableOutput("summary.datatable")
    )
    ui
  })
  
  output$data.summary.caption = renderUI({
    capt = sprintf("Your dataset contains %i observations. Click on one or more variables for visualisation!", nrow(data$data))
    helpText(capt)
  })
  
  
  output$summary.datatable = DT::renderDataTable({
    data.summary()
  }, options = list(scrollX = TRUE))# , caption = capt)
  
  
  ### blok dla visualizacii general data summary
  output$summary.vis.hist = renderUI({
    list(
      column(3,
             radioButtons("summary.vis.dens", "Plot type", choices = c("Histogram", "Density"),
                          selected = "Histogram", inline = TRUE)
      ),
      column(9,
             sliderInput("summary.vis.hist.nbins", "Number of bins", min = 1L, max = 100L,
                         value = 30L, step = 1L, width = "95%")
      )
    )
  })
  
  observeEvent(input$summary.vis.dens, {
    if (input$summary.vis.dens == "Density")
      shinyjs::hide("summary.vis.hist.nbins", animType = "fade")
    else
      shinyjs::show("summary.vis.hist.nbins", animType = "fade")
  })
  
  observeEvent(summary.vis.var(), {
    feature = summary.vis.var()
    if (length(feature) > 0L) {
      shinyjs::show("summary.vis.box", anim = TRUE)
      if (length(feature) == 1L) {
        if (feature %in% factorFeatures()) {
          shinyjs::hide("summary.vis.hist", animType = "fade")
        } else {
          shinyjs::show("summary.vis.hist", anim = TRUE)
        }
      } else
        shinyjs::hide("summary.vis.hist", animType = "fade")
    } else {
      shinyjs::hide("summary.vis.box", anim = TRUE)
    }
  })
  
  summary.vis.out = reactive({
    reqAndAssign(summary.vis.var(), "feature")
    withProgress( 
      message = 'Plotting in progress',
      detail = 'This may take a while...', value = 0, {
        for (i in 1:5) {
          incProgress(1/5)
          Sys.sleep(0.015)
          
        }
        
      })
    
    d = na.omit(data$data)
    reqNFeat(feature, d)
    barfill = "#3c8dbc"
    barlines = "#1d5a92"
    if (length(feature) == 1L) {
      if (feature %in% numericFeatures()) {
        reqAndAssign(input$summary.vis.dens, "density")
        x = as.numeric(d[,feature])
        summary.plot = ggplot(data = d, aes(x = x))
        
        if (density == "Density")
          summary.plot = summary.plot + geom_density(fill = "pink", alpha = 0.7)
        else
          summary.plot = summary.plot + geom_histogram(colour = barlines, fill = barfill, stat = "bin", bins = input$summary.vis.hist.nbins)
        
        summary.plot = summary.plot + xlab(feature) +
          geom_vline(aes(xintercept = quantile(x, 0.05)), color = "black", size = 0.5, linetype = "dashed") +
          geom_vline(aes(xintercept = quantile(x, 0.95)), color = "black", size = 0.5, linetype = "dashed") +
          geom_vline(aes(xintercept = quantile(x, 0.5)), color = "red", size = 1, linetype = "dashed")
        summary.plot = addPlotTheme(summary.plot)
        summary.plot
      } else {
        class = d[,feature]
        summary.plot = ggplot(data = d, aes(x = class)) + 
          geom_bar(aes(fill = class), stat = "count") + xlab(feature) +
          guides(fill = FALSE)
        summary.plot = addPlotTheme(summary.plot) 
        summary.plot
      }
    } else if (length(feature) > 1L) {
      summary.plot = ggpairs(data = d, columns = feature, mapping = ggplot2::aes_string(color = input$modelLabelUI),
                             upper = list(continuous = wrap("cor", size = 5)), 
                             lower = list(continuous = "smooth")) 
      summary.plot
    }
  })
  
  output$summary.vis = renderPlotly({
    ggplotly(summary.vis.out()) 
  })
  
  summary.vis.collection = reactiveValues(var.plots = NULL)#var.names = NULL, var.plots = NULL)
  
  observeEvent(summary.vis.out(), {
    q = summary.vis.out()
    feat = isolate(summary.vis.var())
    feat = paste(feat, collapse = ".x.")
    
    # summary.vis.collection$var.names = c(summary.vis.collection$var.names, feat)
    summary.vis.collection$var.plots[[feat]] = q
    
  })
  
  summary.vis.var = reactive({
    reqAndAssign(data$data, "d")
    pos.x = colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
    d = dropNamed(d, drop = pos.x)
    s = summarizeColumns(d)
    s$name[input$summary.datatable_rows_selected]
  })
  
 ################ vyvod data prepoc_data 
  
  output$preproc_data = DT::renderDataTable({
    validateData(data$data)
    validatePreprocData(preproc.data$data, input$preproc_df)
    d = preproc.data$data
    colnames(d) = make.names(colnames(d))
    d
  }, options = list(lengthMenu = c(5, 20, 50), pageLength = 9, scrollX = TRUE)
  )
  
  #### Preproc out
  
  output$preproc_out = renderUI({
    switch(input$preproc_method,
           "Drop variable(s)" = preproc_dropfeature(),
           "Convert variable" = preproc_convar(),
           "Normalize variables" = preproc_normfeat(),
          # "Remove constant variables" = preproc_remconst(),
           "Recode factor levels" = c(preproc_recodelevels()),#  preproc_recodelevels_levels()),
          # "Cap large values" = preproc_caplarge(),
          # "Subset" = preproc_subset(),
           "Create dummy features" = preproc_createdummy(),
           "Impute" = preproc_impute()
          # "Feature selection" = preproc_feature_selection(),
          # "Merge small factor levels" = preproc_merge_factor_levels()
    )
  })
  
  
  
  
  
  
 # funckcii help dla preprocessinga

  
  # knopka go
  
  ### preproc go ###
  
  output$preproc.go = renderUI({
    label = switch(input$preproc_method,
                   "Drop variable(s)" = "drop",
                   "Convert variable" = "convert",
                   "Normalize variables" = "normalize",
                  # "Remove constant variables" = "remove",
                   "Recode factor levels" = "recode",
                  # "Cap large values" = "cap",
                   "Subset" = "subset",
                   "Create dummy features" = "make dummies",
                   "Impute" = "impute"
                   #"Feature selection" = "select features",
                   #"Merge small factor levels" = "merge factor levels"
    )
    bsButton("preproc_go", label, icon = icon("magic"))
  })
  
  # knopka UNDO
  counter = reactiveValues(count = 1L) # opredelaem kak reaktivnuu
  
  observe({
    disabled = (counter$count == 1L)
    updateButton(session, inputId = "preproc_undo", disabled = disabled)
  })
  observeEvent(input$preproc_go, {
    df.type = isolate(input$preproc_df)
    preproc.df = isolate(preproc.data$data)
    
    preproc.data$data.collection = c(preproc.data$data.collection, list(preproc.df))
    counter$count = counter$count + 1L
    
   
      data$data.test = preproc.data$data
    
  })
  
  observeEvent(input$preproc_undo, {
    req(counter$count > 1L)
    preproc.data$data = preproc.data$data.collection[[counter$count - 1L]]
    preproc.data$data.collection = preproc.data$data.collection[seq_len(counter$count - 1L)]
    ta = preproc.data$data
   
      data$data.test = preproc.data$data
    
    
   
    counter$count = counter$count - 1L
  })
  
  
  # Statistics summary used in preproc 
  output$summary.datatable2 = DT::renderDataTable({
    data.summary()
  }, options = list(scrollX = TRUE))
  
  data.summary = reactive({
    # if (input$preproc_df == "training set")
    #   d = data$data
    # else
    #   d = data$data.test
    d = preproc.data$data
    
    validateData(d)
    colnames(d) = make.names(colnames(d))
    pos.x = colnames(Filter(function(x) "POSIXt" %in% class(x) , d))
    d = dropNamed(d, drop = pos.x)    
    summarizeColumns(d)
  })
  
  #### download processed data
  
  output$preproc.data.download = downloadHandler(
    filename = function() {
      pasteDot(data.name(), "_processed", "csv")
    },
    content = function(file) {
      write.csv(preproc.data$data, file)
    }
  )
  
  
  
  
  
  ###  dropFeature ###
  
  preproc_dropfeature = reactive({
    d = preproc.data$data
    req(input$preproc_method)
    # if (input$show_help)
    #   help = htmlOutput("dropfeature.text")
    # else
    #   help = NULL
    makePreprocUI(
      # help,
      selectInput("dropfeature_cols", "Choose column(s)",
                  choices =  as.list(colnames(d)), multiple = TRUE)
    )
  })
  
  dropfeature_target = reactive({
    tar = input$dropfeature_cols
    ifelse(is.null(tar) | tar == "", character(0L), tar)
  })
  
  observeEvent(input$preproc_go, {
    req(input$preproc_method == "Drop variable(s)")
    d = preproc.data$data
    preproc.data$data = dropNamed(d, dropfeature_target())
  })
  
  
  ### convert columns
  
  
  preproc_convar = reactive({
    req(input$preproc_method)
    d = isolate(preproc.data$data)
    # if (input$show_help)
    #   help = htmlOutput("convar.text")
    # else
    #   help = NULL
    makePreprocUI(
      # help,
      selectInput("convar_cols", "Choose column", 
                  choices = as.list(colnames(d)), multiple = FALSE),
      selectInput("convar_type", "Convert to",
                  choices = c("numeric", "factor", "integer"))
    )
  })
  
  convar_target = reactive({
    tar = input$convar_cols
    ifelse(is.null(tar) | tar == "", character(0L), tar)
  })
  
  observeEvent(input$preproc_go, {
    req(input$preproc_method == "Convert variable")
    type = input$convar_type
    
    if (type == "numeric")
      preproc.data$data[,convar_target()] = as.numeric(preproc.data$data[,convar_target()])
    
    if (type == "factor")
      preproc.data$data[,convar_target()] = as.factor(preproc.data$data[,convar_target()])
    
    if (type == "integer")
      preproc.data$data[,convar_target()] = as.integer(preproc.data$data[,convar_target()])
  })
  
  
  ### normalizeFeatures
  
  preproc_normfeat = reactive({
    d = preproc.data$data
    choices = numericFeatures()
    req(input$preproc_method)
    # if (input$show_help)
    #   help = htmlOutput("normfeat.text")
    # else
    #   help = NULL
    makePreprocUI(
     # help,
      list(
        conditionalPanel("input.normfeat_cols == null",
                         selectInput("normfeat_exclude", "Exclude column(s) (optional)",width = '99%', choices = choices, multiple = TRUE)
        ),
        conditionalPanel("input.normfeat_exclude == null",
                         selectInput("normfeat_cols", "Choose columns (optional)",width = '99%', choices = choices, multiple = TRUE)
        )
      ),
      list(
        selectInput("normfeat_method", "Choose method", selected = "scale",
                    choices = c("center", "scale", "standardize", "range")),
        # FIXME What would be the best range?
        conditionalPanel("input.normfeat_method == 'range'",
                         sliderInput("normfeat_range", "Choose range",min = -10L, max = 10L,
                                     value = c(0, 1), round = TRUE, step = 1L)
        ),
        conditionalPanel("input.normfeat_method != 'center'",
                         selectInput("normfeat_on_constant", "How should constant vectors be treated?", selected = "quiet",
                                     choices = c("quiet", "warn", "stop"))
        )
      )
    )
  })
  
  normfeat_target = reactive({
    tar = input$normfeat_exclude
    ifelse(is.null(tar) | tar == "", character(0L), tar)
  })
  
  observeEvent(input$preproc_go, {
    req(input$preproc_method == "Normalize variables")
    d = isolate(preproc.data$data)
    preproc.data$data = normalizeFeatures(d, target = normfeat_target(), method = input$normfeat_method, cols = input$normfeat_cols,
                                          range = input$normfeat_range, on.constant = input$normfeat_on_constant)
  })
  
  ##### data summary #####
  
  # numeric variables
  numericFeatures = reactive({
    # req(data$data)
    d = data$data
    return(colnames(Filter(is.numeric, d)))
  })
  
  # factor variables
  factorFeatures = reactive({
    # req(data$data)
    d = data$data
    return(colnames(Filter(is.factor, d)))
  })
  
  
  ### recode levels
  
  preproc_recodelevels = reactive({
    req(input$preproc_method == "Recode factor levels")
    d = preproc.data$data
    fnames = colnames(Filter(is.factor, d))
    col = preproc_recode$col
    if (is.null(col) | col %nin% fnames)
      col = "-"
    # if (input$show_help)
    #   help = htmlOutput("recodelevels.text")
    # else
    #   help = NULL
    makePreprocUI(
      # help,
      selectInput("recodelevels_col", "Choose factor to modify",
                  choices =  c("-",fnames), selected = col),
      selectInput("recodelevels_method", "Choose method", 
                  choices =  c("Drop empty factor levels" = "drop", "Rename factor levels" = "recode",
                               "Define factor level as NA" = "findNA")),
      conditionalPanel("input.recodelevels_method == 'recode'",
                       if (!is.null(col)) {
                         if (col != "-")
                           makeRecodeLevelUI(levels(d[, col]))
                       }
      ),
      conditionalPanel("input.recodelevels_method == 'findNA'",
                       if (!is.null(col)) {
                         if (col != "-")
                           selectInput("recodelevels_levels", "Choose level to set to NA",
                                       choices = levels(d[, col]))
                       }
      )
    )
  })
  
  
  
  # observeEvent(data.name, {
  #   updateSelectInput(session, "recodelevels_cols", selected = "-", choices = "-")
  # })
  
  preproc_recode = reactiveValues(col = NULL)
  
  observe({
    # req(input$recodelevels_cols)
    inp = input$recodelevels_col
    if (is.null(inp))
      inp = "-"
    preproc_recode$col = inp
  })
  
  # observe({
  #   req(data.name())
  #   data.name()
  #   preproc_recode$levels = NULL
  # })
  
  observeEvent(input$preproc_go, {
    req(input$preproc_method == "Recode factor levels")
    d = preproc.data$data
    col = input$recodelevels_col
    method = input$recodelevels_method
    if (!is.null(col) & "-" %nin% col) {
      if (method == "drop") {
        cols.ex = colnames(d)[colnames(d) %nin% col]
        preproc.data$data = droplevels(d, except = cols.ex)
      } else {
        fac = preproc.data$data[, col]
        if (method == "recode") {
          new.levs = vcapply(levels(fac), function(lev) {
            input[[paste("recode_", lev)]]
          })
          names(new.levs) = levels(fac)
          preproc.data$data[, col] = revalue(fac, new.levs)
        } else {
          fac = as.character(fac)
          fac[fac == input$recodelevels_levels] = NA
          preproc.data$data[, col] = factor(fac)
        }
      }
      
    }
  })
  
  ## createDummyFeatures
  
  preproc_createdummy = reactive({
    reqAndAssign(preproc.data$data, "d")
    req(input$preproc_method)
    choices = factorFeatures()
    validate(need(length(choices) > 0L, "No factor features available!"))
    # if (input$show_help)
    #   help = htmlOutput("createdummy.text")
    # else
    #   help = NULL
    makePreprocUI(
      # help,
      selectInput("createdummy_method", "Choose Method", selected = "1-of-n",
                  choices = c("1-of-n", "reference"))
      # conditionalPanel("input.createdummy_cols == null",
      #                  selectInput("createdummy_exclude", "Exclude column(s) (optional)",
      #                              choices = choices, multiple = TRUE)
      # ),
      # conditionalPanel("input.createdummy_exclude == null",
      #                  selectInput("createdummy_cols", "Choose specific column(s) (optional)",
      #                              choices = choices, multiple = TRUE)
      # )
    )
  })
  
   createdummy_target = reactive({
     tar = input$createdummy_exclude
     ifelse(is.null(tar) | tar == "", character(0L), tar)
   })

  observeEvent(input$preproc_go, {
    req(input$preproc_method == "Create dummy features")
    d = isolate(preproc.data$data)
    preproc.data$data = createDummyFeatures(d, target = createdummy_target(),
                                            method = input$createdummy_method, cols = input$createdummy_cols)
  })

  ### Impute
  
  preproc_impute = reactive({
    req(input$preproc_method)
    reqAndAssign(preproc.data$data, "d")
    # if (input$show_help)
    #   help = htmlOutput("impute.text")
    # else
    #   help = NULL
    makePreprocUI(
      # help,
      list(
        selectInput("impute_exclude", "Exclude column(s) (optional)",
                    choices =  as.list(colnames(d)), multiple = TRUE),
        selectInput("impute_methods_num", "Choose imputation method for numeric variables",
                    selected = "imputeMean",
                    choices = c("imputeConstant", "imputeMean", "imputeMedian",
                                "imputeMode", "imputeMin", "imputeMax", "imputeNormal", "imputeHist")
        ),
        selectInput("impute_methods_fac", "Choose imputation method for factor variables", selected = "imputeMode",
                    choices = c("imputeConstant", "imputeMode"))
      ),
      list(
        conditionalPanel("input.impute_methods_num == 'imputeConstant'",
                         numericInput("impute_constant_num_input", "Constant value for numerical features",
                                      min = -Inf,  max = Inf, value = 0)
        ),
        conditionalPanel("input.impute_methods_fac == 'imputeConstant'",
                         numericInput("impute_constant_fac_input", "Constant value for factors", min = -Inf,  max = Inf, value = 0)
        )
      )
    )
  })
  
  observeEvent(input$preproc_go, {
    req(input$preproc_method == "Impute")
    d = isolate(preproc.data$data)
    reqAndAssign(input$impute_methods_num, "num")
    reqAndAssign(input$impute_methods_fac, "fac")
    
    if (num == "imputeConstant" ) {
      num_impute = imputeConstant(input$impute_constant_num_input)
    } else {
      num_impute = match.fun(num)()
    }
    
    if (fac == "imputeConstant" ) {
      fac_impute = imputeConstant(input$impute_constant_fac_input)
    } else {
      fac_impute = match.fun(fac)()
    }
    
    imputed = impute(d, target = impute_target(), classes = list(numeric = num_impute, factor = fac_impute))
    preproc.data$data = imputed$data
  })
  
  impute_target = reactive({
    tar = input$impute_exclude
    ifelse(is.null(tar) | tar == "", character(0L), tar)
  })
  
 ### konec help funkcii 
  
 
  
  
 
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
  # # reactive object, responsible for loading the main data
  # rawInputData = reactive({
  #   
  #   rawData = input$rawInputFile # tabPanel "Data"
  #   headerTag = input$headerUI;
  #   sepTag = input$sepUI;
  #   quoteTag = input$quoteUI;
  #   
  #   
  #   if(!is.null(rawData)) {
  #     data = read.csv(rawData$datapath,
  #                     header=headerTag,
  #                     sep=sepTag,
  #                     quote=quoteTag);
  #   } else {
  #     return(NULL);
  #   }
  #   
  # });
  
  # Create table output in Data view tapPanel
  output$pre.data <- DT::renderDataTable( 
    
    #rawInputData(),
    preproc.data$data,
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
    # preproc.data$data = isolate(data$data)
     data = preproc.data$data
   # data = rawInputData();
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
    data = preproc.data$data;
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
    
    datas = preproc.data$data
    
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
    
    data = isolate(preproc.data$data);
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
    eval(parse(text = preproc.data$data))  # pomenali staruu rawdada()
  })
  
  # Let user choose columns, and add plot.
  output$column_ui <- renderUI({
    choices <- c("Choose one" = "", names(preproc.data$data))
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
      df <- brushedPoints(preproc.data$data, input$brush, allRows = TRUE)
      
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
    data = preproc.data$data
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
  
  
  
  ###### testing block############################
 
  
  }) # end shinyServer


