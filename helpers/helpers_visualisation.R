# FIXME: Should be done with validate/need
validatePlotLearnerPrediction = function(tsk.type, fnames, feats) {
  res = NULL
  validateNumFeatures(fnames)
  nfeats = length(feats)
  if (tsk.type == "regr") {
    validate(need(nfeats %in% 1:2,
      "You must choose one or two features to plot learner predictions."))
  } else if (tsk.type == "classif") {
    validate(need(nfeats == 2L, "You must choose exactly two features to plot learner predictions."))
  }
  return(res)
}

validatePlotLearnerPredictionNoText = function(tsk.type, fnames, feats) {
  res = NULL
  validateNumFeatures(fnames)
  nfeats = length(feats)
  shinyjs::hide("prediction.plot")
  if (tsk.type == "regr") {
    validate(need(nfeats %in% 1:2, message = FALSE))
  } else if (tsk.type == "classif") {
    validate(need(nfeats == 2L, message = FALSE))
  }
  shinyjs::show("prediction.plot")
  return(NULL)
}

checkPlotROCCurves = function(lrn) {
  validate(
    need(lrn$predict.type == "prob", "You must predict probabilities to plot ROC curves.")
  )
}

checkPlotPartialDependency = function(tsk.type, lrn, fnames) {
  validateNumFeatures(fnames)
  if (tsk.type == "classif") {
    validate(
      need(lrn$predict.type == "prob", "You must predict probabilities to plot partial dependency plots.")
    )
  }
}


# # FIXME: maybe we want this as a helper too in mlr directly plot pd plot for one feature??
# sPlotPartialDep = function(input, task, learners) {
#   lrn = input$partialdep.learner
#   lrn = learners[[lrn]]
#   mod = train(lrn, task)
#   fn = input$partialdep.feature
#   pd = generatePartialDependenceData(mod, task, features = fn)
#   plotPartialDependence(pd)
# }


makeVisualisationSelectionUI = function(tsk) {
  if (tsk$type == "classif") {
    if (length(getTaskClassLevels(tsk)) == 2) {
      vis.inp = selectInput("prediction.plot.sel", "Choose plot",
        choices = c("prediction", "residuals", "partial dependency", "confusion matrix", "ROC"),
        selected = "prediction plot", width = 200
      )
    } else {
      vis.inp = selectInput("prediction.plot.sel", "Choose plot",
        choices = c("prediction", "residuals", "partial dependency", "confusion matrix"),
        selected = "prediction plot", width = 200
      )
    }
  } else {
    vis.inp = selectInput("prediction.plot.sel", "Choose plot",
      choices = c("prediction", "residuals", "partial dependency"),
      selected = "prediction plot", width = 200
    )
  }
  return(vis.inp)
}


addPlotTheme = function(plot.obj) {
  plot.theme = theme(axis.line = element_line(size = 1, colour = "black"),
    panel.grid.major = element_line(colour = "#d3d3d3"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_blank(),
    axis.text.x = element_text(colour = "black", size = 1),
    axis.title.x = element_text(vjust = 0.5),
    axis.text.y = element_text(colour = "black", size = 1))
  plot.obj + theme_bw() + plot.theme
}

