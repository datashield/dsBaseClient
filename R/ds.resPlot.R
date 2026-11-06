# naming for 'which' argument : https://stat.ethz.ch/R-manual/R-patched/library/stats/html/plot.lm.html

#'
#' @title ds.resplots
#' @description Creates residual plots from \code{ds.glm}, using \code{ds.predict} and \code{ds.scatterPlot}.
#' 
#' @details 
#' This function generates residual plots (residuals vs fitted) and/or QQ-plots
#' for a model fitted with ds.glm, based on the `which` argument.
#' 
#' 
#' @param name The client-side return object from \code{ds.glm}. 
#' @param traindataname The name of the dataset used to train the model.
#' @param which A numeric value deciding what type of plot to return. 
#' 1 = residuals vs fitted plot, 2 = QQ plot, 0 = both (default).
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be used: 
#' see \code{\link[DSI]{datashield.connections_default}}.
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#'  require('DSI')
#'  require('DSOpal')
#'  require('dsBaseClient')
#'
#'  builder <- DSI::newDSLoginBuilder()
#'  builder$append(server = "study1", url = "https://opal-demo.obiba.org",
#'                 user = "dsuser", password = "P@ssw0rd",
#'                 table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'  builder$append(server = "study2", url = "https://opal-demo.obiba.org",
#'                 user = "dsuser", password = "P@ssw0rd",
#'                 table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'  logindata <- builder$build()
#' 
#'  # Log onto the remote Opal training servers
#'  connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#' 
#' 
#'  # Example 1: Fit the model using ds.glm for study1
#'  fitted_model <- ds.glm(formula = "LAB_TSC ~ LAB_HDL + PM_BMI_CONTINUOUS * GENDER + MEDI_LPD",
#'                         data = "D", family = "gaussian", datasources = connections)
#'
#'  # Residuals for the model
#'  ds.resPlot(name = fitted_model, traindataname="D", datasources = connections)
#'
#' 
#'  # Clear the Datashield R sessions and logout
#'  datashield.logout(connections)
#' }
#'  
#' @author Zulal Bekerecioglu
#' @export
#'
#'
ds.resPlot <- function(name, traindataname, which=0, pch = 1, col = "black", 
                       lty = 1, datasources=NULL) {
  
  #source("./ds.predict.R")
  
  which_values <- list("0" = "both", "both" = "both", 
                       "1" = "res",  "res"  = "res",
                       "2" = "qq",   "qq"   = "qq")
  
  which <- tolower(as.character(which))
  
  # 'which' argument can only be 0, 1, or 2.
  if (!which %in% names(which_values)) {
    stop("Invalid `which` argument. Must be one of: 0 / 'both', 1 / 'res', 2 / 'qq'.")
  }
  
  which <- which_values[[which]]
  
  # look for DS connections
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # Ensure model object name is provided
  if (is.null(name)) {
    stop("The argument 'name' cannot be empty. Please provide the ds.glm output object.", call. = FALSE)
  }
  
  if (is.null(traindataname)) {
    stop("The argument 'traindataname' cannot be empty. Please provide the name of the training dataset.", call. = FALSE)
  }
  
  # Generate model predictions on the same training data
  ds.predict(name = name, traindataname = traindataname, newdataname = traindataname,
             datasources = connections, newobj = "prediction.for.res")
  
  # Extract outcome variable name from model formula
  outcome_variable <- all.vars(formula(name$formula)[[2]])
  
  # Compute residuals (Y - fitted) on the server
  ds.make(toAssign = paste0(traindataname, "$", outcome_variable, "-prediction.for.res"), 
          newobj = 'residuals.for.plot', datasources = datasources)
  
  # Standardize residuals
  res_var <- ds.var(x = "residuals.for.plot", type = "split", datasources = datasources)
  res_sd <- sqrt(res_var[["Variance.by.Study"]][, "EstimatedVar"])
  names(res_sd) <- rownames(res_var[["Variance.by.Study"]])
  
  for (i in seq_along(datasources)) {
    ds.make(toAssign = paste0("residuals.for.plot / ", res_sd[i]),
            newobj = "std.residuals.for.plot",
            datasources = datasources[i])
  }
  
  # Get the anonymised scatterplot points for residuals
  pdf(NULL) # to prevent unwanted plot output
  plottedpoints <- as.data.frame(ds.scatterPlot(x='prediction.for.res', y='std.residuals.for.plot', type='combine', 
                                                datasources=datasources, return.coords=TRUE)[1])
  
  if (!interactive()) grDevices::pdf("resplots.pdf") else grDevices::dev.new()
  
  ds.rm(x.names = "prediction.for.res", datasources = datasources)
  ds.rm(x.names = "std.residuals.for.plot", datasources = datasources)
  ds.rm(x.names = "residuals.for.plot", datasources = datasources)
  
  # Rename columns
  names(plottedpoints) <- c("Fitted values", "Residuals")
  
  if (which %in% c("res", "both")) {
    plot(plottedpoints$"Fitted values", plottedpoints$Residuals, pch = pch, 
         col = col, lty = lty, main = "Residuals vs Fitted", 
         xlab = "Predicted values", ylab = "Residuals")
    
    abline(h = 0, lty = 2, col = "gray")
    
    lines(lowess(plottedpoints$"Fitted values", plottedpoints$Residuals), col = "red")
  }
  
  
  if (which %in% c("qq", "both")) {
    qqnorm(plottedpoints$Residuals, main = "Q-Q Plot of Residuals", pch = pch, 
         col = col)
    qqline(plottedpoints$Residuals, col = "black", lty = 2)
  }
  
}
