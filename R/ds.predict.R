#'
#' @title ds.predict
#' @description Generates server-side predictions using the client-side output from \code{ds.glm}.
#' 
#' @details 
#' This function takes the client-side output from \code{ds.glm} and sends the necessary components
#' (coefficients, family, formula, and any categorical variables) to the server for prediction.
#' 
#' Server function called: \code{predictDS2}
#' 
#' @param name The client-side return object from \code{ds.glm}. 
#' @param newdataname A character string specifying the name of the new dataset to be used for predictions.
#' @param type A character string specifying the type of prediction. Options are \code{"response"} or \code{"link"}.
#' @param newobj A character string specifying the name of the output object created on the server. 
#' Default is \code{"new.predictions"}.
#' @param traindataname A character string specifying the name of the dataset used for model training.
#' @param na.action A character string to specify the action to take if missing values are present. Default is \code{"na.pass"}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be used: 
#' see \code{\link[DSI]{datashield.connections_default}}.
#' 
#' 
#' @examples 
#' 
#' \dontrun{
#' 
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#' 
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1", 
#'                  url = "https://opal-demo.obiba.org", 
#'                  user = "dsuser", password = "P@ssw0rd", 
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "https://opal-demo.obiba.org", 
#'                  user = "dsuser", password = "P@ssw0rd", 
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   builder$append(server = "study3", 
#'                  url = "https://opal-demo.obiba.org", 
#'                  user = "dsuser", password = "P@ssw0rd", 
#'                  table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   
#'   # Example: Fit the model using ds.glm for study1 and study2
#'   fitted_model <- ds.glm(formula = "LAB_TSC ~ LAB_HDL + PM_BMI_CONTINUOUS * GENDER + MEDI_LPD",
#'                         data = "D", family = "gaussian", datasources = connections[c("study1", "study2"]))
#'                          
#'   # Predictions for study3
#'   ds.predict(name = fitted_model, newdataname = "D", type = "response",
#'              traindataname="D", na.action="na.omit", datasources = connections["study3"]) 
#'   
#'   
#'   # Clear the Datashield R sessions and logout           
#'   datashield.logout(connections)
#' }
#'  
#' @author Zulal Bekerecioglu
#' @export
#'

ds.predict <- function(name = NULL, newdataname = NULL, type = c("response", "link"),
                       newobj = "new.predictions", traindataname = NULL, 
                       na.action = "na.pass", datasources = NULL) {
  
  # look for DS connections
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  type <- match.arg(type)
  
  # Ensure new data and training data names are provided
  if (is.null(newdataname)) {
    stop("The argument 'newdataname' cannot be empty. Please provide the name of the dataset for predictions.", call. = FALSE)
  }
  
  if (is.null(traindataname)) {
    stop("The argument 'traindataname' cannot be empty. Please provide the name of the training dataset.", call. = FALSE)
  }
  
  # Ensure model object name is provided
  if (is.null(name)) {
    stop("The argument 'name' cannot be empty. Please provide the ds.glm output object.", call. = FALSE)
  }
  
  # Sending necessary components to the server-side
  # Create a coefficients object from the model, numbers only
  ds.make(toAssign = paste0("c(", paste(name$coefficients[, 1], collapse = ", "), 
                            ")"), 
          newobj = 'predictDS_coefficients', datasources = datasources)
  
  # Create family and link function object from the model
  ds.make(toAssign = paste0("\"", paste(name$family$family, "link", name$family$link, sep = "."),
                            "\""), 
          newobj = 'predictDS_family', datasources = datasources)
  
  # Create a formula object from the model
  ds.make(toAssign = paste0(name$formula),
          newobj = 'predictDS_formula', datasources = datasources)
  
  
  # Create a categorical_variables object from the model. Necessary for correct factoring!
  
  # Compare coefficient names from the model output and the elements from the formula
  # if there is a mismatch, check if any of the coefficient names start with a formula element -> Sexmale from Sex, Pclass2 from Pclass
  # if it does, save it as a categorical variable. If there is no factors detected, save an empty list as categorical_variables
  categorical_variables <- c()
  coefficient_names <- names(name$coefficients[, 1])
  formula_elements <- labels(terms(formula(name$formula)))
  main_effects <- formula_elements[!grepl(":", formula_elements)] # excluding terms with :
  
  for(element in main_effects){
    if(!(element %in% coefficient_names)){
      partial_match <- coefficient_names[startsWith(coefficient_names, element)] 
      
      if (length(partial_match) > 0) {
        categorical_variables <- append(categorical_variables, element)
      }
    }
  }
  
  if(length(categorical_variables) > 0) {
    ds.make(toAssign = paste0("c(", paste0("'", categorical_variables, "'", collapse = ", "), ")"),
            newobj = 'predictDS_categorical_variables', datasources = datasources)
  } else {
    ds.make(toAssign = "NULL",
            newobj = 'predictDS_categorical_variables', datasources = datasources)
  }
  
  
  # Build the call string
  cally <- paste0(
    "predictDS(",
    "newdataname = '", newdataname, "', ",
    "traindataname = '", traindataname, "', ",
    "type = '", type, "', ",
    "na.action = '", na.action, "' ",
    ")"
  )
  
  
  DSI::datashield.assign(datasources, symbol = newobj, as.symbol(cally))
  
  # Remove the objects
  ds.rm(x.names = "predictDS_coefficients", datasources = datasources)
  ds.rm(x.names = "predictDS_family", datasources = datasources)
  ds.rm(x.names = "predictDS_formula", datasources = datasources)
  ds.rm(x.names = "predictDS_categorical_variables", datasources = datasources)
  
  
  
  #############################################################################################################
  # Check that the object was successfully created on all servers
  #############################################################################################################
  
  test.obj.name <- newobj
  
  # Run server-side object existence test
  calltext <- call("testObjExistsDS", test.obj.name)
  object.info <- DSI::datashield.aggregate(datasources, calltext)
  
  num.datasources <- length(object.info)
  obj.name.exists.in.all.sources <- TRUE
  obj.non.null.in.all.sources <- TRUE
  
  for (j in 1:num.datasources) {
    if (!object.info[[j]]$test.obj.exists) {
      obj.name.exists.in.all.sources <- FALSE
    }
    if (is.null(object.info[[j]]$test.obj.class) || ("ABSENT" %in% object.info[[j]]$test.obj.class)) {
      obj.non.null.in.all.sources <- FALSE
    }
  }
  
  if (obj.name.exists.in.all.sources && obj.non.null.in.all.sources) {
    return.message <- paste0("A data object <", test.obj.name, "> has been created in all specified data sources.")
  } else {
    return.message.1 <- paste0("Error: A valid data object <", test.obj.name, "> does NOT exist in ALL specified data sources.")
    return.message.2 <- paste0("It is either ABSENT and/or has no valid content/class, see return.info above.")
    return.message.3 <- paste0("Please use ds.ls() or ds.names() to identify where missing.")
    return.message <- list(return.message.1, return.message.2, return.message.3)
  }
  
  # Check for study-side messages
  calltext <- call("messageDS", test.obj.name)
  studyside.message <- DSI::datashield.aggregate(datasources, calltext)
  
  no.errors <- all(unlist(studyside.message) == "ALL OK: there are no studysideMessage(s) on this datasource")
  
  
  if (no.errors) {
    validity.check <- paste0("<", test.obj.name, "> appears valid in all sources.")
    
    return(list(is.object.created = return.message,
                validity.check = validity.check))
    
  } else {
    validity.check <- paste0("<", test.obj.name, "> invalid in at least one source. See studyside.messages:")
    
    return(list(is.object.created = return.message,
                validity.check = validity.check,
                studyside.messages = studyside.message))
  }
  
  #############################################################################################################
  # End of check
  #############################################################################################################
  
}
#ds.predict
