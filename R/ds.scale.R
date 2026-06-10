#'
#' @title Standardizes a server-side vector
#' @description Scales / standardizes a server-side vector using the scale function
#' 
#' @details 
#' Note: \code{add.column = TRUE} is only valid for data-frame inputs.
#' 
#' Server function called: \code{scaleDS}
#' 
#' @param x A character string specifying the server-side vector For data-frame columns, use the format \code{df$column}. 
#' @param newobj A character string for the name of the object that will be created on the server. Default is \code{"scaled.data"}.
#' @param add.column Logical. If \code{FALSE}, the result is created as a new server-side object; 
#' if \code{TRUE}, the result is added as a new column in the existing data-frame. Default is \code{FALSE}.
#' @param datasources A list of \code{\link[DSI]{DSConnection-class}} objects obtained after login. 
#' If the \code{datasources} argument is not specified the default set of connections will be used: 
#' see \code{\link[DSI]{datashield.connections_default}}.
#' 
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
#'                  table = "GWAS.ega_phenotypes_1", driver = "OpalDriver")
#'   builder$append(server = "study2",
#'                  url = "https://opal-demo.obiba.org",
#'                  user = "dsuser", password = "P@ssw0rd",
#'                  table = "GWAS.ega_phenotypes_2", driver = "OpalDriver")
#'   logindata <- builder$build()
#' 
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#' 
#'   ds.make(toAssign = "D$energy", newobj = 'energy.obj', datasources = connections)
#' 
#'   # Example 1: Give a column, save as a new column
#'   ds.scale(x="D$age_recruitment", newobj="scaled.age.recruitment", add.column=TRUE)
#' 
#'   # Example 2: Give a numeric object, save as a new object
#'   ds.scale(x="energy.obj", newobj="scaled.energy", add.column=FALSE)
#'   
#'   
#'   # Clear the Datashield R sessions and logout           
#'   datashield.logout(connections)
#' }
#'  
#' @author Zulal Bekerecioglu
#' @export
#'

ds.scale <- function(x=NULL, newobj="scaled.data", add.column=FALSE, datasources = NULL) {
  
  # look for DS connections
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  # if x is empty, throw an error
  if (is.null(x)) {
    stop("Argument 'x' cannot be NULL. Please provide an object name or column name(s).")
  }
  
  # Build the call string
  args <- c(
    sprintf('x = "%s"', x),
    sprintf('newobj = "%s"', newobj),
    sprintf('add.column = "%s"', add.column)
  )
  
  
  cally <- paste0("scaleDS(", paste(args, collapse = ", "), ")")
  
  is_dataframe <- grepl("\\$", x)
  
  if(is_dataframe) {
    # Extract data-frame and column name 
    dataframe_name <- strsplit(x, "\\$")[[1]][1]
    column_name <- strsplit(x, "\\$")[[1]][2]
  }
  
  # Data-frame object
  # Save as a new object
  if(!add.column&&is_dataframe){
    DSI::datashield.assign(datasources, symbol = newobj, as.symbol(cally))
    
    
  } else if(add.column&&is_dataframe) { # Save as a new column
    DSI::datashield.assign(datasources, symbol = dataframe_name, as.symbol(cally))
  
    
  } else if(!add.column&&!is_dataframe) { # Vector object, only saving as a new object is valid
    DSI::datashield.assign(datasources, symbol = newobj, as.symbol(cally))
    
    
  } else {
    stop("The argument 'x' is not a column. To save the result, 
         either provide a column (e.g., df$colname) or set add.column = FALSE.", call. = FALSE)
    
  }
  
  #############################################################################################################
  # Check that the object (or dataframe with new column) was successfully created on all servers
  #############################################################################################################
  
  test.obj.name <- if (!add.column) newobj else dataframe_name
  
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
  
  no.errors <- TRUE
  for (nd in 1:num.datasources) {
    if (studyside.message[[nd]] != "ALL OK: there are no studysideMessage(s) on this datasource.") {
      no.errors <- FALSE
    }
  }
  
  
  if (no.errors) {
    if (add.column) {
      # Check if the new column exists in the dataframe on all servers
      calltext.names <- call("namesDS", dataframe_name)
      df.colnames <- DSI::datashield.aggregate(datasources, calltext.names)
      
      col.exists.in.all.sources <- TRUE
      for (j in 1:length(df.colnames)) {
        if (!(newobj %in% df.colnames[[j]])) {
          col.exists.in.all.sources <- FALSE
        }
      }
      
      if (col.exists.in.all.sources) {
        validity.check <- paste0("New column <", newobj, "> successfully added to dataframe <", dataframe_name, "> in all sources.")
      } else {
        validity.check <- paste0("Warning: column <", newobj, "> not found in dataframe <", dataframe_name, "> in one or more sources. Check with ds.names().")
      }
    } else {
      validity.check <- paste0("<", test.obj.name, "> appears valid in all sources.")
    }
    
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
#ds.scale