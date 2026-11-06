#'
#' @title ds.round
#' @description Generates objects using a server-side object, which can be either a vector or 
#' a data-frame column. Supports five operations:  
#' 1. (\code{round})  
#' 2. (\code{ceiling})  
#' 3. (\code{floor})
#' 4. (\code{trunc})
#' 5. (\code{signif})
#' where each function in baseR is applied on the server side to the specified object.
#' 
#' @details 
#' Note: \code{add.column = TRUE} is only valid for data-frame inputs.
#' 
#' Server function called: \code{DateDS}
#' 
#' @param x Character vector specifying the server-side object(s). For data-frame columns, use the format \code{df$column}. 
#' @param type Character string specifying the operation: \code{"round"}, \code{"ceiling"}, \code{"floor"},
#' \code{trunc}, or \code{"signif"}.
#' @param digits Number of digits to be used in arguments \code{"round"} and \code{"signif"}.
#' @param add.column Logical. If \code{FALSE}, the result is created as a new server-side object; 
#' if \code{TRUE}, the result is added as a new column in the existing data-frame. Default is \code{FALSE}.
#' @param newobj Character string for the name of the object that will be created on the server. Default is \code{"rounding.result"}.
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
#'                  table = "DASIM.DASIM1", driver = "OpalDriver")
#'   builder$append(server = "study2", 
#'                  url = "https://opal-demo.obiba.org", 
#'                  user = "dsuser", password = "P@ssw0rd", 
#'                  table = "DASIM.DASIM2", driver = "OpalDriver")
#'   logindata <- builder$build()
#'   
#'   # Log onto the remote Opal training servers
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D") 
#'   
#'   ds.make(toAssign = "D$LAB_TSC", newobj = 'LAB.TSC.obj', datasources = connections)
#'   
#'   # Example 1: Give a numeric object, save as a new object
#'   ds.round("LAB.TSC.obj", digits=2, add.column = FALSE)
#'            
#'   # Example 2: Give a column, save as a new column.
#'   ds.round("D$LAB_HDL", type = "ceiling", newobj = "LAB_rounded_HDL", add.column = TRUE)
#'   
#'   
#'   # Clear the Datashield R sessions and logout           
#'   datashield.logout(connections)
#' }
#'  
#' @author Zulal Bekerecioglu
#' @export
#'
#'
#'
ds.round <- function(x=NULL, type=c("round", "ceiling", "floor", "trunc", "signif"), digits=0, 
                     add.column = FALSE, newobj = "rounding.result", datasources = NULL) {

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
  
  type <- match.arg(type)
  
  
  # Build the call string
  args <- c(
    sprintf('x = "%s"', x),
    sprintf('type = "%s"', type),
    sprintf('digits = %s', digits),
    sprintf('add.column = %s', add.column),
    sprintf('newobj = "%s"', newobj)
  )
  
  cally <- paste0("roundDS(", paste(args, collapse = ", "), ")")
  
  if(!add.column){
    # Save the rounding result as a new object
    DSI::datashield.assign(datasources, symbol = newobj, as.symbol(cally))
    
  } else {
    # If add.column is TRUE, first check whether the object is a column in a dataframe, 
    # if it is, save the result as a new column in that dataframe
    
    # Check if x contains a $
    if(!grepl("\\$", x)){
      stop("The argument `x` is not a column. To save the result, either provide a column (e.g., df$colname) or set add.column = FALSE.", call. = FALSE)
    } else {
      # Extract dataframe name 
      dataframe_name <- strsplit(x, "\\$")[[1]][1]
      
      # Assign as a column in the dataframe
      DSI::datashield.assign(datasources, symbol = dataframe_name, as.symbol(cally))
      
    }
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
        validity.check <- paste0("New column <", newobj, "> appears valid in all sources.")
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
#ds.round
