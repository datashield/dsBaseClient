#'
#' @title Creates date objects using a server-side object
#' @description Generates objects using a server-side object, which can be either a vector or 
#' a data-frame column. Supports three operations:
#'   
#' 1. Extract components of a date (\code{extractdate})  
#' 
#' 2. Combine numeric year, month, and day into a full date (\code{makedate})  
#' 
#' 3. Compute the time interval between two dates (\code{timebetween})
#' 
#' @details 
#' If the input is a data-frame column, it must be provided in the \code{x} argument as \code{df$column}. 
#' Inputs for \code{extractdate} and \code{timebetween} must be date objects.  
#' For \code{makedate}, three numeric vectors (year, month, day) must be provided in the correct order.  
#' The \code{add.column} argument determines whether the result is added as a new column in the existing 
#' data-frame (\code{TRUE}), or created as a new server-side object (\code{FALSE}).  
#' For \code{timebetween}, \code{months(1)} and \code{years(1)} are calendar periods (from the \code{lubridate} package). 
#' The expression \verb{interval() %/% months(1)} counts whole calendar months between two dates.  
#' Examples: Jan 31 -> Feb 2 = 0 months; Jan 31 -> Mar 2 = 1 month.  
#' \code{days(1)}, it counts a fixed 24-hour duration.  
#' Note: \code{add.column = TRUE} is only valid for data-frame inputs.
#' 
#' Server function called: \code{dateDS}
#' 
#' @param x Character vector specifying the server-side object(s). For data-frame columns, use the format \code{df$column}. 
#' @param type Character string specifying the operation: \code{"extractdate"}, \code{"makedate"}, or \code{"timebetween"}.
#' @param newobj Character string for the name of the object that will be created on the server. Default is \code{"date.result"}.
#' @param unit Character string specifying the unit for \code{extractdate} or \code{timebetween}: \code{"days"}, \code{"months"}, or \code{"years"}.
#' @param add.column Logical. If \code{FALSE}, the result is created as a new server-side object; 
#' if \code{TRUE}, the result is added as a new column in the existing data-frame. Default is \code{FALSE}.
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
#'                url = "https://opal-demo.obiba.org",
#'                user = "dsuser", password = "P@ssw0rd",
#'                table = "GWAS.ega_phenotypes_1", driver = "OpalDriver")
#'   builder$append(server = "study2",
#'                url = "https://opal-demo.obiba.org",
#'                user = "dsuser", password = "P@ssw0rd",
#'                table = "GWAS.ega_phenotypes_2", driver = "OpalDriver")
#' 
#'   logindata <- builder$build()
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   ds.make(toAssign = "D$date_diagnosis",
#'           newobj = 'diagnosis_date', datasources = connections)
#'   ds.date(x="D$date_diagnosis", type = "extractdate",
#'         newobj = "diag_month", unit = "months", add.column = TRUE)
#'   ds.date(x="D$date_diagnosis", type = "extractdate",
#'           newobj = "diag_day", unit = "days", add.column = TRUE)
#' 
#' 
#'   # Example 1: Create a new object by extracting the year from an object
#'   ds.date(x="diagnosis_date", type = "extractdate",
#'           newobj = "diagnosis_year", unit = "years", add.column = FALSE)
#' 
#'   # Example 2: Create a new column by extracting year from an object. This will result in an error since
#'   # creating a new column option requires a dataframe input.
#'   ds.date(x="diagnosis_date", type = "extractdate",
#'           newobj = "diagnosis_year", unit = "years", add.column = TRUE)
#' 
#'   # Example 3: Create a new date column by combining 3 objects: 2 columns and 1 vector.
#'   ds.date(x=c("diagnosis_year", "D$diag_month", "D$diag_day"), type = "makedate",
#'           newobj = "combined_date", add.column = TRUE)
#' 
#'   # Example 4: Create a new object by calculating time between one column and one object in months.
#'   ds.date(x=c("diagnosis_date", "D$date_death"), type = "timebetween",
#'           newobj = "timebetween.months", unit = "months", add.column = FALSE)
#' 
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'  
#' @author Zulal Bekerecioglu
#' @export
#'



ds.date <- function(x=NULL, type=c("extractdate", "makedate", "timebetween"),
                    unit=c("days", "months", "years"), add.column=FALSE, 
                    newobj="date.result", datasources = NULL) {
  
  # look for DS connections
  if (is.null(datasources)) {
    datasources <- datashield.connections_find()
  }
  
  # ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }
  
  unit <- match.arg(unit)

  # check if parameter type is valid : in c("extractdate", "makedate", "timebetween") 
  if (!(type %in% c("extractdate", "makedate", "timebetween"))) {
    stop("Invalid value for 'type'. Must be one of: extractdate, makedate, timebetween.")
  }
  
  # if x is empty, throw an error
  if (is.null(x)) {
    stop("Argument 'x' cannot be NULL. Please provide an object name or column name(s).")
  }
  
  # If add.column is TRUE, then inputs must have at least one column, i.e. one element should have $. 
  # If there are multiple columns, then the df must be shared.
  if (add.column) {
    error_message <- "Input object not valid: when 'add.column' is TRUE, at least one element must be a column.
    If multiple columns are specified, they must all belong to the same dataframe."
    
    # Identify elements that contain a $
    has_dollar <- grepl("\\$", x, perl = TRUE)
    
    # At least one element must contain $
    if (!any(has_dollar)) stop(error_message, call. = FALSE)
    
    # Multiple $ elements: check that the prefix before $ is identical
    if (sum(has_dollar) > 1) {
      prefixes <- vapply(strsplit(x[has_dollar], "\\$", perl = TRUE),
                         function(x) x[[1]], FUN.VALUE = character(1))
      if (length(unique(prefixes)) != 1) stop(error_message, call. = FALSE)
      
      common_df <- unique(prefixes)
    } else {
      # Single $ element: ensure both prefix and suffix are non-empty
      parts <- strsplit(x[has_dollar], "\\$", perl = TRUE)[[1]]
      if (length(parts) != 2 || !nzchar(parts[1]) || !nzchar(parts[2])) stop(error_message, call. = FALSE)
      
      common_df <- parts[1]
    }
  }
  
  
  args <- c(
    sprintf('x = c(%s)', paste(sprintf('"%s"', x), collapse = ", ")),
    sprintf('type = "%s"', type),
    sprintf('newobj = "%s"', newobj),
    sprintf('add.column = "%s"', add.column),
    sprintf('unit = "%s"', unit)
  )
  
  
  cally <- paste0("dateDS(", paste(args, collapse = ", "), ")")
  
  # extractdate -------
  if (type == "extractdate") {
    if (length(unit) != 1 || !(unit %in% c("days", "months", "years"))) {
      stop("Invalid unit. Must be one of: days, months, years")
    }
    
    # get a column with a date, extract year, month, or day according to specificed unit
    # save as a new object
    if (!add.column) {
      if (newobj == "newdate") {
        newobj <- paste0(newobj, ".", unit)
      }
      DSI::datashield.assign(datasources, symbol = newobj, as.symbol(cally))
      message_text <- sprintf("Extracted %s and saved it as a new object named '%s'.",
                              unit, newobj)
      
      
    } else {  # save as a new column
      if (newobj == "newdate") {
        # add the unit after newdate for readability (newdate.months)
        newobj <- paste0(newobj, ".", unit)
      }
      DSI::datashield.assign(datasources, symbol = common_df, as.symbol(cally))
      message_text <- sprintf("Extracted %s from %s and added it as a new column named '%s'.",
                              unit, common_df, newobj)
      
    }
  }
  
  
  # makedate -------
  if (type == "makedate") {
    
    # get three columns in a list as 'year' 'month' 'day', combine them into one column
    # check if provided x is a list of length 3
    if (!(is.character(x) && length(x) == 3)) {
      stop("For 'makedate', x must be a character vector of length 3 (year, month, day).")
    }
    
    # save as a new object
    if (!add.column) {
      DSI::datashield.assign(datasources, symbol = newobj, as.symbol(cally))
      message_text <- sprintf("Created a combined date and saved it as a new object named '%s'.",
                              newobj)
      
      
    } else {  # save as a new column
      if (newobj == "newdate") {
        newobj <- paste0(newobj, ".", unit)
      }
      DSI::datashield.assign(datasources, symbol = common_df, as.symbol(cally))
      message_text <- sprintf("Created a combined date and added it as a new column in '%s' named '%s'.",
                              common_df, newobj)
      
    }
  
  }
  
  
  # timebetween --------
  if (type == "timebetween") {
    # get two columns as a list, get the unit of return unit, calculate the time between in that unit
    
    if (!(is.character(x) && length(x) == 2)) {
      stop("For 'timebetween', x must be a character vector of length 2 (start column, end column).")
    }
    
    # save as a new object
    if (!add.column) {
      DSI::datashield.assign(datasources, symbol = newobj, as.symbol(cally))
      message_text <- sprintf("Calculated the time difference and saved it as a new object named '%s'.",
                              newobj)
      
      
    } else {  # save as a new column
      if (newobj == "newdate") {
        newobj <- paste0(newobj, ".", unit) 
      }
      DSI::datashield.assign(datasources, symbol = common_df, as.symbol(cally))
      message_text <- sprintf("Calculated the time difference and added it as a new column in '%s' named '%s'.",
                              common_df, newobj)
      
    }
    
  }
  
  
  #############################################################################################################
  # Check that the object (or dataframe with new column) was successfully created on all servers
  #############################################################################################################
  
  test.obj.name <- if (!add.column) newobj else common_df
  
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
      calltext.names <- call("namesDS", common_df)
      df.colnames <- DSI::datashield.aggregate(datasources, calltext.names)
      
      col.exists.in.all.sources <- TRUE
      for (j in 1:length(df.colnames)) {
        if (!(newobj %in% df.colnames[[j]])) {
          col.exists.in.all.sources <- FALSE
        }
      }
      
      if (col.exists.in.all.sources) {
        validity.check <- paste0("New column <", newobj, "> successfully added to dataframe <", common_df, "> in all sources.")
      } else {
        validity.check <- paste0("Warning: column <", newobj, "> not found in dataframe <", common_df, "> in one or more sources. Check with ds.names().")
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
#ds.date