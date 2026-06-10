#'
#' @title Display missing data patterns with disclosure control
#' @description This function is a client-side wrapper for the server-side mdPatternDS
#' function. It generates a missing data pattern matrix similar to mice::md.pattern but
#' with disclosure control applied to prevent revealing small cell counts.
#' @details The function calls the server-side mdPatternDS function which uses
#' mice::md.pattern to analyze missing data patterns. Patterns with counts below the 
#' disclosure threshold (default: nfilter.tab = 3) are suppressed to maintain privacy.
#'
#' \strong{Output Format:}
#' - Each row represents a missing data pattern
#' - Pattern counts are shown in row names (e.g., "150", "25")
#' - Columns show 1 if the variable is observed, 0 if missing
#' - Last column shows the total number of missing values per pattern
#' - Last row shows the total number of missing values per variable
#'
#' \strong{Disclosure Control:}
#'
#' Suppressed patterns (count below threshold) are indicated by:
#' - Row name: "suppressed(<N>)" where N is the threshold
#' - All pattern values set to NA
#' - Summary row also suppressed to prevent back-calculation
#'
#' \strong{Pooling Behavior (type='combine'):}
#'
#' When pooling across studies, the function uses a \emph{conservative approach} 
#' for disclosure control:
#'
#' 1. Identifies identical missing patterns across studies
#' 2. \strong{EXCLUDES suppressed patterns from pooling} - patterns suppressed in 
#'    ANY study are not included in the pooled count
#' 3. Sums counts only for non-suppressed identical patterns
#' 4. Re-validates pooled counts against disclosure threshold
#'
#' \strong{Important:} This conservative approach means:
#' - Pooled counts may be \emph{underestimates} if some studies had suppressed patterns
#' - This prevents disclosure through subtraction (e.g., if study A shows count=5 
#'   and pool shows count=7, one could deduce study B has count=2, violating disclosure)
#' - Different patterns across studies are preserved separately in the pooled result
#'
#' @param x a character string specifying the name of a data frame or matrix on the 
#' server-side containing the data to analyze.
#' @param type a character string specifying the output type. If 'split' (default), 
#' returns separate patterns for each study. If 'combine', attempts to pool patterns 
#' across studies.
#' @param datasources a list of \code{\link[DSI]{DSConnection-class}} objects obtained 
#' after login. If the \code{datasources} argument is not specified, the default set of 
#' connections will be used: see \code{\link[DSI]{datashield.connections_default}}.
#' @return For type='split': A list with one element per study, each containing:
#' \describe{
#'   \item{pattern}{The missing data pattern matrix for that study}
#'   \item{valid}{Logical indicating if all patterns meet disclosure requirements}
#'   \item{message}{A message describing the validity status}
#' }
#'
#' For type='combine': A list containing:
#' \describe{
#'   \item{pattern}{The pooled missing data pattern matrix across all studies}
#'   \item{valid}{Logical indicating if all pooled patterns meet disclosure requirements}
#'   \item{message}{A message describing the validity status}
#' }
#' @author Xavier Escribà montagut for DataSHIELD Development Team
#' @export
#' @examples
#' \dontrun{
#'  ## Version 6, for version 5 see the Wiki
#'
#'   # Connecting to the Opal servers
#'
#'   require('DSI')
#'   require('DSOpal')
#'   require('dsBaseClient')
#'
#'   builder <- DSI::newDSLoginBuilder()
#'   builder$append(server = "study1",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "CNSIM.CNSIM1", driver = "OpalDriver")
#'   builder$append(server = "study2",
#'                  url = "http://192.168.56.100:8080/",
#'                  user = "administrator", password = "datashield_test&",
#'                  table = "CNSIM.CNSIM2", driver = "OpalDriver")
#'   logindata <- builder$build()
#'
#'   connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#'   # Get missing data patterns for each study separately
#'   patterns_split <- ds.mdPattern(x = "D", type = "split", datasources = connections)
#'
#'   # View results for study1
#'   print(patterns_split$study1$pattern)
#'   #      var1 var2 var3
#'   # 150    1    1    1  0    <- 150 obs complete
#'   #  25    0    1    1  1    <- 25 obs missing var1
#'   #       25    0    0 25    <- Summary: 25 missing per variable
#'
#'   # Get pooled missing data patterns across studies
#'   patterns_pooled <- ds.mdPattern(x = "D", type = "combine", datasources = connections)
#'   print(patterns_pooled$pattern)
#'
#'   # Example with suppressed patterns:
#'   # If study1 has a pattern with count=2 (suppressed) and study2 has same pattern 
#'   # with count=5 (valid), the pooled result will show count=5 (conservative approach)
#'   # A warning will indicate: "Pooled counts may underestimate the true total"
#'
#'   # Clear the Datashield R sessions and logout
#'   datashield.logout(connections)
#' }
#'
ds.mdPattern <- function(x = NULL, type = 'split', datasources = NULL){

  # Look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  # Ensure datasources is a list of DSConnection-class
  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  if(is.null(x)){
    stop("Please provide the name of a data frame or matrix!", call.=FALSE)
  }

  # Get study names
  study_names <- names(datasources)

  # Call the server side function
  cally <- call("mdPatternDS", x)
  results <- DSI::datashield.aggregate(datasources, cally)

  # Process results based on type
  if(type == "split"){
    # Return individual study results
    return(results)

  } else if(type == "combine"){
    # Pool results across studies

    # First check if any study has invalid patterns
    any_invalid <- any(sapply(results, function(r) !r$valid))
    invalid_studies <- names(results)[sapply(results, function(r) !r$valid)]

    if(any_invalid){
      warning(
        "Disclosure control: Some studies have suppressed patterns (below threshold).\n",
        "  Studies with suppressed patterns: ", paste(invalid_studies, collapse=", "), "\n",
        "  These patterns are EXCLUDED from pooling to prevent disclosure.\n",
        "  Pooled counts may underestimate the true total.",
        call. = FALSE
      )
    }

    # Extract patterns from each study
    patterns_list <- lapply(results, function(r) r$pattern)

    # Check if all patterns have the same variables (columns)
    n_vars <- sapply(patterns_list, ncol)
    if(length(unique(n_vars)) > 1){
      stop("Cannot pool patterns: studies have different numbers of variables", call.=FALSE)
    }

    var_names <- colnames(patterns_list[[1]])
    if(length(patterns_list) > 1){
      for(i in 2:length(patterns_list)){
        if(!identical(colnames(patterns_list[[i]]), var_names)){
          warning("Variable names differ across studies. Pooling by position.")
          break
        }
      }
    }

    # Pool the patterns
    pooled_pattern <- .pool_md_patterns(patterns_list, study_names)

    # Check validity of pooled results
    # Get threshold from first study's results or use a default check
    nfilter.tab <- getOption("default.nfilter.tab")
    if(is.null(nfilter.tab)) nfilter.tab <- 3

    n_patterns <- nrow(pooled_pattern) - 1
    pooled_valid <- TRUE

    if(n_patterns > 0){
      # Pattern counts are in row names
      pattern_counts <- as.numeric(rownames(pooled_pattern)[1:n_patterns])
      pattern_counts <- pattern_counts[!is.na(pattern_counts) & pattern_counts > 0]

      if(any(pattern_counts < nfilter.tab)){
        pooled_valid <- FALSE
      }
    }

    pooled_message <- ifelse(pooled_valid,
                             "Valid: all pooled pattern counts meet disclosure requirements",
                             "Some pooled pattern counts may be below threshold")

    return(list(
      pattern = pooled_pattern,
      valid = pooled_valid,
      message = pooled_message,
      studies = study_names
    ))

  } else {
    stop("Argument 'type' must be either 'split' or 'combine'", call.=FALSE)
  }
}

#' @title Pool missing data patterns across studies
#' @description Internal function to pool md.pattern results from multiple studies
#' @param patterns_list List of pattern matrices from each study
#' @param study_names Names of the studies
#' @return Pooled pattern matrix
#' @keywords internal
.pool_md_patterns <- function(patterns_list, study_names){

  # Initialize with first study's pattern structure
  pooled <- patterns_list[[1]]
  n_vars <- ncol(pooled)
  n_rows <- nrow(pooled) - 1  # Exclude summary row

  # Create a list to store unique patterns
  unique_patterns <- list()
  pattern_counts <- list()

  # Process each study
  for(i in seq_along(patterns_list)){
    pattern <- patterns_list[[i]]
    study_n_patterns <- nrow(pattern) - 1

    if(study_n_patterns > 0){
      for(j in 1:study_n_patterns){
        # Get pattern (columns show 1/0 for observed/missing)
        pat_vector <- pattern[j, 1:(n_vars-1)]
        # Pattern count is in row name
        pat_count_str <- rownames(pattern)[j]
        pat_count <- suppressWarnings(as.numeric(pat_count_str))

        # Skip if suppressed (non-numeric row name like "suppressed(<3)")
        if(is.na(pat_count)){
          next
        }

        # Convert pattern to string for comparison
        pat_string <- paste(pat_vector, collapse="_")

        # Check if this pattern already exists
        if(pat_string %in% names(unique_patterns)){
          # Add to existing count
          pattern_counts[[pat_string]] <- pattern_counts[[pat_string]] + pat_count
        } else {
          # New pattern
          unique_patterns[[pat_string]] <- pat_vector
          pattern_counts[[pat_string]] <- pat_count
        }
      }
    }
  }

  # Build pooled pattern matrix
  if(length(unique_patterns) == 0){
    # No valid patterns
    pooled[1:n_rows, ] <- NA
  } else {
    # Sort patterns by count (descending)
    sorted_idx <- order(unlist(pattern_counts), decreasing = TRUE)
    sorted_patterns <- unique_patterns[sorted_idx]
    sorted_counts <- pattern_counts[sorted_idx]

    # Create new pooled matrix
    n_pooled_patterns <- length(sorted_patterns)
    pooled <- matrix(NA, nrow = n_pooled_patterns + 1, ncol = n_vars)
    colnames(pooled) <- colnames(patterns_list[[1]])

    # Set row names (counts for patterns, empty for summary)
    row_names <- c(as.character(unlist(sorted_counts)), "")
    rownames(pooled) <- row_names

    # Fill in patterns
    for(i in 1:n_pooled_patterns){
      pooled[i, 1:(n_vars-1)] <- sorted_patterns[[i]]
      # Calculate number of missing for this pattern
      pooled[i, n_vars] <- sum(sorted_patterns[[i]] == 0)
    }
  }

  # Calculate summary row (total missing per variable)
  # Sum across studies
  summary_row <- rep(0, n_vars)
  for(i in seq_along(patterns_list)){
    study_summary <- patterns_list[[i]][nrow(patterns_list[[i]]), ]
    # Only add if not suppressed
    if(!all(is.na(study_summary))){
      summary_row <- summary_row + ifelse(is.na(study_summary), 0, study_summary)
    }
  }

  # Add summary row
  pooled[nrow(pooled), ] <- summary_row

  return(pooled)
}

