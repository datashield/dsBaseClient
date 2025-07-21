#' Fill DataFrame with Missing Columns and Adjust Classes
#'
#' This function fills a given DataFrame by adding missing columns, ensuring consistent column classes, and adjusting factor levels where necessary.
#' It performs checks to detect class and factor level conflicts and prompts the user for decisions to resolve these conflicts.
#'
#' @param df.name Name of the input DataFrame to fill.
#' @param newobj Name of the new DataFrame object created after filling.
#' @param fix_class Character, determines behaviour if class of variables is not the same in all
#' studies. Option "ask" (default) provides the user with a prompt asking if they want to set the
#' class across all studies, option "no" will throw an error if class conflicts are present.
#' @param fix_levels Character, determines behaviour if levels of factor variables is not the same
#' in all studies. Option "ask" (default) provides the user with a prompt asking if they want to set
#' the levels of factor variables to be the same across all studies, whilst option "no" will throw
#' an error if factor variables do not have the same class.
#' @param datasources Data sources from which to aggregate data. Default is `NULL`.
#' @importFrom assertthat assert_that
#' @importFrom DSI datashield.aggregate datashield.assign
#' @return The filled DataFrame with added columns and adjusted classes or factor levels.
#' @export
ds.standardiseDf <- function(df.name = NULL, newobj = NULL, fix_class = "ask", fix_levels = "ask",
                             datasources = NULL) {
  fill_warnings <- list()

  .check_arguments(df.name, newobj, fix_class, fix_levels)

  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }

  col_names <- datashield.aggregate(datasources, call("colnamesDS", df.name))
  .stop_if_cols_identical(col_names)

  var_classes <- .get_var_classes(df.name, datasources)
  class_conflicts <- .identify_class_conflicts(var_classes)

  datashield.assign(datasources, newobj, as.symbol(df.name))

  if (length(class_conflicts) > 0 & fix_class == "no") {
    DSI::datashield.aggregate(datasources, call("rmDS", newobj))
    cli_abort("Variables do not have the same class in all studies and `fix_class` is 'no'")
  } else if (length(class_conflicts) > 0 & fix_class == "ask") {
    class_decisions <- prompt_user_class_decision_all_vars(
      names(class_conflicts),
      var_classes$server,
      dplyr::select(var_classes, all_of(names(class_conflicts))),
      newobj,
      datasources
    )

    withCallingHandlers({
      .fix_classes(newobj, names(class_conflicts), class_decisions, newobj, datasources)
    }, warning = function(w) {
      fill_warnings <<- c(fill_warnings, conditionMessage(w))  # Append warning to the list
      invokeRestart("muffleWarning")  # Suppress immediate display of the warning
    })
  }

  unique_cols <- .get_unique_cols(col_names)
  .add_missing_cols_to_df(newobj, unique_cols, newobj, datasources)
  new_names <- datashield.aggregate(datasources, call("colnamesDS", newobj))
  added_cols <- .get_added_cols(col_names, new_names)

  new_classes <- .get_var_classes(newobj, datasources)
  factor_vars <- .identify_factor_vars(new_classes)
  factor_levels <- .get_factor_levels(factor_vars, newobj, datasources)
  level_conflicts <- .identify_level_conflicts(factor_levels)

  if (length(level_conflicts) > 0 & fix_levels == "no") {
    DSI::datashield.aggregate(datasources, call("rmDS", newobj))
    cli_abort("Factor variables do not have the same levels in all studies and `fix_levels` is 'no'")
  } else if (length(level_conflicts) > 0 & fix_levels == "ask") {
    levels_decision <- ask_question_wait_response_levels(level_conflicts, newobj, datasources)
  }

  if (levels_decision == "1") {
    unique_levels <- .get_unique_levels(factor_levels, level_conflicts)
    .set_factor_levels(newobj, unique_levels, datasources)
  }

  .print_out_messages(added_cols, class_decisions, names(class_conflicts), unique_levels,
                      level_conflicts, levels_decision, newobj)

  .handle_warnings(fill_warnings)
  .print_class_warning(class_conflicts, fix_class, class_decisions)
}

#' Check Function Arguments for Validity
#'
#' This function validates the arguments provided to ensure they meet specified conditions.
#' It checks that the `fix_class` and `fix_levels` arguments are set to accepted values
#' and that `df.name` and `newobj` are character strings.
#'
#' @param df.name A character string representing the name of the data frame.
#' @param newobj A character string representing the name of the new object to be created.
#' @param fix_class A character string indicating the method for handling class issues.
#'   Must be either `"ask"` or `"no"`.
#' @param fix_levels A character string indicating the method for handling level issues.
#'   Must be either `"ask"` or `"no"`.
#' @return NULL. This function is used for validation and does not return a value.
#' @importFrom assertthat assert_that
#' @noRd
.check_arguments <- function(df.name, newobj, fix_class, fix_levels) {
  assert_that(fix_class %in% c("ask", "no"))
  assert_that(fix_levels %in% c("ask", "no"))
  assert_that(is.character(df.name))
  assert_that(is.character(newobj))
}

#' Stop If Columns Are Identical
#'
#' Checks if the columns in the data frames are identical and throws an error if they are.
#'
#' @param col_names A list of column names from different data sources.
#' @return None. Throws an error if columns are identical.
#' @importFrom cli cli_abort
#' @noRd
.stop_if_cols_identical <- function(col_names) {
  are_identical <- all(sapply(col_names, identical, col_names[[1]]))
  if (are_identical) {
    cli_abort("Columns are identical in all data frames: nothing to fill")
  }
}

#' Get Variable Classes from DataFrame
#'
#' Retrieves the class of each variable in the specified DataFrame from different data sources.
#'
#' @param df.name Name of the input DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @return A DataFrame containing the variable classes from each data source.
#' @import dplyr
#' @noRd
.get_var_classes <- function(df.name, datasources) {
  cally <- call("getClassAllColsDS", df.name)
  classes <- datashield.aggregate(datasources, cally) %>%
    bind_rows(.id = "server")
  return(classes)
}

#' Identify Class Conflicts
#'
#' Identifies conflicts in variable classes across different data sources.
#'
#' @param classes A DataFrame containing variable classes across data sources.
#' @return A list of variables that have class conflicts.
#' @import dplyr
#' @importFrom purrr map
#' @noRd
.identify_class_conflicts <- function(classes) {
  server <- NULL
  different_class <- classes |>
    dplyr::select(-server) |>
    map(~ unique(na.omit(.)))

  out <- different_class[which(different_class %>% map(length) > 1)]
  return(out)
}

#' Prompt User for Class Decision for All Variables
#'
#' Prompts the user to resolve class conflicts for all variables.
#'
#' @param vars A vector of variable names with class conflicts.
#' @param all_servers The names of all servers.
#' @param all_classes The classes of the variables across servers.
#' @return A vector of decisions for each variable's class.
#' @noRd
prompt_user_class_decision_all_vars <- function(vars, all_servers, all_classes, newobj, datasources) {
  decisions <- c()
  for (i in 1:length(vars)) {
    decisions[i] <- prompt_user_class_decision(vars[i], all_servers, all_classes[[i]], newobj, datasources)
  }
  return(decisions)
}

#' Prompt User for Class Decision for a Single Variable
#'
#' Prompts the user to resolve a class conflict for a single variable.
#'
#' @param var The variable name with a class conflict.
#' @param all_servers The names of all servers.
#' @param all_classes The classes of the variable across servers.
#' @importFrom cli cli_alert_warning cli_alert_danger
#' @return A decision for the variable's class.
#' @noRd
prompt_user_class_decision <- function(var, servers, classes, newobj, datasources) {
  cli_alert_warning("`ds.dataFrameFill` requires that all columns have the same class.")
  cli_alert_danger("Column {.strong {var}} has following classes:")
  print_all_classes(servers, classes)
  cli_text("")
  return(ask_question_wait_response_class(var, newobj, datasources))
}

#' Print All Server-Class Pairs
#'
#' This function prints out a list of server names along with their corresponding
#' class types. It formats the output with a bullet-point list using the `cli` package.
#'
#' @param all_servers A character vector containing the names of servers.
#' @param all_classes A character vector containing the class types corresponding
#' to each server.
#' @return This function does not return a value. It prints the server-class pairs
#' to the console as a bulleted list.
#' @importFrom cli cli_ul cli_li cli_end
#' @noRd
print_all_classes <- function(all_servers, all_classes) {
  combined <- paste(all_servers, all_classes, sep = ": ")
  cli_ul()
  for (i in 1:length(combined)) {
    cli_li("{combined[i]}")
  }
  cli_end()
}

#' Ask Question and Wait for Class Response
#'
#' Prompts the user with a question and waits for a response related to class decisions.
#'
#' @param question The question to ask the user.
#' @return The user's decision.
#' @importFrom cli cli_text cli_alert_warning cli_abort
#' @noRd
ask_question_wait_response_class <- function(var, newobj, datasources) {
  readline <- NULL
  ask_question_class(var)
  answer <- readline()
  if (answer == "6") {
    DSI::datashield.aggregate(datasources, call("rmDS", newobj))
    cli_abort("Aborted `ds.dataFrameFill`", .call = NULL)
  } else if (!answer %in% as.character(1:5)) {
    cli_text("")
    cli_alert_warning("Invalid input. Please try again.")
    cli_text("")
    ask_question_wait_response_class(var, newobj, datasources)
  } else {
    return(answer)
  }
}

#' Prompt User for Class Conversion Options
#'
#' This function prompts the user with options to convert a variable to a specific class (e.g., factor, integer, numeric, character, or logical).
#' The function provides a list of class conversion options for the specified variable and includes an option to cancel the operation.
#'
#' @param var The name of the variable for which the user is prompted to select a class conversion option.
#'
#' @importFrom cli cli_alert_info cli_ol
#' @return None. This function is used for prompting the user and does not return a value.
#' @examples
#' ask_question("variable_name")
#' @noRd
ask_question_class <- function(var) {
  cli_alert_info("Would you like to:")
  class_options <- c("a factor", "an integer", "numeric", "a character", "a logical vector")
  class_message <- paste0("Convert `{var}` to ", class_options, " in all studies")
  cli_ol(
    c(class_message, "Cancel `ds.dataFrameFill` operation")
  )
}

#' Fix Variable Classes
#'
#' Applies the user's class decisions to fix the classes of variables across different data sources.
#'
#' @param df.name The name of the DataFrame.
#' @param different_classes A list of variables with class conflicts.
#' @param class_decisions The decisions made by the user.
#' @param newobj The name of the new DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @return None. Updates the DataFrame with consistent variable classes.
#' @noRd
.fix_classes <- function(df.name, different_classes, class_decisions, newobj, datasources) {
  cally <- call("fixClassDS", df.name, different_classes, class_decisions)
  datashield.assign(datasources, newobj, cally)
}

#' Get Unique Columns from Data Sources
#'
#' Retrieves all unique columns from the data sources.
#'
#' @param col_names A list of column names.
#' @return A vector of unique column names.
#' @noRd
.get_unique_cols <- function(col_names) {
  return(
    unique(
      unlist(col_names)
    )
  )
}

#' Add Missing Columns to DataFrame
#'
#' Adds any missing columns to the DataFrame to ensure all columns are present across data sources.
#'
#' @param df.name The name of the DataFrame.
#' @param unique_cols A vector of unique column names.
#' @param newobj The name of the new DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @return None. Updates the DataFrame with added columns.
#' @noRd
.add_missing_cols_to_df <- function(df.name, cols_to_add_if_missing, newobj, datasources) {
  cally <- call("fixColsDS", df.name, cols_to_add_if_missing)
  datashield.assign(datasources, newobj, cally)
}

#' Get Added Columns
#'
#' Compares the old and new column names and identifies newly added columns.
#'
#' @param old_names A list of old column names.
#' @param new_names A list of new column names.
#' @importFrom purrr pmap
#' @return A list of added column names.
#' @noRd
.get_added_cols <- function(old_names, new_names) {
  list(old_names, new_names) %>%
    pmap(function(.x, .y) {
      .y[!.y %in% .x]
    })
}

#' Identify Factor Variables
#'
#' Identifies which variables are factors in the DataFrame.
#'
#' @param var_classes A DataFrame containing variable classes.
#' @return A vector of factor variables.
#' @noRd
.identify_factor_vars <- function(var_classes) {
  return(
    var_classes %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::select(where(~ . == "factor"))
  )
}

#' Get Factor Levels from Data Sources
#'
#' Retrieves the levels of factor variables from different data sources.
#'
#' @param factor_vars A vector of factor variables.
#' @param newobj The name of the new DataFrame.
#' @param datasources Data sources from which to aggregate data.
#' @return A list of factor levels.
#' @noRd
.get_factor_levels <- function(factor_vars, df, datasources) {
  cally <- call("getAllLevelsDS", df, names(factor_vars))
  return(datashield.aggregate(datasources, cally))
}

#' Identify Factor Level Conflicts
#'
#' Identifies conflicts in factor levels across different data sources.
#'
#' @param factor_levels A list of factor levels.
#' @return A list of variables with level conflicts.
#' @importFrom purrr map_lgl pmap_lgl
#' @noRd
.identify_level_conflicts <- function(factor_levels) {
  levels <- factor_levels %>%
    pmap_lgl(function(...) {
      args <- list(...)
      !all(map_lgl(args[-1], ~ identical(.x, args[[1]])))
    })

  return(names(levels[levels == TRUE]))
}

#' Ask Question and Wait for Response on Factor Levels
#'
#' Prompts the user with options for resolving factor level conflicts and waits for a response.
#'
#' @param level_conflicts A list of variables with factor level conflicts.
#' @return The user's decision.
#' @noRd
ask_question_wait_response_levels <- function(level_conflicts, newobj, datasources) {
  .make_levels_message(level_conflicts)
  answer <- readline()
  if (answer == "3") {
    DSI::datashield.aggregate(datasources, call("rmDS", newobj))
    cli_abort("Aborted `ds.dataFrameFill`", .call = NULL)
  } else if (!answer %in% as.character(1:2)) {
    cli_alert_warning("Invalid input. Please try again.")
    cli_alert_info("")
    .make_levels_message(level_conflicts)
    return(ask_question_wait_response_levels(level_conflicts, newobj, datasources))
  } else {
    return(answer)
  }
}

#' Make Factor Level Conflict Message
#'
#' Creates a message to alert the user about factor level conflicts and prompt for action.
#'
#' @param level_conflicts A list of variables with factor level conflicts.
#' @importFrom cli cli_alert_warning cli_alert_info cli_ol
#' @return None. Prints the message to the console.
#' @noRd
.make_levels_message <- function(level_conflicts) {
  cli_alert_warning("Warning: factor variables {level_conflicts} do not have the same levels in all studies")
  cli_alert_info("Would you like to:")
  cli_ol(c("Create the missing levels where they are not present", "Do nothing", "Cancel `ds.dataFrameFill` operation"))
}

#' Get Unique Factor Levels
#'
#' Retrieves the unique factor levels for variables with conflicts.
#'
#' @param factor_levels A list of factor levels.
#' @param level_conflicts A list of variables with level conflicts.
#' @importFrom purrr pmap
#' @return A list of unique factor levels.
#' @noRd
.get_unique_levels <- function(factor_levels, level_conflicts) {
  unique_levels <- factor_levels %>%
    map(~ .[level_conflicts]) %>%
    pmap(function(...) {
      as.character(c(...))
    }) %>%
    map(~ unique(.))
  return(unique_levels)
}

#' Set Factor Levels in DataFrame
#'
#' Applies the unique factor levels to the DataFrame.
#'
#' @param newobj The name of the new DataFrame.
#' @param unique_levels A list of unique factor levels.
#' @param datasources Data sources from which to aggregate data.
#' @return None. Updates the DataFrame with the new factor levels.
#' @noRd
.set_factor_levels <- function(df, unique_levels, datasources) {
  cally <- call("fixLevelsDS", df, names(unique_levels), unique_levels)
  datashield.assign(datasources, df, cally)
}

#' Print Out Summary Messages
#'
#' Prints summary messages regarding the filled DataFrame, including added columns, class decisions, and factor level adjustments.
#'
#' @param added_cols A list of added columns.
#' @param class_decisions A vector of class decisions.
#' @param different_classes A list of variables with class conflicts.
#' @param unique_levels A list of unique factor levels.
#' @param level_conflicts A list of variables with level conflicts.
#' @param levels_decision The decision made regarding factor levels.
#' @param newobj The name of the new DataFrame.
#' @importFrom cli cli_text
#' @return None. Prints messages to the console.
#' @noRd
.print_out_messages <- function(added_cols, class_decisions, different_classes, unique_levels,
                                level_conflicts, levels_decision, newobj) {
  .print_var_recode_message(added_cols, newobj)

  if (length(different_classes) > 0) {
    .print_class_recode_message(class_decisions, different_classes, newobj)
    cli_text("")
  }

  if (length(level_conflicts) > 0 & levels_decision == "1") {
    .print_levels_recode_message(unique_levels, newobj)
  }
}

#' Print Variable Recode Message
#'
#' Prints a message summarizing the columns that were added to the DataFrame.
#'
#' @param added_cols A list of added columns.
#' @param newobj The name of the new DataFrame.
#' @importFrom cli cli_text
#' @return None. Prints the message to the console.
#' @noRd
.print_var_recode_message <- function(added_cols, newobj) {
  cli_alert_success("The following variables have been added to {newobj}:")
  added_cols_neat <- added_cols %>% map(~ ifelse(length(.) == 0, "", .))
  var_message <- paste0(names(added_cols), " --> ", added_cols_neat)
  for (i in 1:length(var_message)) {
    cli_alert_info("{var_message[[i]]}")
  }
  cli_text("")
}

#' Print Class Recode Message
#'
#' Prints a message summarizing the class decisions that were made for variables with conflicts.
#'
#' @param class_decisions A vector of class decisions.
#' @param different_classes A list of variables with class conflicts.
#' @param newobj The name of the new DataFrame.
#' @importFrom cli cli_alert_info cli_alert_success
#' @return None. Prints the message to the console.
#' @noRd
.print_class_recode_message <- function(class_decisions, different_classes, newobj) {
  choice_neat <- .change_choice_to_string(class_decisions)
  class_message <- paste0(different_classes, " --> ", choice_neat)
  cli_alert_success("The following classes have been set for all datasources in {newobj}: ")
  for (i in 1:length(class_message)) {
    cli_alert_info("{class_message[[i]]}")
  }
}

#' Convert Class Decision Code to String
#'
#' This function converts a numeric class decision input (represented as a string)
#' into the corresponding class type string (e.g., "factor", "integer", "numeric", etc.).
#' @param class_decision A string representing the class decision. It should be
#' one of the following values: "1", "2", "3", "4", or "5".
#' @return A string representing the class type corresponding to the input:
#' "factor", "integer", "numeric", "character", or "logical".
#' @noRd
.change_choice_to_string <- function(class_decision) {
  case_when(
    class_decision == "1" ~ "factor",
    class_decision == "2" ~ "integer",
    class_decision == "3" ~ "numeric",
    class_decision == "4" ~ "character",
    class_decision == "5" ~ "logical"
  )
}

#' Print Factor Levels Recode Message
#'
#' Prints a message summarizing the factor level decisions that were made for variables with conflicts.
#'
#' @param unique_levels A list of unique factor levels.
#' @param newobj The name of the new DataFrame.
#' @importFrom cli cli_alert_success cli_alert_info
#' @return None. Prints the message to the console.
#' @noRd
.print_levels_recode_message <- function(unique_levels, newobj) {
  levels_message <- .make_levels_recode_message(unique_levels)
  cli_alert_success("The following levels have been set for all datasources in {newobj}: ")
  for (i in 1:length(levels_message)) {
    cli_alert_info("{levels_message[[i]]}")
  }
}

#' Make Levels Recode Message
#'
#' Creates a message to alert the user about factor level recoding.
#'
#' @param unique_levels A list of unique factor levels.
#' @return A formatted string summarizing the level recoding.
#' @importFrom purrr pmap
#' @noRd
.make_levels_recode_message <- function(unique_levels) {
  return(
    list(names(unique_levels), unique_levels) %>%
      pmap(function(.x, .y) {
        paste0(.x, " --> ", paste0(.y, collapse = ", "))
      })
  )
}

#' Handle Warnings for Class Conversion Issues
#'
#' This function iterates through a list of warnings generated during class conversion and
#' triggers a danger alert if any warnings indicate that the conversion has resulted in `NA` values.
#'
#' @param fill_warnings A list or vector of warning messages generated during class conversion.
#'   If any warnings indicate that `NA` values were introduced, a danger alert will be displayed.
#' @return NULL. This function is used for its side effects of printing alerts.
#' @importFrom cli cli_alert_danger
#' @importFrom stringr str_detect
#' @noRd
.handle_warnings <- function(fill_warnings) {
  if(length(fill_warnings) > 0) {
    for(i in 1:length(fill_warnings)) {
      if(str_detect(fill_warnings[[i]], "NAs introduced by coercion")){
        cli_alert_danger("Class conversion resulted in the creation of NA values.")
      } else {
        cli_alert_danger(fill_warnings[[i]])
      }
    }
  }
}

#' Print Warning for Class Conflicts in Data Conversion
#'
#' This function displays a warning when there are class conflicts in a dataset that may have resulted
#' from incompatible class changes during data conversion. It alerts users to verify column classes,
#' as incompatible changes could corrupt the data.
#'
#' @param class_conflicts A list or vector of conflicting classes identified during conversion.
#' @param fix_class A string indicating the user's choice for fixing class conflicts. Typically,
#'   this is "ask" if the user is prompted to confirm class changes.
#' @param class_decisions A vector of decisions made for class conversions. When any value is not
#'   "6", it indicates unresolved class conflicts.
#' @return NULL. This function is used for its side effects of printing alerts.
#' @importFrom cli cli_alert_warning
#' @noRd
.print_class_warning <- function(class_conflicts, fix_class, class_decisions) {
  if(length(class_conflicts) > 0 & fix_class == "ask" & all(!class_decisions == "6")) {
    cli_alert_warning("Please check all columns that have changed class. Not all class changes
    are compatible with all data types, so this could have corrupted the data.")
  }
}

readline <- NULL
