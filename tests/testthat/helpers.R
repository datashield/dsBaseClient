#' Create a DSLite login object that can be used for testing
#'
#' @param assign_method A string specifying the name of the custom assign method to be added
#'   to the DSLite server. If `NULL`, no additional assign method is added. Default is `NULL`.
#' @param aggregate_method A string specifying the name of the custom aggregate method to be
#'   added to the DSLite server. If `NULL`, no additional aggregate method is added. Default is `NULL`.
#' @param tables A named list of tables to be made available on the DSLite server. Default is `NULL`.
#'
#' @return A DataSHIELD login object containing the necessary connection information for the DSLite server.
#'
#' @examples
#' \dontrun{
#' # Prepare a DSLite server with default methods and custom assign/aggregate methods
#' login_data <- .prepare_dslite(
#'   assign_method = "customAssign",
#'   aggregate_method = "customAggregate",
#'   tables = list(mtcars = mtcars, mtcars_group = mtcars_group)
#'   )
#'
#' @importFrom DSLite newDSLiteServer
#' @importFrom DSI newDSLoginBuilder
#' @export
.prepare_dslite <- function(assign_method = NULL, aggregate_method = NULL, tables = NULL) {

  options(datashield.env = environment())
  dslite.server <- DSLite::newDSLiteServer(tables = tables)
  dslite.server$config(defaultDSConfiguration(include = c("dsBase", "dsTidyverse")))
  dslite.server$aggregateMethod("exists", "base::exists")
  dslite.server$aggregateMethod("classDS", "dsBase::classDS")
  dslite.server$aggregateMethod("lsDS", "dsBase::lsDS")
  dslite.server$aggregateMethod("dsListDisclosureSettings", "dsTidyverse::dsListDisclosureSettings")

  if (!is.null(assign_method)) {
    dslite.server$assignMethod(assign_method, paste0("dsTidyverse::", assign_method))
  }

  if (!is.null(aggregate_method)) {
    dslite.server$aggregateMethod(assign_method, paste0("dsTidyverse::", assign_method))
  }

  builder <- DSI::newDSLoginBuilder()
  builder$append(server = "server_1", url = "dslite.server", driver = "DSLiteDriver")
  builder$append(server = "server_2", url = "dslite.server", driver = "DSLiteDriver")
  builder$append(server = "server_3", url = "dslite.server", driver = "DSLiteDriver")
  login_data <- builder$build()
  return(login_data)
}

#' Create a mixed dataframe with factor and other types of columns
#'
#' This function generates a dataframe with a specified number of rows,
#' factor columns, and other columns (integer, numeric, and string).
#'
#' @param n_rows Number of rows in the dataframe. Default is 10,000.
#' @param n_factor_cols Number of factor columns in the dataframe. Default is 15.
#' @param n_other_cols Number of other columns (integer, numeric, and string) in the dataframe. Default is 15.
#'
#' @return A dataframe with the specified number of rows and columns, containing mixed data types.
#' @importFrom dplyr bind_cols
#' @importFrom purrr map_dfc
#' @examples
#' df <- create_mixed_dataframe(n_rows = 100, n_factor_cols = 10, n_other_cols = 5)
create_mixed_dataframe <- function(n_rows = 10000, n_factor_cols = 15, n_other_cols = 15) {

  # Function to create a factor column with defined levels
  create_factor_column <- function(levels, n = n_rows) {
    set.seed(123)  # Set seed before sample for reproducibility
    factor(sample(levels, n, replace = TRUE))
  }

  # Define factor levels for different columns
  factor_levels <- list(
    c("Low", "Medium", "High"),
    c("Red", "Green", "Blue"),
    c("Yes", "No"),
    c("A", "B", "C"),
    c("One", "Two", "Three"),
    c("Cat", "Dog", "Bird"),
    c("Small", "Medium", "Large"),
    c("Alpha", "Beta", "Gamma"),
    c("True", "False"),
    c("Left", "Right"),
    c("North", "South", "East", "West"),
    c("Day", "Night"),
    c("Up", "Down"),
    c("Male", "Female"),
    c("Summer", "Winter", "Spring", "Fall")
  )

  # Create factor columns
  factor_columns <- map_dfc(factor_levels[1:n_factor_cols], create_factor_column)
  colnames(factor_columns) <- paste0("fac_col", 1:n_factor_cols)

  # Function to create other types of columns
  create_other_column <- function(type, n = n_rows) {
    set.seed(123)  # Set seed before sample for reproducibility
    switch(type,
           "int" = sample(1:100, n, replace = TRUE),        # Integer column
           "num" = runif(n, 0, 100),                       # Numeric column
           "str" = sample(letters, n, replace = TRUE),      # Character column
           "log" = sample(c(TRUE, FALSE), n, replace = TRUE) # Logical column
    )
  }

  # Ensure that each data type is included
  column_types <- c(
    "int", "num", "str", "log", "int",
    "num", "str", "log", "int", "num",
    "str", "int", "num", "log", "str"
  )

  # Create other columns with specified types
  other_columns <- map_dfc(column_types[1:n_other_cols], create_other_column)
  colnames(other_columns) <- paste0("col", (n_factor_cols + 1):(n_factor_cols + n_other_cols))

  # Combine factor and other columns into a single dataframe
  df <- bind_cols(factor_columns, other_columns)

  return(df)
}


#' Modify factor levels for partial overlap
#'
#' This function takes two sets of factor levels, computes the common and unique levels,
#' and returns a new set of levels with partial overlap.
#'
#' @param levels1 First set of factor levels.
#' @param levels2 Second set of factor levels.
#'
#' @return A character vector of new factor levels with partial overlap.
#' @examples
#' new_levels <- partial_overlap_levels(c("A", "B", "C"), c("B", "C", "D"))
partial_overlap_levels <- function(levels1, levels2) {
  common <- intersect(levels1, levels2)
  unique1 <- setdiff(levels1, common)
  unique2 <- setdiff(levels2, common)

  # Set seed before each sample call
  set.seed(123)
  sampled_unique1 <- sample(unique1, length(unique1) * 0.5)

  set.seed(123)
  sampled_unique2 <- sample(unique2, length(unique2) * 0.5)

  new_levels <- c(common, sampled_unique1, sampled_unique2)
  return(new_levels)
}


#' Create additional dataframes with specific conditions
#'
#' This function generates additional dataframes based on an input dataframe, modifying column classes and levels,
#' and adding new columns with unique names. Different seeds are used for each iteration of the loop,
#' ensuring reproducibility of the generated dataframes.
#'
#' @param base_df The base dataframe used to create the additional dataframes.
#' @param n_rows Number of rows in the additional dataframes. Default is 10,000.
#' @param df_names Names of the additional dataframes to be created. Default is c("df1", "df2", "df3").
#'
#' @return A list of dataframes with the specified modifications.
#' @importFrom dplyr bind_cols
#' @examples
#' base_df <- create_mixed_dataframe(n_rows = 100, n_factor_cols = 10, n_other_cols = 5)
#' additional_dfs <- create_additional_dataframes(base_df, n_rows = 1000, df_names = c("df1", "df2"))
create_additional_dataframes <- function(base_df, n_rows = 10000, df_names = c("df1", "df2", "df3")) {

  # Define a fixed sequence of seeds, one for each dataframe to be created
  seeds <- c(123, 456, 789, 101112)

  df_list <- list()

  for (i in seq_along(df_names)) {
    # Set the seed for this iteration based on the pre-defined seeds
    set.seed(seeds[i])

    # Proceed with the dataframe generation process
    overlap_cols <- sample(colnames(base_df), size = round(0.8 * ncol(base_df)))
    df <- base_df
    cols_to_modify_class <- sample(overlap_cols, size = round(0.2 * length(overlap_cols)))

    # Modify columns to have different data types
    for (col in cols_to_modify_class) {
      current_class <- class(df[[col]])
      new_class <- switch(current_class,
                          "factor" = as.character(df[[col]]),
                          "character" = as.factor(df[[col]]),
                          "numeric" = as.integer(df[[col]]),
                          "integer" = as.numeric(df[[col]]),
                          df[[col]])
      df[[col]] <- new_class
    }

    # Modify factor levels for partial overlap
    factor_cols <- colnames(base_df)[sapply(base_df, is.factor)]
    overlap_factor_cols <- intersect(overlap_cols, factor_cols)
    cols_to_modify_levels <- sample(overlap_factor_cols, size = round(0.5 * length(overlap_factor_cols)))

    for (col in cols_to_modify_levels) {
      original_levels <- levels(base_df[[col]])
      new_levels <- partial_overlap_levels(original_levels, original_levels)
      df[[col]] <- factor(df[[col]], levels = new_levels)
    }

    # Create new random columns for each dataframe (these will vary by seed)
    set.seed(seeds[i])  # Set the seed again for generating new columns
    n_new_cols <- round(0.2 * ncol(base_df))
    new_col_names <- paste0(df_names[i], "_new_col_", 1:n_new_cols)
    new_cols <- data.frame(matrix(runif(n_rows * n_new_cols), ncol = n_new_cols))
    colnames(new_cols) <- new_col_names

    # Bind new columns to the dataframe
    df <- bind_cols(df, new_cols)
    df_list[[df_names[i]]] <- df
  }

  return(df_list)
}
