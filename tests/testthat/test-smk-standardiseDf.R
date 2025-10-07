#
# Set up
#
context("ds.standardiseDf::smk::setup")
options(datashield.errors.print = TRUE)

connect.studies.dataset.stand(
  c(
    "fac_col1", "fac_col2", "fac_col3", "fac_col4", "fac_col5", "fac_col6", "fac_col7", "fac_col9",
    "fac_col10", "col11", "col12", "col13", "col14", "col15", "col16", "col17", "col18", "col19",
    "col20")
  )

test_that("setup", {
  ds_expect_variables(c("D"))
})

#
# Tests
#

####################################################################################################
# Code that will be used in multiple tests
####################################################################################################
var_class <- .get_var_classes("D", datasources = ds.test_env$connections)

class_conflicts <- .identify_class_conflicts(var_class)

different_classes <- c("fac_col4", "fac_col5")

class_decisions <- c("1", "5")

.fix_classes(
  df.name = "D",
  different_classes = different_classes,
  class_decisions = class_decisions,
  newobj = "new_classes",
  datasources = ds.test_env$connections)

cols_to_set <- c(
  "fac_col1", "fac_col2", "fac_col3", "fac_col4", "fac_col5", "fac_col6", "fac_col9", "col12",
  "col15", "col18", "fac_col7", "fac_col10", "col13", "col16", "col19", "col11", "col14", "col17",
  "col20")

.add_missing_cols_to_df(
  df.name = "D",
  cols_to_add_if_missing = cols_to_set,
  newobj = "with_new_cols",
  datasources = ds.test_env$connections)

old_cols <- ds.colnames("D")

new_cols <- c("col11", "col12", "col13", "col14", "col15", "col16", "col17", "col18", "col19",
              "col20", "fac_col1", "fac_col10", "fac_col2", "fac_col3", "fac_col4", "fac_col5",
              "fac_col6", "fac_col7", "fac_col9")

new_cols_servers <- list(
  server_1 = new_cols,
  server_2 = new_cols,
  server_3 = new_cols
)

added_cols <- .get_added_cols(old_cols, new_cols_servers)

var_class_fact <- .get_var_classes("with_new_cols", datasources = ds.test_env$connections)

fac_vars <- .identify_factor_vars(var_class_fact)

fac_levels <- .get_factor_levels("with_new_cols", fac_vars, ds.test_env$connections)

level_conflicts <- .identify_level_conflicts(fac_levels)

unique_levs <- .get_unique_levels(fac_levels, level_conflicts)

####################################################################################################
# Tests
####################################################################################################
test_that(".stop_if_cols_identical throws error if columns are identical", {

  identical_cols <- list(
    c("col1", "col2", "col3"),
    c("col1", "col2", "col3"),
    c("col1", "col2", "col3")
  )

  expect_error(
    .stop_if_cols_identical(identical_cols),
    "Columns are identical in all data frames: nothing to fill"
  )

})

test_that(".stop_if_cols_identical doesn't throw error if data frames have different columns", {

  different_cols <- list(
    c("col1", "col2", "col3"),
    c("col1", "col2", "col4"),
    c("col1", "col5", "col3")
  )

  expect_silent(
    .stop_if_cols_identical(different_cols)
  )

})

test_that(".get_var_classes returns correct output", {

  expected <- tibble(
    server = c("sim1", "sim2", "sim3"),
    fac_col1 = c("factor", "factor", "factor"),
    fac_col2 = c("factor", "factor", "factor"),
    fac_col3 = c("factor", "factor", "factor"),
    fac_col4 = c("numeric", "character", "factor"),
    fac_col5 = c("logical", "integer", "factor"),
    fac_col6 = c("factor", NA, NA),
    fac_col9 = c("factor", NA, NA),
    col12 = c("numeric", NA, NA),
    col15 = c("integer", NA, NA),
    col18 = c("logical", NA, NA),
    fac_col7 = c(NA, "factor", NA),
    fac_col10 = c(NA, "factor", NA),
    col13 = c(NA, "character", NA),
    col16 = c(NA, "numeric", NA),
    col19 = c(NA, "integer", NA),
    col11 = c(NA, NA, "integer"),
    col14 = c(NA, NA, "logical"),
    col17 = c(NA, NA, "character"),
    col20 = c(NA, NA, "numeric")
  )

  expect_equal(var_class, expected)

})

test_that(".identify_class_conflicts returns correct output", {
  expected <- list(
    fac_col4 = c("numeric", "character", "factor"),
    fac_col5 = c("logical", "integer", "factor")
  )

  expect_equal(class_conflicts, expected)

})

test_that("ask_question displays the correct prompt", {
  expect_snapshot(ask_question_class("my_var"))
})

test_that("ask_question_wait_response_class continues with valid response", {
  expect_equal(
    with_mocked_bindings(
      ask_question_wait_response_class("a variable"),
      ask_question_class = function(var) "A question",
      readline = function() "1"
    ), "1"
  )
})

test_that("ask_question_wait_response_class throws error if option 6 selected", {
  expect_error(
    with_mocked_bindings(
      ask_question_wait_response_class("a variable"),
      ask_question_class = function(var) "A question",
      readline = function() "6")
  )
})

test_that("print_all_classes prints the correct message", {
  expect_snapshot(
    print_all_classes(
      c("server_1", "server_2", "server_3"),
      c("numeric", "factor", "integer")
    )
  )
})

test_that("prompt_user_class_decision function properly", {
  expect_message(
    with_mocked_bindings(
      prompt_user_class_decision(
        var = "test_col",
        servers = c("sim2", "sim2", "sim3"),
        classes = c("numeric", "character", "factor"),
        newobj = "test_df",
        datasources = datasources),
      ask_question_wait_response_class = function(var, newobj, datasources) "test_col"
    )
  )

  expect_equal(
    with_mocked_bindings(
      prompt_user_class_decision(
        var = "test_col",
        servers = c("sim2", "sim2", "sim3"),
        classes = c("numeric", "character", "factor"),
        newobj = "test_df",
        datasources = datasources),
      ask_question_wait_response_class = function(var, newobj, datasources) "test_col"
    ),
    "test_col"
  )
})

test_that("prompt_user_class_decision_all_vars returns correct value", {
  expect_equal(
    with_mocked_bindings(
      prompt_user_class_decision_all_vars(
        vars = c("test_var_1", "test_var_2"),
        all_servers = c("sim2", "sim2", "sim3"),
        all_classes = tibble(
          test_var_1 = c("numeric", "character", "factor"),
          test_var_2 = c("logical", "integer", "factor")
        ),
        "test_df",
        conns),
      prompt_user_class_decision = function(var, server, classes, newobj, datasources) "1"
    ),
    c("1", "1")
  )
})

test_that(".fix_classes sets the correct classes in serverside data frame", {

  expect_equal(
    unname(unlist(ds.class("D$fac_col4"))),
    c("numeric", "character", "factor")
  )

  expect_equal(
    unname(unlist(ds.class("D$fac_col5"))),
    c("logical", "integer", "factor")
  )

  expect_equal(
    unname(unlist(ds.class("new_classes$fac_col4"))),
    rep("factor", 3)
  )

  expect_equal(
    unname(unlist(ds.class("new_classes$fac_col5"))),
    rep("logical", 3)
  )

})

test_that(".get_unique_cols extracts unique names from a list", {
  expect_equal(
    .get_unique_cols(
      list(
        server_1 = c("col_1", "col_2", "col_3"),
        server_1 = c("col_1", "col_2", "col_4"),
        server_1 = c("col_2", "col_3", "col_3", "col_5")
      )
    ),
    c("col_1", "col_2", "col_3", "col_4", "col_5")
  )
})

test_that(".add_missing_cols_to_df correctly creates missing columns", {

  new_cols <- c("col11", "col12", "col13", "col14", "col15", "col16", "col17", "col18", "col19",
                "col20", "fac_col1", "fac_col10", "fac_col2", "fac_col3", "fac_col4", "fac_col5",
                "fac_col6", "fac_col7", "fac_col9")

  observed <- ds.colnames("with_new_cols")

  expected <- list(
    sim1 = new_cols,
    sim2 = new_cols,
    sim3 = new_cols
  )

  expect_equal(observed, expected)
})

test_that(".get_added_cols correctly identifies newly added columns", {

  expect_equal(
    added_cols,
    list(
      sim1 = c("col11", "col13", "col14", "col16", "col17", "col19", "col20", "fac_col10", "fac_col7"),
      sim2 = c("col11", "col12", "col14", "col15", "col17", "col18", "col20", "fac_col6", "fac_col9"),
      sim3 = c("col12", "col13", "col15", "col16", "col18", "col19", "fac_col10", "fac_col6", "fac_col7", "fac_col9")
    )
  )
})

test_that(".identify_factor_vars correctly identifies factor variables", {



  var_class_fact <- var_class |> dplyr::select(server: col18)
  expect_equal(
    names(fac_vars),
    c("fac_col1", "fac_col2", "fac_col3", "fac_col6", "fac_col9")
  )
})

test_that(".get_factor_levels correctly identifies factor levels", {
  expected <- list(
    sim1 = list(
      fac_col1 = c("High", "Low", "Medium"),
      fac_col2 = c("Blue", "Green"),
      fac_col3 = c("No", "Yes"),
      fac_col6 = c("Bird", "Cat", "Dog"),
      fac_col9 = c("False", "True")
    ),
    sim2 = list(
      fac_col1 = c("High", "Low", "Medium"),
      fac_col2 = c("Green", "Red"),
      fac_col3 = c("No"),
      fac_col6 = NULL,
      fac_col9 = NULL
    ),
    sim3 = list(
      fac_col1 = c("High", "Low", "Medium"),
      fac_col2 = c("Blue"),
      fac_col3 = c("Yes"),
      fac_col6 = NULL,
      fac_col9 = NULL
    )
  )

  expect_equal(fac_levels, expected)
})

test_that(".identify_level_conflicts correctly factor columns with different levels", {
  expect_equal(
    .identify_level_conflicts(fac_levels),
    c("fac_col2", "fac_col3", "fac_col6", "fac_col9")
  )

})

test_that("ask_question_wait_response_levels continues with valid response", {
  expect_equal(
    with_mocked_bindings(
      suppressWarnings(ask_question_wait_response_levels("test variable", "test_obj", conns)),
      readline = function() "1"
    ), "1"
  )

  expect_equal(
    with_mocked_bindings(
      suppressWarnings(ask_question_wait_response_levels("test variable", "test_obj", conns)),
      readline = function() "1"
    ), "1"
  )

})

test_that("ask_question_wait_response_levels aborts with response of 3", {
  expect_error(
    with_mocked_bindings(
      suppressWarnings(ask_question_wait_response_levels("test variable", "test_obj", conns)),
      readline = function() "3")
  )
})

test_that(".make_levels_message makes correct message", {
  expect_snapshot(.make_levels_message(level_conflicts))
})

test_that(".get_unique_levels extracts all possible levels", {

  expected <- list(
    fac_col2 = c("Blue", "Green", "Red"),
    fac_col3 = c("No", "Yes"),
    fac_col6 = c("Bird", "Cat", "Dog"),
    fac_col9 = c("False", "True")
  )

  expect_equal(unique_levs, expected)

})

test_that(".set_factor_levels sets levels correctly", {
  .set_factor_levels("with_new_cols", unique_levs,  ds.test_env$connections)

  expect_equal(
    ds.levels("with_new_cols$fac_col2") |> map(~.x[[1]]),
    list(
      sim1 = c("Blue", "Green", "Red"),
      sim2 = c("Blue", "Green", "Red"),
      sim3 = c("Blue", "Green", "Red")
    )
  )

  expect_equal(
    ds.levels("with_new_cols$fac_col3") |> map(~.x[[1]]),
    list(
      sim1 = c("No", "Yes"),
      sim2 = c("No", "Yes"),
      sim3 = c("No", "Yes")
    )
  )

  expect_equal(
    ds.levels("with_new_cols$fac_col6") |> map(~.x[[1]]),
    list(
      sim1 = c("Bird", "Cat", "Dog"),
      sim2 = c("Bird", "Cat", "Dog"),
      sim3 = c("Bird", "Cat", "Dog")
    )
  )

  expect_equal(
    ds.levels("with_new_cols$fac_col9") |> map(~.x[[1]]),
    list(
      sim1 = c("False", "True"),
      sim2 = c("False", "True"),
      sim3 = c("False", "True")
    )
  )

})

test_that(".print_var_recode_message prints the correct message", {
  expect_snapshot(.print_var_recode_message(added_cols, "test_df"))
})

test_that(".print_class_recode_message prints the correct message", {
  expect_snapshot(
    .print_class_recode_message(class_decisions, different_classes, "test_df")
  )
})

test_that(".print_levels_recode_message prints the correct message", {
  expect_snapshot(
    .print_levels_recode_message(unique_levs, "test_df")
  )
})

test_that(".make_levels_recode_message prints the correct message", {
  expect_equal(
    .make_levels_recode_message(unique_levs),
    list(
      "fac_col2 --> Blue, Green, Red",
      "fac_col3 --> No, Yes",
      "fac_col6 --> Bird, Cat, Dog",
      "fac_col9 --> False, True"
    )
  )
})

test_that(".print_out_messages prints the correct messages", {
  expect_snapshot(
    .print_out_messages(
      added_cols, class_decisions, different_classes, unique_levs, level_conflicts, "1", "test_df"
    )
  )
})

test_that(".change_choice_to_string converts numeric class codes to strings correctly", {
  expect_equal(.change_choice_to_string("1"), "factor")
  expect_equal(.change_choice_to_string("2"), "integer")
  expect_equal(.change_choice_to_string("3"), "numeric")
  expect_equal(.change_choice_to_string("4"), "character")
  expect_equal(.change_choice_to_string("5"), "logical")
})

test_that("ds.standardiseDf doesn't run if dataframes are identical", {
  with_mocked_bindings(
    ds.standardiseDf(
      df = "D",
      newobj = "test_fill"
      ),
    prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) "1",
    ask_question_wait_response_levels = function(levels_conflict, newobj, datasources) "2"
    )

  expect_error(
    ds.standardiseDf(
      df = "test_fill",
      newobj = "shouldn't_exist"),
    "Columns are identical"
    )
  })

test_that("ds.standardiseDf works when called directly and class conversion is factor", {
  with_mocked_bindings(
    ds.standardiseDf(
      df = "D",
      newobj = "test_fill"
    ),
    prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) "1",
    ask_question_wait_response_levels = function(levels_conflict, newobj, datasources) "2"
  )

  expect_equal(
    ds.class("test_fill$fac_col4")[[1]],
    "factor"
  )
})

test_that("ds.standardiseDf returns warning when called directly and class conversion is integer", {
  with_mocked_bindings(
    ds.standardiseDf(
      df = "D",
      newobj = "test_fill"
    ),
    prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) c("2", "2"),
    ask_question_wait_response_levels = function(levels_conflict, newobj, datasources) "2"
  )

  expect_equal(
    ds.class("test_fill$fac_col4")[[1]],
    "integer"
  )

  expect_equal(
    ds.class("test_fill$fac_col5")[[1]],
    "integer"
  )
})

test_that("ds.standardiseDf returns warning when called directly and class conversion is numeric", {
  with_mocked_bindings(
    ds.standardiseDf(
      df = "D",
      newobj = "test_fill"
    ),
    prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) c("3", "3"),
    ask_question_wait_response_levels = function(levels_conflict, newobj, datasources) "2"
  )

  expect_equal(
    ds.class("test_fill$fac_col4")[[1]],
    "numeric"
  )

  expect_equal(
    ds.class("test_fill$fac_col5")[[1]],
    "numeric"
  )
})

test_that("ds.standardiseDf returns warning when called directly and class conversion is character", {
  with_mocked_bindings(
    ds.standardiseDf(
      df = "D",
      newobj = "test_fill"
    ),
    prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) c("4", "4"),
    ask_question_wait_response_levels = function(levels_conflict, newobj, datasources) "2"
  )

  expect_equal(
    ds.class("test_fill$fac_col4")[[1]],
    "character"
  )

  expect_equal(
    ds.class("test_fill$fac_col5")[[1]],
    "character"
  )
})

test_that("ds.standardiseDf returns warning when called directly and class conversion is logical", {
  with_mocked_bindings(
    ds.standardiseDf(
      df = "D",
      newobj = "test_fill"
    ),
    prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) c("5", "5"),
    ask_question_wait_response_levels = function(levels_conflict, newobj, datasources) "2"
  )

  expect_equal(
    ds.class("test_fill$fac_col4")[[1]],
    "logical"
  )

  expect_equal(
    ds.class("test_fill$fac_col5")[[1]],
    "logical"
  )
})

test_that("ds.standardiseDf changes levels if this option is selected", {
  with_mocked_bindings(
    ds.standardiseDf(
      df = "D",
      newobj = "test_fill"
    ),
    prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) c("1", "1"),
    ask_question_wait_response_levels = function(levels_conflict, newobj, datasources) "1"
  )

  levels_2 <- ds.levels("test_fill$fac_col2") %>% map(~.$Levels)
  levels_3 <- ds.levels("test_fill$fac_col3") %>% map(~.$Levels)
  levels_4 <- ds.levels("test_fill$fac_col4") %>% map(~.$Levels)
  levels_5 <- ds.levels("test_fill$fac_col5") %>% map(~.$Levels)
  levels_6 <- ds.levels("test_fill$fac_col6") %>% map(~.$Levels)
  levels_9 <- ds.levels("test_fill$fac_col9") %>% map(~.$Levels)

  expect_equal(
    levels_2,
    list(
      sim1 = c("Blue", "Green", "Red"),
      sim2 = c("Blue", "Green", "Red"),
      sim3 = c("Blue", "Green", "Red")
    )
  )

  expect_equal(
    levels_3,
    list(
      sim1 = c("No", "Yes"),
      sim2 = c("No", "Yes"),
      sim3 = c("No", "Yes")
    )
  )

  expect_equal(
    levels_4,
    list(
      sim1 = c("1", "2", "3", "A", "B", "C"),
      sim2 = c("1", "2", "3", "A", "B", "C"),
      sim3 = c("1", "2", "3", "A", "B", "C")
    )
  )

  expect_equal(
    levels_5,
    list(
      sim1 = c("1", "2", "3", "One", "Three", "Two"),
      sim2 = c("1", "2", "3", "One", "Three", "Two"),
      sim3 = c("1", "2", "3", "One", "Three", "Two")
    )
  )

  expect_equal(
    levels_6,
    list(
      sim1 = c("Bird", "Cat", "Dog"),
      sim2 = c("Bird", "Cat", "Dog"),
      sim3 = c("Bird", "Cat", "Dog")
    )
  )

  expect_equal(
    levels_9,
    list(
      sim1 = c("False", "True"),
      sim2 = c("False", "True"),
      sim3 = c("False", "True")
    )
  )

})

test_that("ds.standardiseDf doesn't run if classes are not identical and fix_class is no", {
  expect_error(
    ds.standardiseDf(
      df = "D",
      newobj = "shouldnt_exist",
      fix_class = "no"
    ),
    "Variables do not have the same class in all studies"
  )

  expect_equal(
    ds.exists("shouldnt_exist")[[1]],
    FALSE
  )
})

test_that("ds.standardiseDf doesn't run if levels are not identical and fix_class is no", {
  expect_error(
    with_mocked_bindings(
      ds.standardiseDf(
        df = "D",
        newobj = "shouldnt_exist",
        fix_levels = "no"
      ),
      prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) c("1", "1")
    ),
    "Factor variables do not have the same levels in all studies"
  )

  expect_equal(
    ds.exists("shouldnt_exist")[[1]],
    FALSE
  )
})


## Add disclosure check levels
## Push change to dsBase





