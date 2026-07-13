#
# Set up
#

# context("ds.standardiseDf::perf::setup")
connect.studies.dataset.stand(
  c(
    "fac_col1", "fac_col2", "fac_col3", "fac_col4", "fac_col5", "fac_col6", "fac_col7", "fac_col9",
    "fac_col10", "col11", "col12", "col13", "col14", "col15", "col16", "col17", "col18", "col19",
    "col20")
  )

#
# Tests
#

# context("ds.standardiseDf::perf::0")
test_that("performance", {
    .durationSec  <- 60 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        with_mocked_bindings(
            ds.standardiseDf(df.name = "D", newobj = "std.newobj"),
            prompt_user_class_decision_all_vars = function(var, server, classes, newobj, datasources) "1",
            ask_question_wait_response_levels = function(levels_conflict, newobj, datasources) "1"
        )

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.standardiseDf::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.standardiseDf::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.standardiseDf::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.standardiseDf::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.standardiseDf::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.standardiseDf::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.standardiseDf::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.standardiseDf::perf::shutdown")
disconnect.studies.dataset.stand()
# context("ds.standardiseDf::perf::done")
