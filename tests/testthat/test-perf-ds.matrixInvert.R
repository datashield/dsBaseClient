
#
# Set up
#

# context("ds.matrixInvert::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.matrixInvert::perf::0")
test_that("performance", {
    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.matrixInvert(M1="matrix.newobj", newobj="inv.newobj")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.matrixInvert::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.matrixInvert::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.matrixInvert::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.matrixInvert::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.matrixInvert::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.matrixInvert::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.matrixInvert::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.matrixInvert::perf::shutdown")
disconnect.studies.dataset.cnsim()
# context("ds.matrixInvert::perf::done")
