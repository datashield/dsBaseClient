
#
# Set up
#

# context("ds.rep::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

# context("ds.rep::perf::0")
test_that("performance", {
    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.rep(x1=5, times=10, newobj="rep.newobj")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.rep::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.rep::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.rep::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.rep::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.rep::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.rep::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.rep::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.rep::perf::shutdown")
disconnect.studies.dataset.cnsim()
# context("ds.rep::perf::done")
