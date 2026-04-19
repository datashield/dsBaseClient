
#
# Set up
#

# context("ds.matrixDiag::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.matrixDiag::perf::0")
test_that("performance", {
    ds.matrix(mdata=0, nrows.scalar=3, ncols.scalar=3, newobj="matrix.newobj")
    .durationSec  <- 60 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.matrixDiag(x1="matrix.newobj", aim="serverside.matrix.2.vector", newobj="diag.newobj")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.matrixDiag::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.matrixDiag::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.matrixDiag::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.matrixDiag::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.matrixDiag::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.matrixDiag::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.matrixDiag::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.matrixDiag::perf::shutdown")
disconnect.studies.dataset.cnsim()
# context("ds.matrixDiag::perf::done")
