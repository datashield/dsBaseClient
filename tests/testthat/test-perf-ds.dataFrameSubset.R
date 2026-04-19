
#
# Set up
#

# context("ds.dataFrameSubset::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL", "PM_BMI_CATEGORICAL"))

#
# Tests
#

# context("ds.dataFrameSubset::perf::0")
test_that("performance", {
    .durationSec  <- 60 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.dataFrameSubset(df.name="D", V1.name="D$LAB_TSC", V2.name="D$LAB_TRIG", Boolean.operator=">=", newobj="subset.newobj")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.dataFrameSubset::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.dataFrameSubset::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.dataFrameSubset::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.dataFrameSubset::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.dataFrameSubset::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.dataFrameSubset::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.dataFrameSubset::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.dataFrameSubset::perf::shutdown")
disconnect.studies.dataset.cnsim()
# context("ds.dataFrameSubset::perf::done")
