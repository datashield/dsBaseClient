
#
# Set up
#

# context("ds.bp_standards::perf::setup")
connect.studies.dataset.anthro(list("age", "sex", "weight", "height", "muac"))
ds.make("D$age/12", "age.years") # nolint: nonportable_path_linter. DataSHIELD expression, not a file path
ds.make("D$muac*7", "bp.value")

#
# Tests
#

# context("ds.bp_standards::perf::0")
test_that("performance", {
    .durationSec  <- perf.testduration()
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.bp_standards(sex = "D$sex", age = "age.years", height = "D$height", bp = "bp.value", systolic = TRUE, newobj = "bp.out")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.bp_standards::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.bp_standards::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.bp_standards::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.bp_standards::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.bp_standards::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.bp_standards::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.bp_standards::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.bp_standards::perf::shutdown")
disconnect.studies.dataset.anthro()
# context("ds.bp_standards::perf::done")
