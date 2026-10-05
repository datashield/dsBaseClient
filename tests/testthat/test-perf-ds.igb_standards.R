
#
# Set up
#

# context("ds.igb_standards::perf::setup")
connect.studies.dataset.anthro(list("age", "sex", "weight", "height", "muac"))
ds.make("(D$age*0)+280", "gagebrth")
ds.recodeValues(var.name = "D$sex", values2replace.vector = c(1, 2), new.values.vector = c("Male", "Female"), newobj = "sex.label")

#
# Tests
#

# context("ds.igb_standards::perf::0")
test_that("performance", {
    .durationSec  <- perf.testduration()
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.igb_standards(gagebrth = "gagebrth", z = 0, p = 50, var = "lencm", sex = "sex.label", fun = "igb_centile2value", newobj = "igb.out")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.igb_standards::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.igb_standards::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.igb_standards::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.igb_standards::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.igb_standards::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.igb_standards::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.igb_standards::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.igb_standards::perf::shutdown")
disconnect.studies.dataset.anthro()
# context("ds.igb_standards::perf::done")
