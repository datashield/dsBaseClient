
#
# Set up
#

# context("ds.table::perf::setup")

connect.all.datasets()

ds.asFactor(input.var.name="D$FACTOR_CHARACTER", newobj.name="factorCharacter")
ds.asFactor(input.var.name="D$FACTOR_INTEGER", newobj.name="factorInteger")
ds.asFactor(input.var.name="D$CATEGORY", newobj.name="factorCategory")
ds.dataFrame(x=c("factorInteger", "factorCharacter", "factorCategory"), newobj="tablesource")
ds.dataFrameSubset(df.name="tablesource", V1.name="factorInteger", V2.name='6', Boolean.operator="!=", newobj="tablesource_subset")

#
# Tests
#

# context("ds.table::perf::0")
test_that("performance", {
    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.table(rvar='tablesource_subset$factorCharacter')

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.table::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.table::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.table::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.table::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.table::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.table::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.table::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.table::perf::shutdown")
disconnect.all.datasets()
# context("ds.table::perf::done")
