
#
# Set up
#

# context("ds.auc::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TRIG", "DIS_DIAB"))
ds.glmSLMA('D$DIS_DIAB~D$LAB_TRIG', family = "binomial", newobj = "auc.glm")
ds.glmPredict("auc.glm", output.type = "link", se.fit = FALSE, newobj = "auc.pred")

#
# Tests
#

# context("ds.auc::perf::0")
test_that("performance", {
    .durationSec  <- perf.testduration()
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.auc(pred = "auc.pred$fit", y = "auc.glm$y")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.auc::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.auc::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.auc::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.auc::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.auc::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.auc::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.auc::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.auc::perf::shutdown")
disconnect.studies.dataset.cnsim()
# context("ds.auc::perf::done")
