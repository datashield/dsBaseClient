
#
# Set up
#

# context("ds.scatterPlot::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.scatterPlot::smk::simple")
test_that("simple scatter plot deterministic", {
    res <- ds.scatterPlot(x="D$LAB_TSC", y="D$LAB_TRIG", method="deterministic", type="split")

    expect_true(is.character(res))
})

test_that("simple scatter plot with coords", {
    res <- ds.scatterPlot(x="D$LAB_TSC", y="D$LAB_TRIG", method="deterministic", type="combine", return.coords=TRUE)

    expect_true(is.list(res))
})

test_that("scatter plot with nonexistent object fails", {
    expect_error(ds.scatterPlot(x="nonexistent_obj", y="D$LAB_TRIG"), "There are some DataSHIELD errors", fixed=TRUE)
})

#
# Done
#

# context("ds.scatterPlot::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.scatterPlot::smk::done")
