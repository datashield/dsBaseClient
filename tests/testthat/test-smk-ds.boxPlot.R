
#
# Set up
#

# context("ds.boxPlot::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.boxPlot::smk::simple")
test_that("simple boxPlot with data frame", {
    res <- ds.boxPlot(x="D", variables=c("LAB_TSC", "LAB_TRIG"), type="pooled")

    expect_true(inherits(res, "ggplot") || inherits(res, "gtable"))
})

test_that("boxPlot with nonexistent object fails", {
    expect_error(ds.boxPlot(x="nonexistent_obj", variables=c("LAB_TSC")), "There are some DataSHIELD errors", fixed=TRUE)
})

#
# Done
#

# context("ds.boxPlot::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "boxPlotRawData"))
})

disconnect.studies.dataset.cnsim()

# context("ds.boxPlot::smk::done")
