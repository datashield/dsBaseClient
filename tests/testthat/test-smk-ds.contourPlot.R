
#
# Set up
#

# context("ds.contourPlot::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.contourPlot::smk::simple")
test_that("simple contour plot combine", {
    expect_no_error(ds.contourPlot(x="D$LAB_TSC", y="D$LAB_TRIG", type="combine"))
})

test_that("contour plot with nonexistent object fails", {
    expect_error(ds.contourPlot(x="nonexistent_obj", y="D$LAB_TRIG"), "There are some DataSHIELD errors", fixed=TRUE)
})

#
# Done
#

# context("ds.contourPlot::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.contourPlot::smk::done")
