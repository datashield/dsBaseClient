
#
# Set up
#

# context("ds.heatmapPlot::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.heatmapPlot::smk::simple")
test_that("simple heatmap plot combine", {
    expect_no_error(ds.heatmapPlot(x="D$LAB_TSC", y="D$LAB_TRIG", type="combine"))
})

test_that("heatmap plot with nonexistent object fails", {
    expect_error(ds.heatmapPlot(x="nonexistent_obj", y="D$LAB_TRIG"), "There are some DataSHIELD errors", fixed=TRUE)
})

#
# Done
#

# context("ds.heatmapPlot::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.heatmapPlot::smk::done")
