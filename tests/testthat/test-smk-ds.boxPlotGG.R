
#
# Set up
#

# context("ds.boxPlotGG::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.boxPlotGG::smk::simple")
test_that("simple boxPlotGG", {
    ds.boxPlotGG_data_Treatment(table="D", variables=c("LAB_TSC", "LAB_TRIG"))
    res <- ds.boxPlotGG(x="boxPlotRawData", type="pooled")

    expect_true(inherits(res, "ggplot"))
})

#
# Done
#

# context("ds.boxPlotGG::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "boxPlotRawData"))
})

disconnect.studies.dataset.cnsim()

# context("ds.boxPlotGG::smk::done")
