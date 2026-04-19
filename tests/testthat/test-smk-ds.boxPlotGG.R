
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
    ds.dataFrame(x=c("D$LAB_TSC", "D$LAB_TRIG"), newobj="boxplot_df")
    ds_expect_variables(c("D", "boxplot_df"))
})

#
# Done
#

# context("ds.boxPlotGG::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "boxplot_df"))
})

disconnect.studies.dataset.cnsim()

# context("ds.boxPlotGG::smk::done")
