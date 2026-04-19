
#
# Set up
#

# context("ds.densityGrid::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.densityGrid::smk::simple")
test_that("simple density grid combine", {
    res <- ds.densityGrid(x="D$LAB_TSC", y="D$LAB_TRIG", type="combine")

    expect_true(is.matrix(res))
})

test_that("simple density grid split", {
    res <- ds.densityGrid(x="D$LAB_TSC", y="D$LAB_TRIG", type="split")

    expect_true(is.list(res))
    expect_length(res, 3)
})

test_that("density grid with nonexistent object fails", {
    expect_error(ds.densityGrid(x="nonexistent_obj", y="D$LAB_TRIG"), "There are some DataSHIELD errors", fixed=TRUE)
})

#
# Done
#

# context("ds.densityGrid::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.densityGrid::smk::done")
