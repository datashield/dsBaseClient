
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
    expect_error(ds.contourPlot(x="nonexistent_obj", y="D$LAB_TRIG", method="deterministic"), "There are some DataSHIELD errors", fixed=TRUE)

    res.errors <- DSI::datashield.errors()

    expect_match(res.errors$sim1, "The server-side object 'nonexistent_obj' does not exist", fixed = TRUE)
})

test_that("contour plot with wrong input class fails", {
    expect_error(ds.contourPlot(x="D", y="D$LAB_TRIG", method="deterministic"), "There are some DataSHIELD errors", fixed=TRUE)

    res.errors <- DSI::datashield.errors()

    expect_match(res.errors$sim1, "must be of type numeric or integer", fixed = TRUE)
})

#
# Done
#

test_that("contour plot with nonexistent object fails in the default combine path", {
    expect_error(ds.contourPlot(x="nonexistent_obj", y="D$LAB_TRIG"), "There are some DataSHIELD errors", fixed=TRUE)

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_obj' does not exist", fixed = TRUE)
    }
})

# context("ds.contourPlot::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.contourPlot::smk::done")
