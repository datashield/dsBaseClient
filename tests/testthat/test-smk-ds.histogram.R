
#
# Set up
#

# context("ds.histogram::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.histogram::smk::simple")
test_that("simple histogram split", {
    res <- ds.histogram(x="D$LAB_TSC", type="split")

    expect_true(is.list(res))
    expect_length(res, 3)
})

test_that("simple histogram combine", {
    res <- ds.histogram(x="D$LAB_TSC", type="combine")

    expect_true(inherits(res, "histogram"))
})

test_that("histogram with deterministic method", {
    res <- ds.histogram(x="D$LAB_TSC", type="split", method="deterministic", k=3)

    expect_true(is.list(res))
})

test_that("histogram with nonexistent object fails", {
    expect_error(ds.histogram(x="nonexistent_obj"), "There are some DataSHIELD errors", fixed=TRUE)

    res.errors <- DSI::datashield.errors()

    expect_match(res.errors$sim1, "The server-side object 'nonexistent_obj' does not exist", fixed = TRUE)
})

test_that("histogram with wrong input class fails", {
    expect_error(ds.histogram(x="D"), "There are some DataSHIELD errors", fixed=TRUE)

    res.errors <- DSI::datashield.errors()

    expect_match(res.errors$sim1, "must be of type numeric or integer", fixed = TRUE)
})

#
# Done
#

# context("ds.histogram::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.histogram::smk::done")
