
#
# Set up
#

# context("ds.bp_standards::smk::setup")

connect.studies.dataset.anthro(list("age", "sex", "weight", "height", "muac"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

ds.make("D$age/12", "age.years") # nolint: nonportable_path_linter. DataSHIELD expression, not a file path
ds.make("D$muac*7", "bp.value")

#
# Tests
#

# context("ds.bp_standards::smk")
test_that("simple bp_standards", {
    ds.bp_standards(sex = "D$sex", age = "age.years", height = "D$height", bp = "bp.value", systolic = TRUE, newobj = "bp.out")

    res.class <- ds.class("bp.out")
    for (study.class in res.class) {
        expect_equal(study.class, "list")
    }
})

test_that("fails if an input does not exist", {
    expect_error(
        ds.bp_standards(sex = "D$sex", age = "age.years", height = "D$height", bp = "nonexistent_object", systolic = TRUE),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

#
# Done
#

# context("ds.bp_standards::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "age.years", "bp.value", "bp.out"))
})

disconnect.studies.dataset.anthro()

# context("ds.bp_standards::smk::done")
