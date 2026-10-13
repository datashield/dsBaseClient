
#
# Set up
#

# context("ds.igb_standards::smk::setup")

connect.studies.dataset.anthro(list("age", "sex", "weight", "height", "muac"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

ds.make("(D$age*0)+280", "gagebrth")
ds.recodeValues(var.name = "D$sex", values2replace.vector = c(1, 2), new.values.vector = c("Male", "Female"), newobj = "sex.label")

#
# Tests
#

# context("ds.igb_standards::smk")
test_that("simple igb_standards", {
    ds.igb_standards(gagebrth = "gagebrth", z = 0, p = 50, var = "lencm", sex = "sex.label", fun = "igb_centile2value", newobj = "igb.out")

    res.class <- ds.class("igb.out")
    for (study.class in res.class) {
        expect_equal(study.class, "numeric")
    }
})

test_that("fails if an input does not exist", {
    expect_error(
        ds.igb_standards(gagebrth = "nonexistent_object", z = 0, p = 50, var = "lencm", sex = "sex.label", fun = "igb_centile2value"),
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

# context("ds.igb_standards::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "gagebrth", "sex.label", "igb.out"))
})

disconnect.studies.dataset.anthro()

# context("ds.igb_standards::smk::done")
