
#
# Set up
#

# context("ds.mdPattern::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.mdPattern::smk::split")
test_that("simple mdPattern, split", {
    res <- ds.mdPattern(x = "D", type = "split")

    expect_length(res, 3)
    for (study.res in res) {
        expect_named(study.res, c("pattern", "valid", "message"))
        expect_true(is.matrix(study.res$pattern))
    }
})

# context("ds.mdPattern::smk::combine")
test_that("simple mdPattern, combine", {
    warnings <- character()
    res <- withCallingHandlers(
        ds.mdPattern(x = "D", type = "combine"),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )

    expect_named(res, c("pattern", "valid", "message", "studies"))
    expect_true(any(grepl("Disclosure control", warnings)))
    expect_true(is.matrix(res$pattern))
})

test_that("fails if the object does not exist", {
    expect_error(
        ds.mdPattern(x = "nonexistent_object"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

test_that("fails if the object is not a data.frame or matrix", {
    expect_error(
        ds.mdPattern(x = "D$LAB_TSC"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "must be of type data.frame or matrix")
    }
})

test_that("class consistency check aborts when classes differ across studies", {
    ds.asDataMatrix(x.name = "D", newobj = "mixed.class", datasources = ds.test_env$connections[1])
    ds.assign(toAssign = "D", newobj = "mixed.class", datasources = ds.test_env$connections[2:3])

    expect_error(ds.mdPattern(x = "mixed.class"), "not of the same class in all studies")
    expect_no_error(ds.mdPattern(x = "mixed.class", classConsistencyCheck = FALSE))
})

#
# Done
#

# context("ds.mdPattern::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "mixed.class"))
})

disconnect.studies.dataset.cnsim()

# context("ds.mdPattern::smk::done")
