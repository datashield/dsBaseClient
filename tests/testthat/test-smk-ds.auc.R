
#
# Set up
#

# context("ds.auc::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TRIG", "DIS_DIAB"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

ds.glmSLMA('D$DIS_DIAB~D$LAB_TRIG', family = "binomial", newobj = "auc.glm")
ds.glmPredict("auc.glm", output.type = "link", se.fit = FALSE, newobj = "auc.pred")

#
# Tests
#

# context("ds.auc::smk")
test_that("simple auc", {
    res <- ds.auc(pred = "auc.pred$fit", y = "auc.glm$y")

    expect_length(res, 3)
    for (study.res in res) {
        expect_named(study.res, c("AUC", "se"))
        expect_true(study.res$AUC > 0.5 && study.res$AUC < 1)
        expect_true(study.res$se > 0)
    }
})

test_that("fails if pred does not exist", {
    expect_error(
        ds.auc(pred = "nonexistent_object", y = "auc.glm$y"),
        "There are some DataSHIELD errors, list them with datashield.errors()",
        fixed = TRUE
    )

    res.errors <- DSI::datashield.errors()
    for (study.errors in res.errors) {
        expect_match(study.errors, "The server-side object 'nonexistent_object' does not exist")
    }
})

test_that("fails if y does not exist", {
    expect_error(
        ds.auc(pred = "auc.pred$fit", y = "nonexistent_object"),
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

# context("ds.auc::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "auc.glm", "auc.pred"))
})

disconnect.studies.dataset.cnsim()

# context("ds.auc::smk::done")
