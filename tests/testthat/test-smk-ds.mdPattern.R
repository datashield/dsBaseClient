
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
    caught_warnings <- capture_warnings(
        res <- ds.mdPattern(x = "D", type = "combine")
    )

    expect_named(res, c("pattern", "valid", "message", "studies"))
    expect_true(any(grepl("Disclosure control", caught_warnings)))
    expect_true(is.matrix(res$pattern))
})

test_that("combine pools pattern counts by variable name, not column position", {
    res.split <- ds.mdPattern(x = "D", type = "split")
    res.combine <- suppressWarnings(ds.mdPattern(x = "D", type = "combine"))

    # md.pattern rows are named by their count; the last row is a summary and
    # suppressed rows have a non-numeric name, so both are dropped
    count.by.pattern <- function(pattern){
        var.names <- sort(colnames(pattern)[-ncol(pattern)])
        rows <- pattern[-nrow(pattern), var.names, drop = FALSE]
        counts <- suppressWarnings(as.numeric(rownames(rows)))
        keep <- !is.na(counts)
        keys <- apply(rows[keep, , drop = FALSE], 1, paste, collapse = "")
        stats::setNames(counts[keep], keys)
    }

    per.study <- lapply(res.split, function(study.res) count.by.pattern(study.res$pattern))
    expected <- tapply(unlist(per.study, use.names = FALSE), unlist(lapply(per.study, names)), sum)
    actual <- tapply(count.by.pattern(res.combine$pattern), names(count.by.pattern(res.combine$pattern)), sum)

    expect_setequal(names(actual), names(expected))
    expect_equal(as.vector(actual[names(expected)]), as.vector(expected))
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
