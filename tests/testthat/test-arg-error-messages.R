
#
# Set up
#

# context("error messages::arg::setup")

studies <- list(sim1 = "study1", sim2 = "study2", sim3 = "study3")

# Replaces the server with canned replies, named by server function. A reply that is a
# function is called with the study name and the expression, so it can differ between them.
fake_server <- function(..., env = parent.frame()) {
    replies <- list(...)
    answer <- function(conns, expr, ...) {
        reply <- replies[[sub("\\(.*$", "", as.character(expr)[[1]])]]
        study_names <- stats::setNames(names(conns), names(conns))
        lapply(study_names, function(study) if (is.function(reply)) reply(study, expr) else reply)
    }
    assign_nothing <- function(...) invisible(NULL)
    testthat::local_mocked_bindings(datashield.aggregate = answer, datashield.assign = assign_nothing,
        .package = "DSI", .env = env)
    testthat::local_mocked_bindings(datashield.aggregate = answer, datashield.assign = assign_nothing,
        .resolveDatasources = function(datasources) datasources, .env = env)
}

missing_pattern <- function(columns, count = "5") {
    pattern <- matrix(1, nrow = 2, ncol = length(columns), dimnames = list(c(count, ""), columns))
    list(pattern = pattern, valid = TRUE, message = "Valid")
}

one_bar <- structure(list(breaks = 1:2, counts = 1, density = 1, mids = 1.5, xname = "x", equidist = TRUE),
    class = "histogram")

#
# Tests
#

# context("error messages::arg::argument checks")
test_that("ds.asFactorSimple rejects a name that is not a character string", {
    fake_server()
    expect_error(ds.asFactorSimple(input.var.name = 1, datasources = studies), "`input.var.name` must be a character string")
})

test_that("ds.dmtC2S rejects missing and unsupported data", {
    fake_server()
    expect_error(ds.dmtC2S(dfdata = NULL, datasources = studies), "`dfdata` must be a character string")
    expect_error(ds.dmtC2S(dfdata = 1, datasources = studies), "`dfdata` must be a data.frame, matrix or tibble")
})

test_that("ds.elspline rejects fewer than two knots", {
    fake_server()
    expect_error(ds.elspline(x = "a", n = 1, datasources = studies), "`n` must be greater than or equal to 2")
})

test_that("ds.forestplot rejects a variable that is not in the model", {
    betas <- matrix(1, dimnames = list("age", "study1"))
    mod <- list(num.valid.studies = 1, output.summary = list(study1 = 1, input.beta.matrix.for.SLMA = betas, input.se.matrix.for.SLMA = betas))
    expect_error(ds.forestplot(mod, variable = "bmi"), "not \"bmi\"")
})

test_that("ds.getWGSR rejects a missing thirdPart and an invalid standing", {
    fake_server()
    expect_error(ds.getWGSR(sex = "s", firstPart = "a", secondPart = "b", index = "bfa", datasources = studies), "`thirdPart` must be provided")
    expect_error(ds.getWGSR(sex = "s", firstPart = "a", secondPart = "b", index = "hfa", standing = 4, datasources = studies), "`standing` must be 1, 2 or 3")
})

test_that("ds.glmSummary rejects a name that is not a character string", {
    fake_server()
    expect_error(ds.glmSummary(x.name = 1, datasources = studies), "`x.name` must be a character string")
})

test_that("ds.metadata rejects a missing or invalid name", {
    fake_server()
    expect_error(ds.metadata(x = NULL, datasources = studies), "`x` must be the name of an object")
    expect_error(ds.metadata(x = 1, datasources = studies), "`x` must be a single character string")
})

test_that("ds.rBinom rejects missing and out-of-range arguments", {
    fake_server()
    expect_error(ds.rBinom(samp.size = NULL, newobj = "x", datasources = studies), "must all be set")
    expect_error(ds.rBinom(size = -1, prob = 0.5, newobj = "x", datasources = studies), "`size` must be greater than 0")
    expect_error(ds.rBinom(size = 1, prob = 2, newobj = "x", datasources = studies), "`prob` must be between 0 and 1")
})

test_that("ds.rNorm rejects missing and out-of-range arguments", {
    fake_server()
    expect_error(ds.rNorm(samp.size = NULL, datasources = studies), "must all be set")
    expect_error(ds.rNorm(sd = -1, datasources = studies), "`sd` must be greater than 0")
    expect_error(ds.rNorm(force.output.to.k.decimal.places = 10, datasources = studies), "must be an integer between 0 and 9")
})

test_that("ds.rPois rejects missing and out-of-range arguments", {
    fake_server()
    expect_error(ds.rPois(samp.size = NULL, datasources = studies), "must all be set")
    expect_error(ds.rPois(lambda = 0, datasources = studies), "`lambda` must be greater than 0")
})

test_that("ds.rUnif rejects missing and out-of-range arguments", {
    fake_server()
    expect_error(ds.rUnif(samp.size = NULL, datasources = studies), "must all be set")
    expect_error(ds.rUnif(min = 2, max = 1, datasources = studies), "`max` must be greater than `min`")
    expect_error(ds.rUnif(force.output.to.k.decimal.places = 10, datasources = studies), "must be an integer between 0 and 9")
})

test_that("ds.replaceNA rejects the wrong number of replacement values", {
    fake_server(numNaDS = list(numNA = 2))
    expect_error(ds.replaceNA("x", forNA = list(1), datasources = studies), "`forNA` must have one element per study")
    expect_error(ds.replaceNA("x", forNA = list(1:3, 1:3, 1:3), datasources = studies), "Replacement values must have length 1")
})

test_that("ds.rowColCalc rejects a missing operation", {
    fake_server()
    expect_error(ds.rowColCalc("x", operation = NULL, datasources = studies), "`operation` must be \"rowSums\"")
})

test_that("ds.seq rejects each invalid argument", {
    fake_server()
    expect_error(ds.seq(FROM.value.char = 1, TO.value.char = "2", datasources = studies), "`FROM.value.char` must be a real number")
    expect_error(ds.seq(TO.value.char = 2, datasources = studies), "`TO.value.char` must be a real number")
    expect_error(ds.seq(BY.value.char = 1, TO.value.char = "2", datasources = studies), "`BY.value.char` must be a real number")
    expect_error(ds.seq(LENGTH.OUT.value.char = 1, datasources = studies), "`LENGTH.OUT.value.char` must be an integer")
    expect_error(ds.seq(ALONG.WITH.name = 1, datasources = studies), "`ALONG.WITH.name` must be the name")
    expect_error(ds.seq(datasources = studies), "must be set")
})

test_that("ds.tapply and ds.tapply.assign reject more than two factors", {
    fake_server()
    expect_error(ds.tapply("x", c("a", "b", "c"), "sum", datasources = studies), "`INDEX.names` must name no more than two factors")
    expect_error(ds.tapply.assign("x", c("a", "b", "c"), "sum", datasources = studies), "`INDEX.names` must name no more than two factors")
})

test_that("ds.table rejects a force.nfilter that is not a character string", {
    fake_server()
    expect_error(ds.table("r", force.nfilter = 3, datasources = studies), "`force.nfilter` must be a character string")
})

# context("error messages::arg::warnings before the server call")
test_that("ds.changeRefGroup warns about reorderByRef", {
    fake_server()
    expect_warning(ds.changeRefGroup("x", ref = "a", reorderByRef = TRUE, datasources = studies), "`reorderByRef` is `TRUE`")
})

test_that("model functions warn about an offset or weights in the formula", {
    fake_server()
    formula <- "y~x+offset(z)"
    pattern <- "offset or regression weights in the model formula"
    expect_warning(tryCatch(ds.glm(formula, datasources = studies), error = function(e) NULL), pattern)
    expect_warning(tryCatch(ds.glmSLMA(formula, datasources = studies), error = function(e) NULL), pattern)
    expect_warning(tryCatch(ds.glmerSLMA(formula, datasources = studies), error = function(e) NULL), pattern)
    expect_warning(tryCatch(ds.lmerSLMA(formula, datasources = studies), error = function(e) NULL), pattern)
})

# context("error messages::arg::server replies")
test_that("checkClass rejects classes that differ across studies", {
    fake_server(classDS = function(study, expr) if (study == "sim1") "numeric" else "character")
    expect_error(checkClass(studies, "x"), "\"x\" must have the same class in all studies")
})

test_that("isDefined names the studies that lack the object", {
    fake_server(exists = function(study, expr) study != "sim2")
    expect_error(isDefined(studies, "x"), "\"x\" isn't defined in \"sim2\"")
})

test_that("ds.asFactor reports levels the server failed to find", {
    fake_server(asFactorDS1 = "FAILED: too many levels")
    expect_error(ds.asFactor("x", datasources = studies), "Can't obtain the factor levels")
})

test_that("ds.boxPlot rejects a class it can't plot", {
    fake_server(classDS = "character")
    expect_error(ds.boxPlot("x", datasources = studies), "`x` must be a data.frame or numeric vector")
})

test_that("ds.rbind and ds.summary reject an unsupported class", {
    fake_server(classDS = "function")
    expect_error(ds.rbind("x", DataSHIELD.checks = TRUE, datasources = studies), "Each object must be of class")
    expect_error(ds.summary("x", datasources = studies), "`x` must be of class")
})

test_that("ds.cbind and ds.dataFrame warn about duplicate names and reject unequal rows", {
    fake_server(classDS = "numeric")
    testthat::local_mocked_bindings(ds.length = function(x, ...) list(if (x == "a") 3 else 4))
    x <- c("b", "a", "a")
    expect_warning(
        expect_error(ds.cbind(x, DataSHIELD.checks = TRUE, datasources = studies[1]), "All components must have the same number of rows"),
        "column names in study 1 are duplicated"
    )
    expect_warning(
        expect_error(ds.dataFrame(x, DataSHIELD.checks = TRUE, datasources = studies[1]), "All components must have the same number of rows"),
        "column names in study 1 are duplicated"
    )
})

test_that("ds.histogram reports missing break points and invalid cells", {
    fake_server(
        histogramDS1 = list(range = c(1, 2)),
        histogramDS2 = function(study, expr) if (study == "sim1") list(histobject = one_bar, invalidcells = 2)
    )
    expect_error(ds.histogram("x", datasources = studies), "Can't find equidistant break points")
    expect_warning(ds.histogram("x", datasources = studies[1]), "2 bins in \"sim1\" had fewer than")

    fake_server(histogramDS1 = list(range = c(1, 2)), histogramDS2 = list(histobject = one_bar, invalidcells = 2))
    warnings <- capture_warnings(ds.histogram("x", datasources = studies[1:2]))
    expect_match(warnings, "2 bins in \"sim1\" had fewer than", all = FALSE)
    expect_match(warnings, "2 bins in \"sim2\" had fewer than", all = FALSE)
})

test_that("ds.kurtosis and ds.skewness report too few observations for a combined result", {
    fake_server()
    testthat::local_mocked_bindings(ds.mean = function(...) list(Global.Mean = matrix(NA, 1, 1, dimnames = list(NULL, "EstimatedMean"))))
    expect_error(ds.kurtosis("x", type = "combine", datasources = studies), "number of valid observations in at least one study")
    expect_error(ds.skewness("x", type = "combine", datasources = studies), "number of valid observations in at least one study")
})

test_that("ds.mdPattern warns about suppressed patterns and rejects patterns it can't pool", {
    suppressed <- function(study, expr) {
        result <- missing_pattern(c("a", ""))
        result$valid <- study != "sim2"
        result
    }
    fake_server(mdPatternDS = suppressed)
    expect_warning(ds.mdPattern("x", type = "combine", datasources = studies), "suppressed by disclosure control")

    fake_server(mdPatternDS = function(study, expr) missing_pattern(if (study == "sim1") c("a", "") else c("a", "b", "")))
    expect_error(ds.mdPattern("x", type = "combine", datasources = studies), "different numbers of variables")

    fake_server(mdPatternDS = function(study, expr) missing_pattern(if (study == "sim1") c("a", "") else c("b", "")))
    expect_error(ds.mdPattern("x", type = "combine", datasources = studies), "studies have different variables")
})

test_that("ds.table reports factors it can't obtain and tables that differ in shape", {
    levels_except <- function(name) function(study, expr) if (expr[[2]] != name) "1"
    fake_server(asFactorDS1 = levels_except("r"))
    expect_error(ds.table("r", datasources = studies), "Can't obtain factors for `rvar`")
    fake_server(asFactorDS1 = levels_except("c"))
    expect_error(ds.table("r", "c", datasources = studies), "Can't obtain factors for `cvar`")
    fake_server(asFactorDS1 = levels_except("s"))
    expect_error(ds.table("r", "c", "s", datasources = studies), "Can't obtain factors for `stvar`")

    fake_server(asFactorDS1 = "1", tableDS = function(study, expr) if (study == "sim1") table(1) else table(1, 1))
    expect_error(suppressMessages(ds.table("r", datasources = studies)), "different numbers of dimensions")
})

test_that("glmChecks reports variables that fail its checks", {
    formula <- D$y ~ D$x
    fake_server(exists = FALSE)
    expect_error(suppressMessages(glmChecks(y ~ 1, NULL, NULL, NULL, studies[1])), "\"y\" isn't defined in \"sim1\"")

    fake_server(exists = TRUE, isNaDS = list(is.na = TRUE))
    expect_error(suppressMessages(glmChecks(formula, NULL, NULL, NULL, studies[1])), "\"D\\$y\" has only missing values in \"sim1\"")

    fake_server(exists = TRUE, isNaDS = list(is.na = FALSE), classDS = "character")
    expect_error(suppressMessages(glmChecks(formula, NULL, "D$o", NULL, studies[1])), "\"D\\$o\" must be numeric in \"sim1\"")

    fake_server(exists = TRUE, isNaDS = list(is.na = FALSE), classDS = "numeric", checkNegValueDS = TRUE)
    expect_error(suppressMessages(glmChecks(formula, NULL, NULL, "D$w", studies[1])), "Weights \"D\\$w\" must not be negative in \"sim1\"")
})

test_that("ds.meanSdGp rejects invalid tables", {
    fake_server(meanSdGpDS = list(Table_valid = FALSE))
    expect_error(ds.meanSdGp("x", "y", classConsistencyCheck = FALSE, datasources = studies), "At least one cell count is below")
})

test_that("ds.glm and ds.glmSLMA stop when a study's data is invalid", {
    invalid <- list(list(NA, 1), "x", 1, 0, 0, 0, 0, errorMessage = "No errors")
    fake_server(glmDS1 = invalid)
    suppressMessages(expect_error(ds.glm("y~x", family = "gaussian", datasources = studies), "because of a data error"))

    fake_server(glmDS2 = list(disclosure.risk = 1, 1, 1, 1, 1, 1, 1, 1, 1, "No errors"), glmDS1 = list(list(NA, 1), "x", 0, 0, 0, 0, 0, errorMessage = "No errors"))
    suppressMessages(expect_error(ds.glm("y~x", family = "gaussian", datasources = studies), "potential disclosure risk"))
})
