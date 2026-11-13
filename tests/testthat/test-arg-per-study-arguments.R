
#
# Set up
#

# context("per-study arguments::arg::setup")

studies <- list(sim1 = "study1", sim2 = "study2", sim3 = "study3")

# Replaces the server calls so a test can see exactly what each study is sent
record_server_calls <- function(env = parent.frame()) {
    sent <- new.env()
    sent$calls <- list()
    testthat::local_mocked_bindings(
        datashield.assign = function(conns, symbol, value, ...) {
            sent$calls[[length(sent$calls) + 1]] <- value
            invisible(NULL)
        },
        datashield.aggregate = function(conns, expr, ...) list(),
        .resolveDatasources = function(datasources) datasources,
        .env = env
    )
    sent
}

#
# Tests
#

# context("per-study arguments::arg::.expandToStudies")
test_that(".expandToStudies repeats a single value for every study", {
    expect_equal(.expandToStudies(5, "x", 3), c(5, 5, 5))
})

test_that(".expandToStudies keeps one value per study", {
    expect_equal(.expandToStudies(c(1, 2, 3), "x", 3), c(1, 2, 3))
})

test_that(".expandToStudies rejects any other length", {
    expect_error(.expandToStudies(c(1, 2), "x", 3), "must have length 1")
    expect_error(.expandToStudies(c(1, 2, 3, 4), "x", 3), "must have length 1")
})

# context("per-study arguments::arg::ds.rBinom")
test_that("ds.rBinom sends each study its own size and prob", {
    sent <- record_server_calls()
    suppressMessages(ds.rBinom(samp.size = 10, size = c(1, 10, 100), prob = c(0.1, 0.5, 0.9), newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    expect_equal(sent$calls[[1]], call("rBinomDS", 10, size = 1, prob = 0.1))
    expect_equal(sent$calls[[2]], call("rBinomDS", 10, size = 10, prob = 0.5))
    expect_equal(sent$calls[[3]], call("rBinomDS", 10, size = 100, prob = 0.9))
})

test_that("ds.rBinom sends a single size and prob to every study", {
    sent <- record_server_calls()
    suppressMessages(ds.rBinom(samp.size = 10, size = 5, prob = 0.5, newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    for (k in 1:3) {
        expect_equal(sent$calls[[k]], call("rBinomDS", 10, size = 5, prob = 0.5))
    }
})

test_that("ds.rBinom rejects size or prob with the wrong number of values", {
    sent <- record_server_calls()

    expect_error(ds.rBinom(samp.size = 10, size = c(1, 10), prob = 0.5, newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_error(ds.rBinom(samp.size = 10, size = 5, prob = c(0.1, 0.5), newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_length(sent$calls, 0)
})

test_that("ds.rBinom checks every study's prob", {
    sent <- record_server_calls()
    expect_error(ds.rBinom(samp.size = 10, size = 5, prob = c(0.5, 1.5, 0.5), newobj = "x", seed.as.integer = 27, datasources = studies), "must be between 0 and 1")

    expect_length(sent$calls, 0)
})

# context("per-study arguments::arg::ds.rNorm")
test_that("ds.rNorm sends each study its own mean, sd and decimal places", {
    sent <- record_server_calls()
    suppressMessages(ds.rNorm(samp.size = 10, mean = c(0, 100, 200), sd = c(1, 2, 3), force.output.to.k.decimal.places = c(2, 4, 9), newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    expect_equal(sent$calls[[1]], call("rNormDS", 10, mean = 0, sd = 1, force.output.to.k.decimal.places = 2))
    expect_equal(sent$calls[[2]], call("rNormDS", 10, mean = 100, sd = 2, force.output.to.k.decimal.places = 4))
    expect_equal(sent$calls[[3]], call("rNormDS", 10, mean = 200, sd = 3, force.output.to.k.decimal.places = 9))
})

test_that("ds.rNorm sends each study its own server-side object name", {
    sent <- record_server_calls()
    suppressMessages(ds.rNorm(samp.size = 10, mean = c("m1", "m2", "m3"), sd = 1, newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    expect_equal(sent$calls[[1]], call("rNormDS", 10, mean = "m1", sd = 1, force.output.to.k.decimal.places = 9))
    expect_equal(sent$calls[[2]], call("rNormDS", 10, mean = "m2", sd = 1, force.output.to.k.decimal.places = 9))
    expect_equal(sent$calls[[3]], call("rNormDS", 10, mean = "m3", sd = 1, force.output.to.k.decimal.places = 9))
})

test_that("ds.rNorm sends a single mean, sd and decimal places to every study", {
    sent <- record_server_calls()
    suppressMessages(ds.rNorm(samp.size = 10, mean = 5, sd = 2, force.output.to.k.decimal.places = 4, newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    for (k in 1:3) {
        expect_equal(sent$calls[[k]], call("rNormDS", 10, mean = 5, sd = 2, force.output.to.k.decimal.places = 4))
    }
})

test_that("ds.rNorm rejects mean, sd or decimal places with the wrong number of values", {
    sent <- record_server_calls()

    expect_error(ds.rNorm(samp.size = 10, mean = c(0, 100), sd = 1, newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_error(ds.rNorm(samp.size = 10, mean = 0, sd = c(1, 2), newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_error(ds.rNorm(samp.size = 10, mean = 0, sd = 1, force.output.to.k.decimal.places = c(2, 4), newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_length(sent$calls, 0)
})

test_that("ds.rNorm checks every study's sd", {
    sent <- record_server_calls()
    expect_error(ds.rNorm(samp.size = 10, mean = 0, sd = c(1, -1, 1), newobj = "x", seed.as.integer = 27, datasources = studies), "must be greater than 0")

    expect_length(sent$calls, 0)
})

# context("per-study arguments::arg::ds.rPois")
test_that("ds.rPois sends each study its own lambda", {
    sent <- record_server_calls()
    suppressMessages(ds.rPois(samp.size = 10, lambda = c(1, 50, 100), newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    expect_equal(sent$calls[[1]], call("rPoisDS", 10, lambda = 1))
    expect_equal(sent$calls[[2]], call("rPoisDS", 10, lambda = 50))
    expect_equal(sent$calls[[3]], call("rPoisDS", 10, lambda = 100))
})

test_that("ds.rPois sends a single lambda to every study", {
    sent <- record_server_calls()
    suppressMessages(ds.rPois(samp.size = 10, lambda = 4, newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    for (k in 1:3) {
        expect_equal(sent$calls[[k]], call("rPoisDS", 10, lambda = 4))
    }
})

test_that("ds.rPois rejects lambda with the wrong number of values", {
    sent <- record_server_calls()

    expect_error(ds.rPois(samp.size = 10, lambda = c(1, 50), newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_length(sent$calls, 0)
})

test_that("ds.rPois checks every study's lambda", {
    sent <- record_server_calls()
    expect_error(ds.rPois(samp.size = 10, lambda = c(1, 0, 1), newobj = "x", seed.as.integer = 27, datasources = studies), "must be greater than 0")

    expect_length(sent$calls, 0)
})

# context("per-study arguments::arg::ds.rUnif")
test_that("ds.rUnif sends each study its own min, max and decimal places", {
    sent <- record_server_calls()
    suppressMessages(ds.rUnif(samp.size = 10, min = c(0, 2, 5), max = c(2, 5, 9), force.output.to.k.decimal.places = c(1, 2, 3), newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    expect_equal(sent$calls[[1]], call("rUnifDS", 10, min = 0, max = 2, force.output.to.k.decimal.places = 1))
    expect_equal(sent$calls[[2]], call("rUnifDS", 10, min = 2, max = 5, force.output.to.k.decimal.places = 2))
    expect_equal(sent$calls[[3]], call("rUnifDS", 10, min = 5, max = 9, force.output.to.k.decimal.places = 3))
})

test_that("ds.rUnif sends a single min, max and decimal places to every study", {
    sent <- record_server_calls()
    suppressMessages(ds.rUnif(samp.size = 10, min = 0, max = 1, force.output.to.k.decimal.places = 2, newobj = "x", seed.as.integer = 27, datasources = studies))

    expect_length(sent$calls, 3)
    for (k in 1:3) {
        expect_equal(sent$calls[[k]], call("rUnifDS", 10, min = 0, max = 1, force.output.to.k.decimal.places = 2))
    }
})

test_that("ds.rUnif rejects min, max or decimal places with the wrong number of values", {
    sent <- record_server_calls()

    expect_error(ds.rUnif(samp.size = 10, min = c(0, 2), max = 10, newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_error(ds.rUnif(samp.size = 10, min = 0, max = c(2, 5), newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_error(ds.rUnif(samp.size = 10, min = 0, max = 1, force.output.to.k.decimal.places = c(1, 2), newobj = "x", seed.as.integer = 27, datasources = studies), "must have length 1")
    expect_length(sent$calls, 0)
})

test_that("ds.rUnif checks every study's min and max", {
    sent <- record_server_calls()
    expect_error(ds.rUnif(samp.size = 10, min = c(0, 5, 0), max = c(1, 2, 1), newobj = "x", seed.as.integer = 27, datasources = studies), "must be greater than")

    expect_length(sent$calls, 0)
})

#
# Done
#

# context("per-study arguments::arg::done")
