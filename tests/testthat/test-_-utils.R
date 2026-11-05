#
# Tests
#

# context("utils::.checkArgProvided")
test_that(".checkArgProvided names the argument and reports the calling function", {
    check <- function(x) .checkArgProvided(x, "the name of the input vector")

    expect_null(check("a"))
    error <- expect_error(check(NULL), "`x` must be the name of the input vector")
    expect_equal(error$call, quote(check(NULL)))
})

# context("utils::.abortInvalidChoice")
test_that(".abortInvalidChoice lists the allowed values and the value given", {
    type <- "pooled"
    expect_error(.abortInvalidChoice("type", c("combine", "split")), "`type` must be \"combine\" or \"split\", not \"pooled\"")
    expect_error(.abortInvalidChoice("type", c("both", "combine", "split")), "`type` must be \"both\", \"combine\", or \"split\", not \"pooled\"")
})

# context("utils::.resolveNewobjName")
test_that(".resolveNewobjName applies the default before validating", {
    expect_equal(.resolveNewobjName(NULL, "x.newobj"), "x.newobj")
    expect_equal(.resolveNewobjName("my.obj", "x.newobj"), "my.obj")
})

test_that(".resolveNewobjName rejects anything but a single character string", {
    expect_error(.resolveNewobjName(23, "x.newobj"), "`newobj` must be a single character string")
    expect_error(.resolveNewobjName(c("a", "b"), "x.newobj"), "`newobj` must be a single character string")
})

# context("utils::.verifyDatasources")
test_that(".verifyDatasources rejects objects that are not connections", {
    expect_error(.verifyDatasources(list(1)), "`datasources` must be a list of <DSConnection> objects")
})

# context("utils::.checkClassConsistency")
test_that(".checkClassConsistency compares the chosen field across studies", {
    same <- list(a = list(class = "numeric"), b = list(class = "numeric"))
    different <- list(a = list(class = "numeric"), b = list(class = "factor"))

    expect_null(.checkClassConsistency(same))
    expect_error(.checkClassConsistency(different), "The input object must have the same class")
    expect_error(.checkClassConsistency(different, objectName = "D"), "\"D\" must have the same class")
    expect_error(.checkClassConsistency(different), "b has class factor")
})

# context("utils::.seedAsText")
test_that(".seedAsText converts the seed and rejects anything that isn't a number", {
    expect_equal(.seedAsText(NULL), "NULL")
    expect_equal(.seedAsText(27), "27")
    expect_error(.seedAsText("a"), "`seed.as.integer` must be an integer or `NULL`")
})

# context("utils::.checkArgsSet")
test_that(".checkArgsSet names every argument", {
    expect_null(.checkArgsSet(a = 1, b = 2))
    expect_error(.checkArgsSet(a = NULL, b = 2), "`a` and `b` must all be set")
})

# context("utils::.warnOffsetInFormula")
test_that(".warnOffsetInFormula looks for an offset or weights in the formula", {
    expect_no_warning(.warnOffsetInFormula("y~x"))
    expect_warning(.warnOffsetInFormula("y~offset(x)"), "offset or regression weights")
    expect_warning(.warnOffsetInFormula("y~x+WEIGHTS"), "offset or regression weights")
})
