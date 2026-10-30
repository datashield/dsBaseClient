#
# Tests
#

# context("utils::.resolveNewobjName")
test_that(".resolveNewobjName applies the default before validating", {
    expect_equal(.resolveNewobjName(NULL, "x.newobj"), "x.newobj")
    expect_equal(.resolveNewobjName("my.obj", "x.newobj"), "my.obj")
})

test_that(".resolveNewobjName rejects anything but a single character string", {
    expect_error(.resolveNewobjName(23, "x.newobj"), "'newobj' must be a single character string")
    expect_error(.resolveNewobjName(c("a", "b"), "x.newobj"), "'newobj' must be a single character string")
})
