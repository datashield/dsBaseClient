#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

context("isDefined::smk::setup")

connect.discordant.dataset.simple(list("A", "B", "C"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# default

context("isDefined::smk::default")
test_that("default test, dataframe D", {
    res <- isDefined(ds.test_env$connections, "D")
    
    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_true(res$discordant2)
    expect_true(res$discordant3)
})

test_that("default test, dataframe E", {
    expect_error(isDefined(ds.test_env$connections, "E"), "The input object E is not defined in discordant1, discordant2, discordant3!", fixed=TRUE)
})

test_that("default test, dataframe column E$A", {
#    expect_error(isDefined(ds.test_env$connections, "E$A"), "The input object E$A is not defined in discordant1, discordant2, discordant3!", fixed=TRUE)
    expect_error(isDefined(ds.test_env$connections, "E$A"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)

    err <- DSI::datashield.errors();

    expect_length(class(err), 1)
    expect_true(all("list" %in% class(err)))
    expect_length(err, 3)
    expect_match(err$discordant1, "* : object 'E' not found")
    expect_match(err$discordant2, "* : object 'E' not found")
    expect_match(err$discordant3, "* : object 'E' not found")
})

test_that("default test, dataframe column D$A", {
    expect_error(isDefined(ds.test_env$connections, "D$A"), "The input object D$A is not defined in discordant3!", fixed=TRUE)
})

test_that("default test, dataframe column D$B", {
    expect_error(isDefined(ds.test_env$connections, "D$B"), "The input object D$B is not defined in discordant2!", fixed=TRUE)
})

test_that("default test, dataframe column D$C", {
    expect_error(isDefined(ds.test_env$connections, "D$C"), "The input object D$C is not defined in discordant1!", fixed=TRUE)
})

test_that("default test, dataframe column D$D", {
    expect_error(isDefined(ds.test_env$connections, "D$D"), "The input object D$D is not defined in discordant1, discordant2, discordant3!", fixed=TRUE)
})

test_that("default test, dataframe columns D,D$B", {
#    expect_error(isDefined(ds.test_env$connections, c("D","D$A")), "The input object D$A is not defined in discordant3!", fixed=TRUE)
    res <- isDefined(ds.test_env$connections, c("D","D$A"))

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_true(res$discordant2)
    expect_true(res$discordant3)
})

test_that("default test, dataframe columns D$A,D$B", {
    expect_error(isDefined(ds.test_env$connections, c("D$A","D$B")), "The input object D$A is not defined in discordant3!", fixed=TRUE)
})

# error.message = FALSE

context("isDefined::smk::error.message=FALSE")
test_that("error.message=FALSE test, dataframe D", {
    res <- isDefined(ds.test_env$connections, "D", error.message = FALSE)
    
    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_true(res$discordant2)
    expect_true(res$discordant3)
})

test_that("error.message=FALSE test, dataframe E", {
    res <- isDefined(ds.test_env$connections, "E", error.message = FALSE)

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_false(res$discordant1)
    expect_false(res$discordant2)
    expect_false(res$discordant3)
})

test_that("error.message=FALSE test, dataframe column E$A", {
#    expect_error(isDefined(ds.test_env$connections, "E$A", error.message = FALSE), "The input object E$A is not defined in discordant1, discordant2, discordant3!", fixed=TRUE)
    expect_error(isDefined(ds.test_env$connections, "E$A", error.message = FALSE), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)

    err <- DSI::datashield.errors();

    expect_length(class(err), 1)
    expect_true(all("list" %in% class(err)))
    expect_length(err, 3)
    expect_match(err$discordant1, "* : object 'E' not found")
    expect_match(err$discordant2, "* : object 'E' not found")
    expect_match(err$discordant3, "* : object 'E' not found")
})

test_that("error.message=FALSE test, dataframe column D$A", {
    res <- isDefined(ds.test_env$connections, "D$A", error.message = FALSE)

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_true(res$discordant2)
    expect_false(res$discordant3)
})

test_that("error.message=FALSE test, dataframe column D$B", {
    res <- isDefined(ds.test_env$connections, "D$B", error.message = FALSE)

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_false(res$discordant2)
    expect_true(res$discordant3)
})

test_that("error.message=FALSE test, dataframe column D$C", {
    res <- isDefined(ds.test_env$connections, "D$C", error.message = FALSE)

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_false(res$discordant1)
    expect_true(res$discordant2)
    expect_true(res$discordant3)
})

test_that("error.message=FALSE test, dataframe column D$D", {
    res <- isDefined(ds.test_env$connections, "D$D", error.message = FALSE)

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_false(res$discordant1)
    expect_false(res$discordant2)
    expect_false(res$discordant3)
})

test_that("error.message=FALSE test, dataframe columns D,D$B", {
#    expect_error(isDefined(ds.test_env$connections, c("D","D$A")), "The input object D$A is not defined in discordant3!", fixed=TRUE)
    res <- isDefined(ds.test_env$connections, c("D","D$A"), error.message=FALSE)

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_true(res$discordant2)
    expect_true(res$discordant3)
})

test_that("error.message=FALSE test, dataframe columns D$A,D$B", {
    res <- isDefined(ds.test_env$connections, c("D$A","D$B"), error.message=FALSE)

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_true(res$discordant2)
    expect_false(res$discordant3)
})

# error.message = TRUE

context("isDefined::smk::error.message=TRUE")
test_that("error.message=TRUE test, dataframe D", {
    res <- isDefined(ds.test_env$connections, "D", error.message = TRUE)
    
    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_true(res$discordant2)
    expect_true(res$discordant3)
})

test_that("error.message=TRUE test, dataframe E", {
    expect_error(isDefined(ds.test_env$connections, "E", error.message = TRUE), "The input object E is not defined in discordant1, discordant2, discordant3!", fixed=TRUE)
})

test_that("error.message=TRUE test, dataframe column E$A", {
#    expect_error(isDefined(ds.test_env$connections, "E$A", error.message = TRUE), "The input object E$A is not defined in discordant1, discordant2, discordant3!", fixed=TRUE)
    expect_error(isDefined(ds.test_env$connections, "E$A", error.message = TRUE), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)

    err <- DSI::datashield.errors();

    expect_length(class(err), 1)
    expect_true(all("list" %in% class(err)))
    expect_length(err, 3)
    expect_match(err$discordant1, "* : object 'E' not found")
    expect_match(err$discordant2, "* : object 'E' not found")
    expect_match(err$discordant3, "* : object 'E' not found")
})

test_that("error.message=TRUE test, dataframe column D$A", {
    expect_error(isDefined(ds.test_env$connections, "D$A", error.message = TRUE), "The input object D$A is not defined in discordant3!", fixed=TRUE)
})

test_that("error.message=TRUE test, dataframe column D$B", {
    expect_error(isDefined(ds.test_env$connections, "D$B", error.message = TRUE), "The input object D$B is not defined in discordant2!", fixed=TRUE)
})

test_that("error.message=TRUE test, dataframe column D$C", {
    expect_error(isDefined(ds.test_env$connections, "D$C", error.message = TRUE), "The input object D$C is not defined in discordant1!", fixed=TRUE)
})

test_that("error.message=TRUE test, dataframe column D$D", {
    expect_error(isDefined(ds.test_env$connections, "D$D", error.message = TRUE), "The input object D$D is not defined in discordant1, discordant2, discordant3!", fixed=TRUE)
})

test_that("error.message=TRUE test, dataframe columns D,D$B", {
#    expect_error(isDefined(ds.test_env$connections, c("D","D$A")), "The input object D$A is not defined in discordant3!", fixed=TRUE)
    res <- isDefined(ds.test_env$connections, c("D","D$A"), error.message=TRUE)

    expect_length(class(res), 1)
    expect_true(all("list" %in% class(res)))
    expect_length(res, 3)
    expect_true(res$discordant1)
    expect_true(res$discordant2)
    expect_true(res$discordant3)
})

test_that("error.message=TRUE test, dataframe columns D$A,D$B", {
    expect_error(isDefined(ds.test_env$connections, c("D$A","D$B"), error.message=TRUE), "The input object D$A is not defined in discordant3!", fixed=TRUE)
})

#
# Done
#

context("isDefined::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.discordant.dataset.simple()

context("isDefined::smk::done")
