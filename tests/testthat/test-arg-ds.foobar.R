#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.foobar::arg::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.foobar::arg::aggregate")
test_that("NULL connections", {
    calltext <- call("fooBarDS")
    if (ds.test_env$driver == "ArmadilloDriver") {
        expect_error(datashield.aggregate(conns=NULL, expr=calltext), "no applicable method for `@` applied to an object of class \"NULL\"", fixed=TRUE)
#        expect_error(datashield.aggregate(conns=NULL, expr=calltext), "trying to get slot \"name\" from an object of a basic class (\"NULL\") with no slots", fixed=TRUE)
    } else if (ds.test_env$driver == "OpalDriver") {
        expect_error(datashield.aggregate(conns=NULL, expr=calltext), "no applicable method for `@` applied to an object of class \"NULL\"", fixed=TRUE)
    } else {
        fail(message = "Unknown driver type", info = ds.test_env$driver)
    }

    errs <- datashield.errors()
    expect_length(errs, 0)
})

context("ds.foobar::arg::aggregate")
test_that("NULL expr", {
    calltext <- call("fooBarDS")
    expect_error(expect_warning(datashield.aggregate(conns=ds.test_env$connections, expr=NULL), "'x' is NULL so the result will be NULL", fixed = TRUE), "replacement has length zero", fixed = TRUE)

    errs <- datashield.errors()
    expect_length(errs, 0)
})

context("ds.foobar::arg::aggregate")
test_that("non existent aggregate foobarDS", {
    calltext <- call("fooBarDS")
    expect_error(datashield.aggregate(conns=ds.test_env$connections, expr=calltext))

    errs <- datashield.errors()

    expect_length(errs, 3)
    expect_length(errs$sim1, 1)
    expect_true(errs$sim1 %in% c("Command 'fooBarDS()' failed on 'sim1': No such DataSHIELD 'AGGREGATE' method with name: fooBarDS", "[Client error: (400) Bad Request] No such DataSHIELD 'AGGREGATE' method with name: fooBarDS", "Bad request: No such DataSHIELD 'AGGREGATE' method with name: fooBarDS"))
    expect_length(errs$sim2, 1)
    expect_true(errs$sim2 %in% c("Command 'fooBarDS()' failed on 'sim2': No such DataSHIELD 'AGGREGATE' method with name: fooBarDS", "[Client error: (400) Bad Request] No such DataSHIELD 'AGGREGATE' method with name: fooBarDS", "Bad request: No such DataSHIELD 'AGGREGATE' method with name: fooBarDS"))
    expect_length(errs$sim3, 1)
    expect_true(errs$sim3 %in% c("Command 'fooBarDS()' failed on 'sim3': No such DataSHIELD 'AGGREGATE' method with name: fooBarDS", "[Client error: (400) Bad Request] No such DataSHIELD 'AGGREGATE' method with name: fooBarDS", "Bad request: No such DataSHIELD 'AGGREGATE' method with name: fooBarDS"))
})

context("ds.foobar::arg::assign")
test_that("NULL connections", {
    calltext <- call("fooBarDS")
    if (ds.test_env$driver == "ArmadilloDriver") {
        expect_error(datashield.assign(conns=NULL, symbol="new_obj", value=calltext), "no applicable method for `@` applied to an object of class \"NULL\"", fixed=TRUE)
#        expect_error(datashield.assign(conns=NULL, symbol="new_obj", value=calltext), "trying to get slot \"name\" from an object of a basic class (\"NULL\") with no slots", fixed=TRUE)
    } else if (ds.test_env$driver == "OpalDriver") {
        expect_error(datashield.assign(conns=NULL, symbol="new_obj", value=calltext), "no applicable method for `@` applied to an object of class \"NULL\"", fixed=TRUE)
    } else {
        fail(message = "Unknown driver type", info = ds.test_env$driver)
    }

    errs <- datashield.errors()
    expect_length(errs, 0)
})

#context("ds.foobar::arg::assign")
#test_that("NULL symbol", {
#    calltext <- call("fooBarDS")
#    res <- datashield.assign(conns=ds.test_env$connections, symbol=NULL, value=calltext)
#    expect_true(is.raw(res))
#    expect_length(res, 0)
#})

context("ds.foobar::arg::assign")
test_that("NULL value", {
    calltext <- call("fooBarDS")
    expect_error(datashield.assign(conns=ds.test_env$connections, symbol="new_obj", value=NULL), "Not a valid table name", fixed=TRUE)

    errs <- datashield.errors()
    expect_length(errs, 0)
})

context("ds.foobar::arg::assign")
test_that("non existent assign foobarDS", {
    calltext <- call("fooBarDS")
    expect_error(datashield.assign(conns=ds.test_env$connections, symbol="new_obj", value=calltext))

    errs <- datashield.errors()
    expect_length(errs, 3)
    expect_length(errs$sim1, 1)
    expect_true(errs$sim1 %in% c("Command 'fooBarDS()' failed on 'sim1': No such DataSHIELD 'ASSIGN' method with name: fooBarDS", "[Client error: (400) Bad Request] No such DataSHIELD 'ASSIGN' method with name: fooBarDS", "Bad request: No such DataSHIELD 'ASSIGN' method with name: fooBarDS"))
    expect_length(errs$sim2, 1)
    expect_true(errs$sim2 %in% c("Command 'fooBarDS()' failed on 'sim2': No such DataSHIELD 'ASSIGN' method with name: fooBarDS", "[Client error: (400) Bad Request] No such DataSHIELD 'ASSIGN' method with name: fooBarDS", "Bad request: No such DataSHIELD 'ASSIGN' method with name: fooBarDS"))
    expect_length(errs$sim3, 1)
    expect_true(errs$sim3 %in% c("Command 'fooBarDS()' failed on 'sim3': No such DataSHIELD 'ASSIGN' method with name: fooBarDS", "[Client error: (400) Bad Request] No such DataSHIELD 'ASSIGN' method with name: fooBarDS", "Bad request: No such DataSHIELD 'ASSIGN' method with name: fooBarDS"))
})

#
# Tear down
#

context("ds.foobar::arg::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

context("ds.foobar::arg::done")
