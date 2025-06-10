#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.message::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})


#
# Tests
#

context("ds.message::smk")
test_that("not exists - request message", {
    message.res <- ds.message('Test')

    expect_length(message.res, 3)
    expect_equal(message.res$sim1, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)
    expect_equal(message.res$sim2, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)
    expect_equal(message.res$sim3, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)
})

test_that("exists - request message", {
    ds.list("D$LAB_TSC", "Test")

    message.res <- ds.message('Test')

    expect_length(message.res, 3)
    expect_equal(message.res$sim1, "ALL OK: there are no studysideMessage(s) on this datasource", fixed=TRUE)
    expect_equal(message.res$sim2, "ALL OK: there are no studysideMessage(s) on this datasource", fixed=TRUE)
    expect_equal(message.res$sim3, "ALL OK: there are no studysideMessage(s) on this datasource", fixed=TRUE)
})

test_that("partial - request message - conn 1", {
    ds.rm("TestP")

    ds.list("D$LAB_TSC", newobj="TestP", datasources=ds.test_env$connections[1])

    message.res <- ds.message('TestP')

    expect_length(message.res, 3)
    expect_equal(message.res$sim1, "ALL OK: there are no studysideMessage(s) on this datasource", fixed=TRUE)
    expect_equal(message.res$sim2, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)
    expect_equal(message.res$sim3, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)

    ds.rm("TestP")
})

test_that("partial - request message - conn 2", {
    ds.rm("TestP")

    ds.list("D$LAB_TSC", newobj="TestP", datasources=ds.test_env$connections[2])

    message.res <- ds.message('TestP')

    expect_length(message.res, 3)
    expect_equal(message.res$sim1, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)
    expect_equal(message.res$sim2, "ALL OK: there are no studysideMessage(s) on this datasource", fixed=TRUE)
    expect_equal(message.res$sim3, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)

    ds.rm("TestP")
})

test_that("partial - request message - conn 3", {
    ds.rm("TestP")

    ds.list("D$LAB_TSC", newobj="TestP", datasources=ds.test_env$connections[3])

    message.res <- ds.message('TestP')

    expect_length(message.res, 3)
    expect_equal(message.res$sim1, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)
    expect_equal(message.res$sim2, "Error: the object <message.object.name> does not exist in this datasource", fixed=TRUE)
    expect_equal(message.res$sim3, "ALL OK: there are no studysideMessage(s) on this datasource", fixed=TRUE)

    ds.rm("TestP")
})

#
# Done
#
context("ds.message::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "Test"))
})

disconnect.studies.dataset.cnsim()

context("ds.message::smk::done")
