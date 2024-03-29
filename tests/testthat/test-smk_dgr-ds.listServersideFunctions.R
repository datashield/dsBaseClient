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

context("ds.listServersideFunctions::smk_dgr::setup")

require(dsDangerClient)

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.listServersideFunctions::smk_dgr::check results")
test_that("check results", {
    assign.functions <- factor(c(
    ))
    aggregate.functions <- factor(c(
        "DANGERc2sMATDFDS",
        "DANGERlistcodeDS",
        "DANGERdfEXTRACTDS",
        "DANGERpassPARSERDS",
        "DANGERprintDS",
        "DANGERplotDS",
        "DANGERsearchDS",
        "DANGERseedEXTRACTDS",
        "DANGERvarsEXTRACTDS"
    ))

    expect_warning(res <- ds.listServersideFunctions(), "'ds.listServersideFunctions' is deprecated.", fixed=TRUE)

    expect_length(res, 2)
    expect_length(res$serverside.assign.functions, 7)
    expect_length(res$serverside.aggregate.functions, 7)

    sim1.assign.res    <- subset(res$serverside.assign.functions, server == 'sim1', c('name'))
    sim1.aggregate.res <- subset(res$serverside.aggregate.functions, server == 'sim1', c('name'))
    sim2.assign.res    <- subset(res$serverside.assign.functions, server == 'sim2', c('name'))
    sim2.aggregate.res <- subset(res$serverside.aggregate.functions, server == 'sim2', c('name'))
    sim3.assign.res    <- subset(res$serverside.assign.functions, server == 'sim3', c('name'))
    sim3.aggregate.res <- subset(res$serverside.aggregate.functions, server == 'sim3', c('name'))

    for (func.name in assign.functions)
        expect_true(func.name %in% sim1.assign.res$name, info = func.name)
    for (func.name in aggregate.functions)
        expect_true(func.name %in% sim1.aggregate.res$name, info = func.name)
    for (func.name in assign.functions)
        expect_true(func.name %in% sim2.assign.res$name, info = func.name)
    for (func.name in aggregate.functions)
        expect_true(func.name %in% sim2.aggregate.res$name, info = func.name)
    for (func.name in assign.functions)
        expect_true(func.name %in% sim3.assign.res$name, info = func.name)
    for (func.name in aggregate.functions)
        expect_true(func.name %in% sim3.aggregate.res$name, info = func.name)
})

#
# Done
#

context("ds.listServersideFunctions::smk_dgr::shtudown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

context("ds.listServersideFunctions::smk_dgr::done")
