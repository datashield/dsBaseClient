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

context("ds.listClientsideFunctions::smk_dgr::setup")

require(dsDangerClient)

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.listClientsideFunctions::smk_dgr::check results")
test_that("check results", {
    output <- list(
        "ds.DANGERc2sMATDF",
        "ds.DANGERdfEXTRACT",
        "ds.DANGERlistcode",
        "ds.DANGERpassPARSER",
        "ds.DANGERplot",
        "ds.DANGERprint",
        "ds.DANGERsearch",
        "ds.DANGERseedEXTRACT",
        "ds.DANGERvarsEXTRACT"
    )

    res <- ls(pos="package:dsDangerClient")

    for (func.name in output) {
        expect_true(func.name %in% res, info = func.name)
    }
})

#
# Shutdown
#

context("ds.listClientsideFunctions::smk_dgr::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

#
# Done
#

context("ds.listClientsideFunctions::smk_dgr::done")
