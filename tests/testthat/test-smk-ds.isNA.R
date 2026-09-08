#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.isNA::smk::setup")

connect.studies.dataset.cnsim(list("LAB_HDL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.isNA::smk")
res <- ds.isNA(x='D$LAB_HDL')
test_that("isNA", {
    expect_false(res$sim1)
    expect_false(res$sim1)
    expect_false(res$sim1)
})

test_that("isNA, wrong input class returns a server error", {
    ds.asList("D$LAB_HDL", newobj="not_a_vector")

    expect_error(ds.isNA(x="not_a_vector"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed=TRUE)
    res.errors <- DSI::datashield.errors()
    expect_match(res.errors[[1]], "must be of type character, factor, integer, logical, numeric, data.frame or matrix")

    ds.rm("not_a_vector")
})

#
# Tear down
#

# context("ds.isNA::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.isNA::smk::done")

