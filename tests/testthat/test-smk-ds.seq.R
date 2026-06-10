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

# context("ds.seq::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.seq::smk")
test_that("simplest ds.seq", {
    seq.res <- ds.seq(FROM.value.char="1", BY.value.char="1", LENGTH.OUT.value.char="10", ALONG.WITH.name=NULL, newobj="obj1")

    expect_true(length(seq.res) == 2)
    expect_equal(seq.res[[1]], "A data object <obj1> has been created in all specified data sources")
    expect_equal(seq.res[[2]], "<obj1> appears valid in all sources")
})

test_that("simplest ds.seq", {
    seq.res <- ds.seq(FROM.value.char="1", BY.value.char="1", LENGTH.OUT.value.char=NULL, ALONG.WITH.name="D$LAB_TSC", newobj="obj2")

    expect_true(length(seq.res) == 2)
    expect_equal(seq.res[[1]], "A data object <obj2> has been created in all specified data sources")
    expect_equal(seq.res[[2]], "<obj2> appears valid in all sources")
})

#
# Tear down
#

# context("ds.seq::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "obj1", "obj2"))
})

disconnect.studies.dataset.cnsim()

# context("ds.seq::smk::done")
