#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.asList::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.asList::smk::simple test")
test_that("simple test", {
    res <- ds.asList(x.name="D$GENDER")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_equal(res$sim1$return.message, "New object <aslist.newobj> created")
    expect_equal(res$sim1$class.of.newobj, "Class of <aslist.newobj> is 'list'")
    expect_length(res$sim2, 2)
    expect_equal(res$sim2$return.message, "New object <aslist.newobj> created")
    expect_equal(res$sim2$class.of.newobj, "Class of <aslist.newobj> is 'list'")
    expect_length(res$sim3, 2)
    expect_equal(res$sim3$return.message, "New object <aslist.newobj> created")
    expect_equal(res$sim3$class.of.newobj, "Class of <aslist.newobj> is 'list'")
})

#
# Done
#

# context("ds.asList::smk::shutdown")

test_that("stutdown", {
    ds_expect_variables(c("D", "aslist.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.asList::smk::done")
