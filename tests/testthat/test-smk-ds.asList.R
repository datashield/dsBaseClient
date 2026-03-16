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
    expect_silent(ds.asList(x.name="D$GENDER"))

    res.class <- ds.class("aslist.newobj")
    expect_equal(res.class$sim1, "list")
    expect_equal(res.class$sim2, "list")
    expect_equal(res.class$sim3, "list")
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
