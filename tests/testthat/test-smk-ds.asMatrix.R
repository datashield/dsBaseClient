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

# context("ds.asMatrix::smk::setup")

connect.studies.dataset.cnsim(list("GENDER"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.asMatrix::smk::simple test")
test_that("simple test", {
    ds.asMatrix(x.name="D$GENDER")

    res.class <- ds.class("asmatrix.newobj")
    expect_true("matrix" %in% res.class$sim1)
    expect_true("matrix" %in% res.class$sim2)
    expect_true("matrix" %in% res.class$sim3)
})

#
# Done
#

# context("ds.asMatrix::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "asmatrix.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.asMatrix::smk::done")
