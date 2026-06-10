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

# context("ds.abs::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.abs::smk")
test_that("simple c", {
    res <- ds.abs("D$LAB_TSC", newobj = "abs.newobj")

    expect_true(is.null(res))

    res.length <- ds.length("abs.newobj")

    expect_length(res.length, 4)
    expect_equal(res.length$`length of abs.newobj in sim1`, 2163)
    expect_equal(res.length$`length of abs.newobj in sim2`, 3088)
    expect_equal(res.length$`length of abs.newobj in sim3`, 4128)
    expect_equal(res.length$`total length of abs.newobj in all studies combined`, 9379)

    res.class <- ds.class("abs.newobj")

    expect_length(res.class, 3)
    expect_equal(res.class$sim1, "numeric")
    expect_equal(res.class$sim2, "numeric")
    expect_equal(res.class$sim3, "numeric")
})

#
# Done
#

# context("ds.abs::smk::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "abs.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.abs::smk::done")
