#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnurg/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

# context("ds.matrixDet.report::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.matrixDet.report::smk")
test_that("simplest ds.matrixDet.report", {
    matrix <- c(-2, 1, 3, 0, -1, 1, 1, 2, 0)

    ds.matrix(mdata=matrix, nrows.scalar=3, ncols.scalar=3)
    res <- ds.matrixDet.report("matrix.newobj", logarithm=NULL)

    expect_length(res, 1)
    expect_length(res$matrix.determinant, 3)
    expect_length(res$matrix.determinant$sim1, 1)
    expect_length(res$matrix.determinant$sim1$matrix.determinant, 2)
    expect_equal(class(res$matrix.determinant$sim1$matrix.determinant$modulus), "numeric")
    expect_equal(res$matrix.determinant$sim1$matrix.determinant$sign, 1)
    expect_length(res$matrix.determinant$sim2, 1)
    expect_length(res$matrix.determinant$sim2$matrix.determinant, 2)
    expect_equal(class(res$matrix.determinant$sim2$matrix.determinant$modulus), "numeric")
    expect_equal(res$matrix.determinant$sim2$matrix.determinant$sign, 1)
    expect_length(res$matrix.determinant$sim3, 1)
    expect_length(res$matrix.determinant$sim3$matrix.determinant, 2)
    expect_equal(class(res$matrix.determinant$sim3$matrix.determinant$modulus), "numeric")
    expect_equal(res$matrix.determinant$sim3$matrix.determinant$sign, 1)
})

#
# Tear down
#

# context("ds.matrixDet.report::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "matrix.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.matrixDet.report::smk::done")
