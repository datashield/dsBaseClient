#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2019 University of Newcastle upon Tyne. All rights reserved.
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

# context("dsBetaTestClient::ds.matrixDet.report::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.matrixDet.report::smk")
test_that("simplest ds.matrixDet.report", {
    matrix <- c(-2, 1, 3, 0, -1, 1, 1, 2, 0)

    ds.matrix(mdata=matrix, nrows.scalar=3, ncols.scalar=3)
    res <- ds.matrixDet.report("new_matrix", logarithm=NULL)

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

# context("dsBetaTestClient::ds.matrixDet.report::smk done")
