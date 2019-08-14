#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

context("ds.matrixTranspose::smk")
test_that("simplest ds.matrixTranspose", {
    matrix <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

    ds.matrix(mdata=matrix, nrows.scalar=3, ncols.scalar=4)
    res <- ds.matrixTranspose("new_matrix")

    expect_length(res, 2)
    expect_equal(res[[1]], "A data object <new_matrix_transposed> has been created in all specified data sources")
    expect_equal(res[[2]], "<new_matrix_transposed> appears valid in all sources")

    check.class<-ds.class("new_matrix_transposed",datasources=ds.test_env$connection.opal)

    expect_length(check.class, 3)
    expect_equal(check.class$sim1, "matrix")
    expect_equal(check.class$sim2, "matrix")
    expect_equal(check.class$sim3, "matrix")
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
