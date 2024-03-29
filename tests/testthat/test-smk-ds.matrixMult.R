#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.matrixMult::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.matrixMult::smk")
test_that("simplest ds.matrixMult", {
    matrix1 <- c(-2, 1, 3, 0, -1, 1, 1, 2, 0)
    matrix2 <- c(-2, 1, 3, 1, -1, 1, 1, 2, 1)

    ds.matrix(mdata=matrix1, nrows.scalar=3, ncols.scalar=3, newobj="mat1")
    ds.matrix(mdata=matrix2, nrows.scalar=3, ncols.scalar=3, newobj="mat2")
    res <- ds.matrixMult("mat1", "mat2")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <matrixmult.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<matrixmult.newobj> appears valid in all sources")

    check.class<-ds.class("matrixmult.newobj",datasources=ds.test_env$connections)

    expect_length(check.class, 3)
    expect_true("matrix" %in% check.class$sim1)
    expect_true("matrix" %in% check.class$sim2)
    expect_true("matrix" %in% check.class$sim3)
})

#
# Tear down
#

context("ds.matrixMult::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "mat1", "mat2", "matrixmult.newobj"))
})

disconnect.studies.dataset.cnsim()

context("ds.matrixMult::smk::done")
