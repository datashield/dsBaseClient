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

# context("ds.matrixDiag::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.matrixDiag::smk")
test_that("simplest ds.matrixDiag", {
    matrix <- c(0, 1, 2, 3, 4, 5)

    res <- ds.matrixDiag(matrix, aim="clientside.vector.2.matrix", newobj="matrix_diag")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <matrix_diag> has been created in all specified data sources")
    expect_equal(res$validity.check, "<matrix_diag> appears valid in all sources")

    check.class<-ds.class("matrix_diag")

    expect_length(check.class, 3)
    expect_true("matrix" %in% check.class$sim1)
    expect_true("matrix" %in% check.class$sim2)
    expect_true("matrix" %in% check.class$sim3)
})

# context("ds.matrixDiag::smk")
test_that("simplest ds.matrixDiag", {
    matrix <- c(0, 1, 2, 3, 4, 5)

    res <- ds.matrixDiag(matrix, aim="clientside.vector.2.matrix")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <matrixdiag.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<matrixdiag.newobj> appears valid in all sources")

    check.class<-ds.class("matrixdiag.newobj",datasources=ds.test_env$connections)

    expect_length(check.class, 3)
    expect_true("matrix" %in% check.class$sim1)
    expect_true("matrix" %in% check.class$sim2)
    expect_true("matrix" %in% check.class$sim3)
})

#
# Tear down
#

# context("ds.matrixDiag::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "matrix_diag", "matrixdiag.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.matrixDiag::smk::done")
