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

# context("ds.matrixDimnames::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.matrixDimnames::smk")
test_that("simplest ds.matrixDimnames", {
    matrix <- c(-2, 1, 3, 0, -1, 1, 1, 2, 0)

    ds.matrix(mdata=matrix, nrows.scalar=3, ncols.scalar=3)

    dimnames <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))

    res <- ds.matrixDimnames("matrix.newobj", dimnames=dimnames)

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <matrixdimnames.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<matrixdimnames.newobj> appears valid in all sources")

    check.class<-ds.class("matrixdimnames.newobj",datasources=ds.test_env$connections)

    expect_length(check.class, 3)
    expect_true("matrix" %in% check.class$sim1)
    expect_true("matrix" %in% check.class$sim2)
    expect_true("matrix" %in% check.class$sim3)
})

#
# Tear down
#

# context("ds.matrixDimnames::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "matrix.newobj", "matrixdimnames.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.matrixDimnames::smk::done")
