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

# context("ds.matrix::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.matrix::smk")
test_that("simplest ds.matrix", {
    matrix <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

    res <- ds.matrix(mdata=matrix, nrows.scalar=3, ncols.scalar=4)

    expect_length(res, 2)
    expect_equal(res[[1]], "A data object <matrix.newobj> has been created in all specified data sources")
    expect_equal(res[[2]], "<matrix.newobj> appears valid in all sources")

    check.class<-ds.class("matrix.newobj",datasources=ds.test_env$connections)

    expect_length(check.class, 3)
    expect_true("matrix" %in% check.class$sim1)
    expect_true("matrix" %in% check.class$sim2)
    expect_true("matrix" %in% check.class$sim3)
})

#
# Tear down
#

# context("ds.matrix::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "matrix.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.matrix::smk::done")
