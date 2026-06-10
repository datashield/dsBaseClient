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

# context("ds.matrixDet::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.matrixDet::smk")
test_that("simplest ds.matrixDet", {
    matrix <- c(-2, 1, 3, 0, -1, 1, 1, 2, 0)

    ds.matrix(mdata=matrix, nrows.scalar=3, ncols.scalar=3)
    res <- ds.matrixDet("matrix.newobj")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <matrixdet.newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<matrixdet.newobj> appears valid in all sources")

    check.class<-ds.class("matrixdet.newobj",datasources=ds.test_env$connections)

    expect_length(check.class, 3)
    expect_equal(check.class$sim1, "det")
    expect_equal(check.class$sim2, "det")
    expect_equal(check.class$sim3, "det")
})

#
# Tear down
#

# context("ds.matrixDet::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "matrix.newobj", "matrixdet.newobj"))
})

disconnect.studies.dataset.cnsim()

# context("ds.matrixDet::smk::done")
