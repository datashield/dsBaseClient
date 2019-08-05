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

# context("dsBetaTestClient::ds.matrixDet::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC"))

#
# Tests
#

context("ds.matrixDet::smk")
test_that("simplest ds.matrixDet", {
    matrix <- c(-2, 1, 3, 0, -1, 1, 1, 2, 0)

    ds.matrix(mdata=matrix, nrows.scalar=3, ncols.scalar=3)
    res <- ds.matrixDet("new_matrix")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <new_matrix_det> has been created in all specified data sources")
    expect_equal(res$validity.check, "<new_matrix_det> appears valid in all sources")

    check.class<-ds.class("new_matrix_det",datasources=ds.test_env$connection.opal)

    expect_length(check.class, 3)
    expect_equal(check.class$sim1, "det")
    expect_equal(check.class$sim2, "det")
    expect_equal(check.class$sim3, "det")
})

#
# Tear down
#

# context("dsBetaTestClient::ds.matrixDet::smk done")
