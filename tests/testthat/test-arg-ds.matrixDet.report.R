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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.matrixDet.report::arg::test errors")

test_that("ds.matixDet.report erros", {
    res <- ds.matrixDet.report(M1=NULL)

    expect_equal(res, "Error: Please provide the name of the matrix representing M1")
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
