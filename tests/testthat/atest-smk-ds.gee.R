#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

connect.studies.dataset.survival(list("id", "survtime", "female", "age.60"))

#
# Tests
#

context("ds.gee::smk")
test_that("simple gee", {
    expect_warning(res <- ds.gee(formula='survtime~1+female+age.60', family='binomial', data='D', corStructure='ar1', clusterID='id', startCoeff=c(-1, 1, 0)), "NAs introduced by coercion")

    print("2)======")
    print(res)
    print("2)======")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_equal(res$sim1[1], "0")
    expect_equal(res$sim1[2], "1")
    expect_length(res$sim2, 2)
    expect_equal(res$sim2[1], "0")
    expect_equal(res$sim2[2], "1")
    expect_length(res$sim3, 2)
    expect_equal(res$sim3[1], "0")
    expect_equal(res$sim3[2], "1")
})

#
# Done
#

disconnect.studies.dataset.survival()
