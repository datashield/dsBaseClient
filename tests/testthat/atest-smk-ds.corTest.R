#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.survival(list("survtime", "time.id"))

#
# Tests
#

context("ds.corTest::smk")

test_that("simple test", {
    res <- ds.corTest(x="D$survtime", y="D$time.id")

    expect_length(res, 5)
    expect_equal(class(res$`Number of missing values in each variable`), "matrix")
    expect_equal(class(res$`Number of missing values pairwise`), "matrix")
    expect_equal(class(res$`Correlation Matrix`), "matrix")
    expect_equal(class(res$`Number of complete cases used`), "matrix")
    expect_length(res$`Error message`, 3)
    expect_equal(res$`Error message`[[1]], NA)
    expect_equal(res$`Error message`[[2]], NA)
    expect_equal(res$`Error message`[[3]], NA)
})

#
# Done
#

disconnect.studies.dataset.survival()
