#-------------------------------------------------------------------------------
# Copyright (c) 2018-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.corTest::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.corTest::smk")

test_that("simple test", {
    res <- ds.corTest(x="D$survtime", y="D$time.id")

    expect_length(res, 3)
    expect_equal(class(res$survival1), "htest")
    expect_length(res$survival1, 9)
    expect_equal(res$survival1[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival1[[6]], "two.sided")
    expect_equal(res$survival1[[7]], "Pearson's product-moment correlation")
    expect_equal(res$survival1[[8]], "x.var and y.var")
    expect_length(res$survival1[[9]], 2)
    expect_equal(res$survival1[[9]][1], 0.0215, tolerance=1e6)
    expect_equal(res$survival1[[9]][2], 0.0972, tolerance=1e6)
    expect_equal(class(res$survival2), "htest")
    expect_length(res$survival2, 9)
    expect_equal(res$survival2[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival2[[6]], "two.sided")
    expect_equal(res$survival2[[7]], "Pearson's product-moment correlation")
    expect_equal(res$survival2[[8]], "x.var and y.var")
    expect_length(res$survival3[[9]], 2)
    expect_equal(res$survival3[[9]][1], 0.0215, tolerance=1e6)
    expect_equal(res$survival3[[9]][2], 0.0972, tolerance=1e6)
    expect_equal(class(res$survival3), "htest")
    expect_length(res$survival3, 9)
    expect_equal(res$survival3[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival3[[6]], "two.sided")
    expect_equal(res$survival3[[7]], "Pearson's product-moment correlation")
    expect_equal(res$survival3[[8]], "x.var and y.var")
    expect_length(res$survival3[[9]], 2)
    expect_equal(res$survival3[[9]][1], 0.0215, tolerance=1e6)
    expect_equal(res$survival3[[9]][2], 0.0972, tolerance=1e6)
})

#
# Done
#

context("ds.corTest::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

context("ds.corTest::smk::done")
