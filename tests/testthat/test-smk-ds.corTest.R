#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.corTest::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.corTest::smk::pearson")

test_that("simple test", {
    res <- ds.corTest(x="D$survtime", y="D$time.id", method="pearson", conf.level=0.95)

    expect_length(res, 3)
    expect_equal(class(res$survival1), "list")
    expect_length(res$survival1, 2)
    expect_equal(class(res$survival1$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival1$`Number of pairwise complete cases`, 2038)
    expect_equal(res$survival1$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival1$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival1$`Correlation test`[[7]], "Pearson's product-moment correlation")
    expect_equal(res$survival1$`Correlation test`[[8]], "x.var and y.var")
    expect_equal(class(res$survival2), "list")
    expect_length(res$survival2, 2)
    expect_equal(class(res$survival2$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival2$`Number of pairwise complete cases`, 1637)
    expect_equal(res$survival2$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival2$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival2$`Correlation test`[[7]], "Pearson's product-moment correlation")
    expect_equal(res$survival2$`Correlation test`[[8]], "x.var and y.var")
    expect_equal(class(res$survival3), "list")
    expect_length(res$survival3, 2)
    expect_equal(class(res$survival3$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival3$`Number of pairwise complete cases`, 2662)
    expect_equal(res$survival3$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival3$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival3$`Correlation test`[[7]], "Pearson's product-moment correlation")
    expect_equal(res$survival3$`Correlation test`[[8]], "x.var and y.var")
})

# context("ds.corTest::smk::kendall")

test_that("simple test", {
    res <- ds.corTest(x="D$survtime", y="D$time.id", method="kendall", conf.level=0.95)
    
    expect_length(res, 3)
    expect_equal(class(res$survival1), "list")
    expect_length(res$survival1, 2)
    expect_equal(class(res$survival1$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival1$`Number of pairwise complete cases`, 2038)
    expect_equal(res$survival1$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival1$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival1$`Correlation test`[[7]], "Kendall's rank correlation tau")
    expect_equal(res$survival1$`Correlation test`[[8]], "x.var and y.var")
    expect_equal(class(res$survival2), "list")
    expect_length(res$survival2, 2)
    expect_equal(class(res$survival2$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival2$`Number of pairwise complete cases`, 1637)
    expect_equal(res$survival2$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival2$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival2$`Correlation test`[[7]], "Kendall's rank correlation tau")
    expect_equal(res$survival2$`Correlation test`[[8]], "x.var and y.var")
    expect_equal(class(res$survival3), "list")
    expect_length(res$survival3, 2)
    expect_equal(class(res$survival3$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival3$`Number of pairwise complete cases`, 2662)
    expect_equal(res$survival3$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival3$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival3$`Correlation test`[[7]], "Kendall's rank correlation tau")
    expect_equal(res$survival3$`Correlation test`[[8]], "x.var and y.var")
})

# context("ds.corTest::smk::spearman")

test_that("simple test", {
    res <- ds.corTest(x="D$survtime", y="D$time.id", method="spearman", conf.level=0.95)
    
    expect_length(res, 3)
    expect_equal(class(res$survival1), "list")
    expect_length(res$survival1, 2)
    expect_equal(class(res$survival1$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival1$`Number of pairwise complete cases`, 2038)
    expect_equal(res$survival1$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival1$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival1$`Correlation test`[[7]], "Spearman's rank correlation rho")
    expect_equal(res$survival1$`Correlation test`[[8]], "x.var and y.var")
    expect_equal(class(res$survival2), "list")
    expect_length(res$survival2, 2)
    expect_equal(class(res$survival2$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival2$`Number of pairwise complete cases`, 1637)
    expect_equal(res$survival2$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival2$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival2$`Correlation test`[[7]], "Spearman's rank correlation rho")
    expect_equal(res$survival2$`Correlation test`[[8]], "x.var and y.var")
    expect_equal(class(res$survival3), "list")
    expect_length(res$survival3, 2)
    expect_equal(class(res$survival3$`Number of pairwise complete cases`), "integer")
    expect_equal(res$survival3$`Number of pairwise complete cases`, 2662)
    expect_equal(res$survival3$`Correlation test`[[3]], 0.00216, tolerance=1e6)
    expect_equal(res$survival3$`Correlation test`[[6]], "two.sided")
    expect_equal(res$survival3$`Correlation test`[[7]], "Spearman's rank correlation rho")
    expect_equal(res$survival3$`Correlation test`[[8]], "x.var and y.var")
})

#
# Done
#

# context("ds.corTest::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

# context("ds.corTest::smk::done")
