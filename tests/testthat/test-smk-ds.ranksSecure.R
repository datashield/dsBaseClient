#-------------------------------------------------------------------------------
# Copyright (c) 2019-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.ranksSecure::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TRIG", "LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.ranksSecure::smk::continous, without NAs, all positive")
test_that("continous, without NAs, all positive", {
    res.cc <- ds.completeCases("D$LAB_TSC", newobj="CC_LAB_TSC")
    expect_equal(res.cc$validity.check, "<CC_LAB_TSC> appears valid in all sources")

    res.num.na <- ds.numNA("CC_LAB_TSC")
    expect_length(res.num.na, 3)
    expect_equal(res.num.na$sim1, 0)
    expect_equal(res.num.na$sim2, 0)
    expect_equal(res.num.na$sim3, 0)

    res <- ds.ranksSecure("CC_LAB_TSC")

    expect_length(res, 1)
    expect_length(class(res), 1)
    expect_true(all(class(res) %in% c('list')))
    expect_length(names(res), 1)
    expect_true(all(names(res) %in% c('final.quantile.df')))

    expect_length(res$final.quantile.df, 1)
    expect_length(class(res$final.quantile.df), 1)
    expect_true(all(class(res$final.quantile.df) %in% c('list')))
    expect_length(names(res$final.quantile.df), 1)
    expect_true(all(names(res$final.quantile.df) %in% c('final.quantile.df')))

    fq.df <- res$final.quantile.df$final.quantile.df

    expect_length(fq.df, 2)
    expect_length(class(fq.df), 1)
    expect_true(all(class(fq.df) %in% c('data.frame')))
    expect_length(names(fq.df), 2)
    expect_true(all(names(fq.df) %in% c('evaluation.quantiles', 'final.quantile.vector')))

    eval.quant  <- fq.df$evaluation.quantiles
    final.quant <- fq.df$final.quantile.vector

    expect_length(eval.quant, 15)
    expect_length(class(eval.quant), 1)
    expect_true(all(class(eval.quant) %in% c('numeric')))

    expect_length(final.quant, 15)
    expect_length(class(final.quant), 1)
    expect_true(all(class(final.quant) %in% c('numeric')))
})

context("ds.ranksSecure::smk::continous, without NAs, with negative")
test_that("continous, without NAs, with negative", {
    res.cc <- ds.completeCases("D$LAB_TRIG", newobj="CC_LAB_TRIG")
    expect_equal(res.cc$validity.check, "<CC_LAB_TRIG> appears valid in all sources")

    res.num.na <- ds.numNA("CC_LAB_TRIG")
    expect_length(res.num.na, 3)
    expect_equal(res.num.na$sim1, 0)
    expect_equal(res.num.na$sim2, 0)
    expect_equal(res.num.na$sim3, 0)

    res <- ds.ranksSecure("CC_LAB_TRIG")

    expect_length(res, 1)
    expect_length(class(res), 1)
    expect_true(all(class(res) %in% c('list')))
    expect_length(names(res), 1)
    expect_true(all(names(res) %in% c('final.quantile.df')))

    expect_length(res$final.quantile.df, 1)
    expect_length(class(res$final.quantile.df), 1)
    expect_true(all(class(res$final.quantile.df) %in% c('list')))
    expect_length(names(res$final.quantile.df), 1)
    expect_true(all(names(res$final.quantile.df) %in% c('final.quantile.df')))

    fq.df <- res$final.quantile.df$final.quantile.df

    expect_length(fq.df, 2)
    expect_length(class(fq.df), 1)
    expect_true(all(class(fq.df) %in% c('data.frame')))
    expect_length(names(fq.df), 2)
    expect_true(all(names(fq.df) %in% c('evaluation.quantiles', 'final.quantile.vector')))

    eval.quant  <- fq.df$evaluation.quantiles
    final.quant <- fq.df$final.quantile.vector

    expect_length(eval.quant, 15)
    expect_length(class(eval.quant), 1)
    expect_true(all(class(eval.quant) %in% c('numeric')))

    expect_length(final.quant, 15)
    expect_length(class(final.quant), 1)
    expect_true(all(class(final.quant) %in% c('numeric')))
})

context("ds.ranksSecure::smk::continous, with NAs, all positive")
test_that("continous, with NAs, all positive", {
    res <- ds.ranksSecure("D$LAB_TSC")

    expect_length(res, 1)
    expect_length(class(res), 1)
    expect_true(all(class(res) %in% c('list')))
    expect_length(names(res), 1)
    expect_true(all(names(res) %in% c('final.quantile.df')))

    expect_length(res$final.quantile.df, 1)
    expect_length(class(res$final.quantile.df), 1)
    expect_true(all(class(res$final.quantile.df) %in% c('list')))
    expect_length(names(res$final.quantile.df), 1)
    expect_true(all(names(res$final.quantile.df) %in% c('final.quantile.df')))

    fq.df <- res$final.quantile.df$final.quantile.df

    expect_length(fq.df, 2)
    expect_length(class(fq.df), 1)
    expect_true(all(class(fq.df) %in% c('data.frame')))
    expect_length(names(fq.df), 2)
    expect_true(all(names(fq.df) %in% c('evaluation.quantiles', 'final.quantile.vector')))

    eval.quant  <- fq.df$evaluation.quantiles
    final.quant <- fq.df$final.quantile.vector

    expect_length(eval.quant, 15)
    expect_length(class(eval.quant), 1)
    expect_true(all(class(eval.quant) %in% c('numeric')))

    expect_length(final.quant, 15)
    expect_length(class(final.quant), 1)
    expect_true(all(class(final.quant) %in% c('numeric')))
})

context("ds.ranksSecure::smk::continous, with NAs, with negative")
test_that("continous, with NAs, with negative", {
    res <- ds.ranksSecure("D$LAB_TRIG")

    expect_length(res, 1)
    expect_length(class(res), 1)
    expect_true(all(class(res) %in% c('list')))
    expect_length(names(res), 1)
    expect_true(all(names(res) %in% c('final.quantile.df')))

    expect_length(res$final.quantile.df, 1)
    expect_length(class(res$final.quantile.df), 1)
    expect_true(all(class(res$final.quantile.df) %in% c('list')))
    expect_length(names(res$final.quantile.df), 1)
    expect_true(all(names(res$final.quantile.df) %in% c('final.quantile.df')))

    fq.df <- res$final.quantile.df$final.quantile.df

    expect_length(fq.df, 2)
    expect_length(class(fq.df), 1)
    expect_true(all(class(fq.df) %in% c('data.frame')))
    expect_length(names(fq.df), 2)
    expect_true(all(names(fq.df) %in% c('evaluation.quantiles', 'final.quantile.vector')))

    eval.quant  <- fq.df$evaluation.quantiles
    final.quant <- fq.df$final.quantile.vector

    expect_length(eval.quant, 15)
    expect_length(class(eval.quant), 1)
    expect_true(all(class(eval.quant) %in% c('numeric')))

    expect_length(final.quant, 15)
    expect_length(class(final.quant), 1)
    expect_true(all(class(final.quant) %in% c('numeric')))
})


#
# Done
#

context("ds.ranksSecure::smk::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "CC_LAB_TSC", "CC_LAB_TRIG",
                          "final.quantile.df", "blackbox.output.df", "blackbox.ranks.df", "global.bounds.df", "global.ranks.quantiles.df", "input.mean.sd.df", "input.ranks.sd.df",
                          "main.ranks.df", "min.max.df", "numstudies.df", "sR4.df", "sR5.df", "summary.ranks.df", "testvar.ranks"))
})

disconnect.studies.dataset.cnsim()

context("ds.ranksSecure::smk::done")
