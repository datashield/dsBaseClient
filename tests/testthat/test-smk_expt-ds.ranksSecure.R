#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.ranksSecure::smk_expt::setup")

connect.studies.dataset.cnsim(list("LAB_TRIG", "LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.ranksSecure::smk_expt::continous, without NAs, all positive")
test_that("continous, without NAs, all positive", {
    res.cc <- ds.completeCases("D$LAB_TSC", newobj="CC_LAB_TSC")
    expect_equal(res.cc$validity.check, "<CC_LAB_TSC> appears valid in all sources")

    res.num.na <- ds.numNA("CC_LAB_TSC")
    expect_length(res.num.na, 3)
    expect_equal(res.num.na$sim1, 0)
    expect_equal(res.num.na$sim2, 0)
    expect_equal(res.num.na$sim3, 0)

    res <- ds.ranksSecure("CC_LAB_TSC")

    expect_equal_to_reference(res, 'smk_expt-results/ds.ranksSecure-cont.rds')
})

# context("ds.ranksSecure::smk_expt::continous, without NAs, with negative")
test_that("continous, without NAs, with negative", {
    res.cc <- ds.completeCases("D$LAB_TRIG", newobj="CC_LAB_TRIG")
    expect_equal(res.cc$validity.check, "<CC_LAB_TRIG> appears valid in all sources")

    res.num.na <- ds.numNA("CC_LAB_TRIG")
    expect_length(res.num.na, 3)
    expect_equal(res.num.na$sim1, 0)
    expect_equal(res.num.na$sim2, 0)
    expect_equal(res.num.na$sim3, 0)

    res <- ds.ranksSecure("CC_LAB_TRIG")

    expect_equal_to_reference(res, 'smk_expt-results/ds.ranksSecure-cont_neg.rds')
})

# context("ds.ranksSecure::smk_expt::continous, with NAs, all positive")
test_that("continous, with NAs, all positive", {
    res <- ds.ranksSecure("D$LAB_TSC")

    expect_equal_to_reference(res, 'smk_expt-results/ds.ranksSecure-cont_nas.rds')
})

# context("ds.ranksSecure::smk_expt::continous, with NAs, with negative")
test_that("continous, with NAs, with negative", {
    res <- ds.ranksSecure("D$LAB_TRIG")

    expect_equal_to_reference(res, 'smk_expt-results/ds.ranksSecure-cont_nas_neg.rds')
})

#
# Done
#

# context("ds.ranksSecure::smk_expt::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "CC_LAB_TSC", "CC_LAB_TRIG", "final.quantile.df", "summary.ranks.df", "testvar.ranks"))
})

disconnect.studies.dataset.cnsim()

# context("ds.ranksSecure::smk_expt::done")
