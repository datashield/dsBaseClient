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

# context("ds.asFactor::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

ds.asNumeric("D$time.id","TID")

# context("ds.asFactor::smk::force.factor.levels")
test_that("with no force.factor.levels", {
    ds.asFactor("TID", "TID.f1")

    res1 <- ds.class("TID.f1")
    res2 <- expect_warning(ds.table1D("TID.f1"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

test_that("with forced.factor.levels of 1:6", {
    ds.asFactor("TID", "TID.f2", forced.factor.levels=1:6)

    res1 <- ds.class("TID.f2")
    res2 <- expect_warning(ds.table1D("TID.f2"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

test_that("with force.factor.levels of 0:10", {
    ds.asFactor("TID", "TID.f3", forced.factor.levels=0:10)

    res1 <- ds.class("TID.f3")
    res2 <- expect_warning(ds.table1D("TID.f3"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

test_that("with force.factor.levels of 2:3", {
    ds.asFactor("TID", "TID.f4", forced.factor.levels=2:3)

    res1 <- ds.class("TID.f4")
    res2 <- expect_warning(ds.table1D("TID.f4"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

test_that("with force.factor.levels of c(1,2,3,4,'a','h',5)", {
    ds.asFactor("TID", "TID.f5", forced.factor.levels=c(1,2,3,4,'a','h',5))

    res1 <- ds.class("TID.f5")
    res2 <- expect_warning(ds.table1D("TID.f5"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

# context("ds.asFactor::smk::fixed.dummy.vars")
test_that("with fixed.dummy.vars of TRUE", {
    ds.asFactor("TID", "TID.mat1", fixed.dummy.vars=TRUE)

    res <- ds.class("TID.mat1")

    expect_true("matrix" %in% res$`survival1`)
    expect_true("matrix" %in% res$`survival2`)
    expect_true("matrix" %in% res$`survival3`)
})

test_that("with fixed.dummy.vars of TRUE and baseline.level of 6", {
    ds.asFactor("TID", "TID.mat6", fixed.dummy.vars=TRUE,baseline.level=6)

    res <- ds.class("TID.mat6")

    expect_true("matrix" %in% res$`survival1`)
    expect_true("matrix" %in% res$`survival2`)
    expect_true("matrix" %in% res$`survival3`)
})

#
# Done
#

# context("ds.asFactor::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "TID", "TID.f1", "TID.f2", "TID.f3", "TID.f4", "TID.f5", "TID.mat1", "TID.mat6"))
})

disconnect.studies.dataset.survival()

# context("ds.asFactor::smk::done")
