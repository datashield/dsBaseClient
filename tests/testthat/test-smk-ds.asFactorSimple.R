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

# context("ds.asFactorSimple::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

ds.asNumeric("D$time.id","TID")

# context("ds.asFactorSimple::smk::force.factor.levels")
test_that("with no force.factor.levels", {
    ds.asFactorSimple("TID", "TID.f1")

    res1 <- ds.class("TID.f1")
    res2 <- expect_warning(ds.table1D("TID.f1"), "'ds.table1D' is deprecated.\nUse 'ds.table' instead.", fixed = TRUE)

    expect_equal("factor", res1$`survival1`)
    expect_equal("factor", res1$`survival2`)
    expect_equal("factor", res1$`survival3`)
    expect_equal("All tables are valid!", res2$validity)
})

#
# Done
#

# context("ds.asFactorSimple::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "TID", "TID.f1"))
})

disconnect.studies.dataset.survival()

# context("ds.asFactorSimple::smk::done")
