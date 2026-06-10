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

# context("ds.rbind::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female", "age.60"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.rbind::smk")
test_that("simple test", {
    res <- ds.rbind(c("D$survtime", "D$time.id", "D$female", "D$age.60"), newobj="rbind_newobj")

    expect_equal(res$is.object.created, "A data object <rbind_newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<rbind_newobj> appears valid in all sources")

    res1 <- ds.class("rbind_newobj")

    expect_true("matrix" %in% res1$survival1)
    expect_true("matrix" %in% res1$survival2)
    expect_true("matrix" %in% res1$survival3)
})

#
# Done
#

# context("ds.rbind::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "rbind_newobj"))
})

disconnect.studies.dataset.survival()

# context("ds.rbind::smk::done")
