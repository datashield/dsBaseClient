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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

context("ds.listDisclosureSettings::smk::check results")
test_that("check results", {
    res <- ds.listDisclosureSettings()

    expect_equal(length(res$Opal.disclosure), 3)

    sim1.res <- res$Opal.disclosure.settings$sim1
    sim2.res <- res$Opal.disclosure.settings$sim2
    sim3.res <- res$Opal.disclosure.settings$sim3

    expect_equal(length(sim1.res), 9)
    expect_equal(sim1.res$nfilter.tab, 3)
    expect_equal(sim1.res$nfilter.subset, 3)
    expect_equal(sim1.res$nfilter.glm, 0.33)
    expect_equal(sim1.res$nfilter.string, 80)
    expect_equal(sim1.res$nfilter.stringShort, 20)
    expect_equal(sim1.res$nfilter.kNN, 3)
    expect_equal(sim1.res$nfilter.levels, 0.33)
    expect_equal(sim1.res$nfilter.noise, 0.25)
    expect_equal(sim1.res$nfilter.privacy.old, 5)

    expect_equal(length(sim2.res), 9)
    expect_equal(sim2.res$nfilter.tab, 3)
    expect_equal(sim2.res$nfilter.subset, 3)
    expect_equal(sim2.res$nfilter.glm, 0.33)
    expect_equal(sim2.res$nfilter.string, 80)
    expect_equal(sim2.res$nfilter.stringShort, 20)
    expect_equal(sim2.res$nfilter.kNN, 3)
    expect_equal(sim2.res$nfilter.levels, 0.33)
    expect_equal(sim2.res$nfilter.noise, 0.25)
    expect_equal(sim2.res$nfilter.privacy.old, 5)

    expect_equal(length(sim3.res), 9)
    expect_equal(sim3.res$nfilter.tab, 3)
    expect_equal(sim3.res$nfilter.subset, 3)
    expect_equal(sim3.res$nfilter.glm, 0.33)
    expect_equal(sim3.res$nfilter.string, 80)
    expect_equal(sim3.res$nfilter.stringShort, 20)
    expect_equal(sim3.res$nfilter.kNN, 3)
    expect_equal(sim3.res$nfilter.levels, 0.33)
    expect_equal(sim3.res$nfilter.noise, 0.25)
    expect_equal(sim3.res$nfilter.privacy.old, 5)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
