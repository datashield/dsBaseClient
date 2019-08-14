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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.colnames::smk")
test_that("simple colnames", {
    myvectors <- c("D$LAB_TSC", "D$LAB_TRIG")
    ds.dataFrame(x=myvectors, newobj="new_df")

    res <- ds.colnames("new_df")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_equal(res$sim1[1], "D$LAB_TSC")
    expect_equal(res$sim1[2], "D$LAB_TRIG")
    expect_length(res$sim2, 2)
    expect_equal(res$sim2[1], "D$LAB_TSC")
    expect_equal(res$sim2[2], "D$LAB_TRIG")
    expect_length(res$sim3, 2)
    expect_equal(res$sim3[1], "D$LAB_TSC")
    expect_equal(res$sim3[2], "D$LAB_TRIG")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
