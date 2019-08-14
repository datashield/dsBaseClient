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

context("ds.isValid::smk")
test_that("isValid", {
    res1 <- ds.isValid(x='D$LAB_TSC')

    expect_length(res1, 3)
    expect_length(res1$sim1, 1)
    expect_true(res1$sim1)
    expect_length(res1$sim2, 1)
    expect_true(res1$sim2)
    expect_length(res1$sim3, 1)
    expect_true(res1$sim3)

#    myvectors <- c("D$LAB_TSC", "D$LAB_TRIG")
#    ds.dataFrame(x=myvectors, newobj="unsubset_df")
#    ds.dataFrameSubset(df.name="unsubset_df", V1.name="D$LAB_TSC", V2.name="D$LAB_TRIG", Boolean.operator=">", newobj="subset_df")

#    res2 <- ds.isValid(x="subset_df")

#    expect_length(res2, 3)
#    expect_length(res2$sim1, 1)
#    expect_false(res2$sim1)
#    expect_length(res2$sim2, 1)
#    expect_false(res2$sim2)
#    expect_length(res2$sim3, 1)
#    expect_false(res2$sim3)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
