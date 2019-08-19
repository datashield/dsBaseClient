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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "DIS_CVA", "DIS_AMI"))

#
# Tests
#

context("ds.merge::smk")
test_that("simple test", {
    spec_vectors_1 <- c('D$LAB_TSC', 'D$LAB_HDL')
    spec_vectors_2 <- c('D$LAB_TSC', 'D$DIS_AMI')
    ds.dataFrame(x=spec_vectors_1, newobj="test_1_df")
    ds.dataFrame(x=spec_vectors_2, newobj="test_2_df")

    res <- ds.merge(x.name="test_1_df", y.name="test_2_df", by.x.names="D$LAB_TSC", by.y.names="D$LAB_TSC", newobj="merge_newobj")

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <merge_newobj> has been created in all specified data sources")
    expect_equal(res$validity.check, "<merge_newobj> appears valid in all sources")

    class.res <- ds.class("merge_newobj", datasources=ds.test_env$connection.opal)

    expect_length(class.res, 3)
    expect_equal(class.res$sim1, "data.frame")
    expect_equal(class.res$sim2, "data.frame")
    expect_equal(class.res$sim3, "data.frame")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
