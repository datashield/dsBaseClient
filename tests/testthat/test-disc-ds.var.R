#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnuy.org/licenses/>.
#-------------------------------------------------------------------------------

#
# Set up
#

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

# context("ds.var::disc::type=both")
test_that("var values [both]", {
#    myvectors <- c("D$LAB_TSC", "D$LAB_TRIG")
#    ds.dataFrame(x=myvectors, newobj="unsubset_df")
#
#    ds.dataFrameSubset(df.name="unsubset_df", V1.name="D$LAB_TSC", V2.name="D$LAB_TRIG", Boolean.operator=">", newobj="subset_df")
#
#    length_res <- ds.length("subset_df")
#    
#    expect_length(length_res, 4)
#    expect_equal(length_res$`length of subset_df in sim1`, 2)
#    expect_equal(length_res$`length of subset_df in sim2`, 2)
#    expect_equal(length_res$`length of subset_df in sim3`, 2)
#    expect_equal(length_res$`total length of subset_df in all studies combined`, 6)
#
#    var.res <- ds.var(x="subset_df$LAB_TSC", type="both")
#
#    expect_length(var.res, 4)
#    expect_length(var.res$Variance.by.Study, 12)
#    expect_true(is.na(var.res$Variance.by.Study[1]))
#    expect_true(is.na(var.res$Variance.by.Study[2]))
#    expect_true(is.na(var.res$Variance.by.Study[3]))
#    expect_equal(var.res$Variance.by.Study[4], 0)
#    expect_equal(var.res$Variance.by.Study[5], 0)
#    expect_equal(var.res$Variance.by.Study[6], 0)
#    expect_equal(var.res$Variance.by.Study[7], 0)
#    expect_equal(var.res$Variance.by.Study[8], 0)
#    expect_equal(var.res$Variance.by.Study[9], 0)
#    expect_equal(var.res$Variance.by.Study[10], 0)
#    expect_equal(var.res$Variance.by.Study[11], 0)
#    expect_equal(var.res$Variance.by.Study[12], 0)
#    expect_length(var.res$Global.Variance, 4)
#    expect_true(is.na(var.res$Global.Variance[1]))
#    expect_equal(var.res$Global.Variance[2], 0)
#    expect_equal(var.res$Global.Variance[3], 0)
#    expect_equal(var.res$Global.Variance[4], 0)
#    expect_equal(var.res$Nstudies, 3)
#    expect_length(var.res$ValidityMessage, 3)
#    expect_equal(var.res$ValidityMessage[1], "FAILED: Nvalid less than nfilter.tab")
#    expect_equal(var.res$ValidityMessage[2], "FAILED: Nvalid less than nfilter.tab")
#    expect_equal(var.res$ValidityMessage[3], "FAILED: Nvalid less than nfilter.tab")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
