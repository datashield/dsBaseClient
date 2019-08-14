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

connect.studies.dataset.cnsim(list('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))

#
# Tests
#

context("ds.summary::smk::summary of a numerical variable")
res <- ds.summary(x='D$LAB_TSC')
test_that("summary_numerical_variable", {
  expect_equal(res$sim1$class, "numeric")
  expect_equal(res$sim2$length, 3088)
  expect_equal(res$sim3$`quantiles & mean`[[4]], 5.786)
})

context("ds.summary::smk::summary of a character variable")
ds.asCharacter(x='D$GENDER', newobj="a_character")
res <- ds.summary(x='a_character')
test_that("summary_character_variable", {
  expect_equal(res$sim1$class, "character")
  expect_equal(res$sim2$length, 3088)
})

context("ds.summary::smk::summary of a factor variable")
res <- ds.summary(x='D$GENDER')
test_that("summary_factor_variable", {
  expect_equal(res$sim1$class, "factor")
  expect_equal(res$sim2$length, 3088)
  expect_equal(res$sim3$`count of '0'`, 2091)
  expect_equal(res$sim1$categories[[1]], "0")
  expect_equal(res$sim1$categories[[2]], "1")
})

context("ds.summary::smk::summary of a data frame")
res <- ds.summary(x='D')
test_that("summary_data_frame", {
  expect_equal(res$sim1$class, "data.frame")
  expect_equal(res$sim2$`number of rows`, 3088)
  expect_equal(res$sim2$`number of columns`, 11)
  expect_equal(res$sim3$`variables held`[[11]], "PM_BMI_CATEGORICAL")
  expect_equal(res$sim1$`variables held`[[2]], "LAB_TRIG")
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
