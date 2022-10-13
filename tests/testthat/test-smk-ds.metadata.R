#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.metadata::smk::setup")

connect.studies.dataset.cnsim(list('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.metadata::smk")
test_that("data.frame metadata", {
    res <- ds.metadata(x='D')

    expect_true(all(class(res) %in% c('list')))
    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_true(all(class(res$sim1) %in% c('list')))
    expect_true(all(names(res$sim1) %in% c('names', 'spec', 'class')))
    expect_length(res$sim1$names, 11)
    expect_length(res$sim1$class, 1)
    expect_length(res$sim2, 2)
    expect_true(all(class(res$sim2) %in% c('list')))
    expect_true(all(names(res$sim2) %in% c('names', 'spec', 'class')))
    expect_length(res$sim2$names, 11)
    expect_length(res$sim2$class, 1)
    expect_length(res$sim3, 2)
    expect_true(all(class(res$sim3) %in% c('list')))
    expect_true(all(names(res$sim3) %in% c('names', 'spec', 'class')))
    expect_length(res$sim3$names, 11)
    expect_length(res$sim3$class, 1)
})

test_that("column metadata", {
    res <- ds.metadata(x='D$LAB_TSC')

    expect_true(all(class(res) %in% c('list')))
    expect_length(res, 3)
    expect_length(res$sim1, 0)
    expect_length(res$sim2, 0)
    expect_length(res$sim3, 0)
})

#
# Tear down
#

context("ds.metadata::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

context("ds.metadata::smk::done")
