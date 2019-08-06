#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2018 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.mean::smk_expt::start")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "LAB_TRIG", "LAB_HDL", "LAB_GLUC_ADJUSTED", "PM_BMI_CONTINUOUS", "DIS_CVA", "MEDI_LPD", "DIS_DIAB", "DIS_AMI", "GENDER", "PM_BMI_CATEGORICAL"))

#
# Tests
#

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$LAB_TSC',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_TSC-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$LAB_TSC', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_TSC-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$LAB_TSC', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_TSC-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$LAB_TRIG',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_TRIG-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$LAB_TRIG', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_TRIG-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$LAB_TRIG', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_TRIG-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$LAB_HDL',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_HDL-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$LAB_HDL', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_HDL-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$LAB_HDL', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_HDL-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$LAB_GLUC_ADJUSTED',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_GLUC_ADJUSTED-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$LAB_GLUC_ADJUSTED', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_GLUC_ADJUSTED-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$LAB_GLUC_ADJUSTED', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-LAB_GLUC_ADJUSTED-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$PM_BMI_CONTINUOUS',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-PM_BMI_CONTINUOUS-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$PM_BMI_CONTINUOUS', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-PM_BMI_CONTINUOUS-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$PM_BMI_CONTINUOUS', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-PM_BMI_CONTINUOUS-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$DIS_CVA',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_CVA-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$DIS_CVA', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_CVA-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$DIS_CVS', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_CVS-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$MEDI_LPD',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-MEDI_LPD-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$MEDI_LPD', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-MEDI_LPD-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$MEDI_LPD', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-MEDI_LPD-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$DIS_DIAB',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_DIAB-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$DIS_DIAB', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_DIAB-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$DIS_DIAB', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_DIAB-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$DIS_AMI',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_AMI-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$DIS_AMI', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_AMI-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$DIS_AMI', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-DIS_AMI-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$GENDER',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-GENDER-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$GENDER', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-GENDER-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$GENDER', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-GENDER-both.rds')
})

context("ds.mean::smk_expt::type=combine")
test_that("mean values [combine]", {
    stat.mean <- ds.mean(x='D$PM_BMI_CATEGORICAL',type='combine')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-PM_BMI_CATEGORICAL-combine.rds')
})

context("ds.mean::smk_expt::type=split")
test_that("mean values [split]", {
    stat.mean <- ds.mean(x='D$PM_BMI_CATEGORICAL', type='split')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-PM_BMI_CATEGORICAL-split.rds')
})

context("ds.mean::smk_expt::type=both")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$PM_BMI_CATEGORICAL', type='both')

    expect_equal_to_reference(stat.mean, 'smk_expt-results/ds.mean-PM_BMI_CATEGORICAL-both.rds')
})

#
# Done
#

# context("ds.mean::smk_expt::done")