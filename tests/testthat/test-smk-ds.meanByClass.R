#-------------------------------------------------------------------------------
# Copyright (c) 2018-2021 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.meanByClass::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC","LAB_HDL","PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.meanByClass::smk::LAB_TSC across PM_BMI_CATEGORICAL categories where both vectors are 'loose'")
ds.assign("D$LAB_TSC", "ldl")
ds.assign("D$PM_BMI_CATEGORICAL", "pm_bmi")
test_that("LAB_TSC_across_", {
    res <- ds.meanByClass(x='ldl~pm_bmi')

    expect_length(res, 6)
    expect_equal(res[[1]], '2753')
    expect_equal(res[[2]], '5.85(1.03)')
    expect_equal(res[[3]], '3545')
    expect_equal(res[[4]], '5.82(1.05)')
    expect_equal(res[[5]], '2629')
    expect_equal(res[[6]], '5.89(1.15)')
})

context("ds.meanByClass::smk::calculate the mean proportion for LAB_HDL across PM_BMI_CATEGORICAL categories")
res <- ds.meanByClass(x='D', outvar='LAB_HDL', covar='PM_BMI_CATEGORICAL')
test_that("LAB_HDL_across_PM_BMI_CATEGORICAL", {

    expect_length(res, 6)
    expect_equal(res[[1]], '2753')
    expect_equal(res[[2]], '1.64(0.41)')
    expect_equal(res[[3]], '3545')
    expect_equal(res[[4]], '1.57(0.41)')
    expect_equal(res[[5]], '2629')
    expect_equal(res[[6]], '1.5(0.44)')
})


context("ds.meanByClass::smk::calculate the mean proportion for LAB_HDL & LAB_TSC across bmi categories")
res <- ds.meanByClass(x='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('PM_BMI_CATEGORICAL'))
test_that("LAB_HDL-LAB_TSC_across_PM_BMI_CATEGORICAL", {

    expect_length(res, 12)
    expect_equal(res[[1]], '2753')
    expect_equal(res[[2]], '1.64(0.41)')
    expect_equal(res[[3]], '2753')
    expect_equal(res[[4]], '5.85(1.03)')
    expect_equal(res[[5]], '3545')
    expect_equal(res[[6]], '1.57(0.41)')
    expect_equal(res[[7]], '3545')
    expect_equal(res[[8]], '5.82(1.05)')
    expect_equal(res[[9]], '2629')
    expect_equal(res[[10]], '1.5(0.44)')
    expect_equal(res[[11]], '2629')
    expect_equal(res[[12]], '5.89(1.15)')
})


# context("ds.meanByClass::smk::calculate the mean proportion for LAB_HDL across gender bmi and diabetes status categories")
# res <- ds.meanByClass(datasources=ds.test_env$connection.opal, x='D', outvar=c('LAB_HDL','LAB_TSC'), covar=c('GENDER','PM_BMI_CATEGORICAL','DIS_DIAB'))


context("ds.meanByClass::smk::calculate the mean proportion for LAB_HDL across PM_BMI_CATEGORICAL categories, split")
res <- ds.meanByClass(x='D', outvar='LAB_HDL', covar='PM_BMI_CATEGORICAL', type='split')
test_that("LAB_HDL_across_PM_BMI_CATEGORICAL", {

    expect_length(res, 3)
    expect_length(res$sim1, 6)
    expect_equal(res$sim1[[1]], '641')
    expect_equal(res$sim1[[2]], '1.64(0.4)')
    expect_equal(res$sim1[[3]], '816')
    expect_equal(res$sim1[[4]], '1.57(0.4)')
    expect_equal(res$sim1[[5]], '609')
    expect_equal(res$sim1[[6]], '1.51(0.42)')
    expect_length(res$sim2, 6)
    expect_equal(res$sim2[[1]], '899')
    expect_equal(res$sim2[[2]], '1.62(0.4)')
    expect_equal(res$sim2[[3]], '1173')
    expect_equal(res$sim2[[4]], '1.56(0.41)')
    expect_equal(res$sim2[[5]], '866')
    expect_equal(res$sim2[[6]], '1.49(0.45)')
    expect_length(res$sim3, 6)
    expect_equal(res$sim3[[1]], '1213')
    expect_equal(res$sim3[[2]], '1.65(0.41)')
    expect_equal(res$sim3[[3]], '1556')
    expect_equal(res$sim3[[4]], '1.57(0.41)')
    expect_equal(res$sim3[[5]], '1154')
    expect_equal(res$sim3[[6]], '1.51(0.44)')
})


#
# Tear down
#

context("ds.meanByClass::smk::shutdown")

test_that("shutdown", {
     ds_expect_variables(c("D", "D.PM_BMI_CATEGORICAL1", "D.PM_BMI_CATEGORICAL2", "D.PM_BMI_CATEGORICAL3",
                           "ldl", "pm_bmi", "tempholder", "X", "X.pm_bmi1", "X.pm_bmi2", "X.pm_bmi3"))
})

disconnect.studies.dataset.cnsim()

context("ds.meanByClass::smk::done")
