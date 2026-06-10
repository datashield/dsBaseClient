#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.mice::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC","LAB_TRIG","LAB_HDL","LAB_GLUC_ADJUSTED",
                                   "PM_BMI_CONTINUOUS","DIS_CVA","MEDI_LPD","DIS_DIAB",
                                   "DIS_AMI","GENDER","PM_BMI_CATEGORICAL"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.mice::smk::imp1")
test_that("mice, initial imputation", {
    initialImp <- ds.mice(data="D", m=1, method=NULL, predictorMatrix=NULL, post=NULL, seed="NA",
                          newobj_df='impSet')
    
    expect_length(initialImp, 3)
    expect_length(initialImp$sim1, 3)
    expect_length(initialImp$sim2, 3)
    expect_length(initialImp$sim3, 3)
    expect_true("character" %in% class(initialImp$sim1$method))
    expect_equal(as.character(initialImp$sim1$method), c("pmm","pmm","pmm","pmm","pmm","","","","","","polyreg"))
    expect_true("matrix" %in% class(initialImp$sim1$predictorMatrix))
    expect_true("array" %in% class(initialImp$sim1$predictorMatrix))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,1]), c(0,1,1,1,1,1,1,1,1,1,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,2]), c(1,0,1,1,1,1,1,1,1,1,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,3]), c(1,1,0,1,1,1,1,1,1,1,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,4]), c(1,1,1,0,1,1,1,1,1,1,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,5]), c(1,1,1,1,0,1,1,1,1,1,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,6]), c(0,0,0,0,0,0,0,0,0,0,0))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,7]), c(1,1,1,1,1,1,0,1,1,1,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,8]), c(1,1,1,1,1,1,1,0,1,1,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,9]), c(1,1,1,1,1,1,1,1,0,1,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,10]), c(1,1,1,1,1,1,1,1,1,0,1))
    expect_equal(as.numeric(initialImp$sim1$predictorMatrix[,11]), c(1,1,1,1,1,1,1,1,1,1,0))
    expect_true("character" %in% class(initialImp$sim1$post))
    expect_equal(as.character(initialImp$sim1$post), c("","","","","","","","","","",""))
    
    numNA_bmi <- ds.numNA('impSet.1$PM_BMI_CONTINUOUS')
    expect_equal(numNA_bmi$sim1, 0)
    expect_equal(numNA_bmi$sim2, 0)
    expect_equal(numNA_bmi$sim3, 0)

})

# context("ds.mice::smk::imp2")
test_that("mice, second imputation", {
  
  initialImp <- ds.mice(data="D", m=1, method=NULL, predictorMatrix=NULL, post=NULL, seed="NA",
                        newobj_df='impSet')
  
  method <- initialImp$sim1$method
  method["LAB_TRIG"] <- "norm"
  predictorMatrix <- initialImp$sim1$predictorMatrix
  predictorMatrix[,"LAB_GLUC_ADJUSTED"] <- 0
  post <- initialImp$sim1$post
  post["PM_BMI_CONTINUOUS"] <- "imp[[j]][, i] <- squeeze(imp[[j]][, i], c(15,35))"
  
  newImp <- ds.mice(data='D', m=5, maxit=10, method=method, post=post, 
                    predictorMatrix=predictorMatrix, seed=NA,
                    newobj_df='imp_new', newobj_mids='mids_new')
  
  expect_length(newImp, 3)
  expect_length(newImp$sim1, 3)
  expect_length(newImp$sim2, 3)
  expect_length(newImp$sim3, 3)
  expect_true("character" %in% class(newImp$sim1$method))
  expect_equal(as.character(newImp$sim1$method), c("pmm","norm","pmm","pmm","pmm","","","","","","polyreg"))
  expect_true("matrix" %in% class(newImp$sim1$predictorMatrix))
  expect_true("array" %in% class(newImp$sim1$predictorMatrix))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,1]), c(0,1,1,1,1,1,1,1,1,1,1))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,2]), c(1,0,1,1,1,1,1,1,1,1,1))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,3]), c(1,1,0,1,1,1,1,1,1,1,1))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,4]), c(0,0,0,0,0,0,0,0,0,0,0))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,5]), c(1,1,1,1,0,1,1,1,1,1,1))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,6]), c(0,0,0,0,0,0,0,0,0,0,0))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,7]), c(1,1,1,1,1,1,0,1,1,1,1))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,8]), c(1,1,1,1,1,1,1,0,1,1,1))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,9]), c(1,1,1,1,1,1,1,1,0,1,1))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,10]), c(1,1,1,1,1,1,1,1,1,0,1))
  expect_equal(as.numeric(newImp$sim1$predictorMatrix[,11]), c(1,1,1,1,1,1,1,1,1,1,0))
  expect_true("character" %in% class(newImp$sim1$post))
  expect_equal(as.character(newImp$sim1$post), c("","","","","imp[[j]][,i]<-squeeze(imp[[j]][,i],c(15,35))","","","","","",""))
  
  numNA_bmi.1 <- ds.numNA('imp_new.1$PM_BMI_CONTINUOUS')
  expect_equal(numNA_bmi.1$sim1, 0)
  expect_equal(numNA_bmi.1$sim2, 0)
  expect_equal(numNA_bmi.1$sim3, 0)
  numNA_bmi.2 <- ds.numNA('imp_new.2$PM_BMI_CONTINUOUS')
  expect_equal(numNA_bmi.2$sim1, 0)
  expect_equal(numNA_bmi.2$sim2, 0)
  expect_equal(numNA_bmi.2$sim3, 0)
  numNA_bmi.3 <- ds.numNA('imp_new.3$PM_BMI_CONTINUOUS')
  expect_equal(numNA_bmi.3$sim1, 0)
  expect_equal(numNA_bmi.3$sim2, 0)
  expect_equal(numNA_bmi.3$sim3, 0)
  numNA_bmi.4 <- ds.numNA('imp_new.4$PM_BMI_CONTINUOUS')
  expect_equal(numNA_bmi.4$sim1, 0)
  expect_equal(numNA_bmi.4$sim2, 0)
  expect_equal(numNA_bmi.4$sim3, 0)
  numNA_bmi.5 <- ds.numNA('imp_new.5$PM_BMI_CONTINUOUS')
  expect_equal(numNA_bmi.5$sim1, 0)
  expect_equal(numNA_bmi.5$sim2, 0)
  expect_equal(numNA_bmi.5$sim3, 0)
  
})

#
# Done
#

# context("ds.mice::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "impSet.1", "imp_new.1","imp_new.2","imp_new.3","imp_new.4","imp_new.5",
                          "mids_new","mids_object"))
})

disconnect.studies.dataset.cnsim()

# context("ds.mice::smk::done")
