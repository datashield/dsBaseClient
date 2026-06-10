#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-ds.make.R")
source("definition_tests/def-assign-stats.R")


# context("ds.make::expt::copy_transform:single")
test_that("copy and transform", 
{
  connect.dataset.1()
  constant.value <- sample(2:200,1)
  .test.copy.apply.operator('D$INTEGER',constant.value,'NUMERIC_created',"+",some.values = ds.test_env$local.values.1[,6], result.local = (ds.test_env$local.values.1[,6] + constant.value))
  .test.copy.apply.operator('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"*",some.values = ds.test_env$local.values.1[,7],result.local = (ds.test_env$local.values[,7] * constant.value))
  .test.copy.apply.operator('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"*",some.values = ds.test_env$local.values.1[,8],result.local = (ds.test_env$local.values[,8] * constant.value))
  .test.copy.apply.operator('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"*",some.values = ds.test_env$local.values.1[,9],result.local = (ds.test_env$local.values[,9] * constant.value))
  .test.copy.apply.operator('D$NUMERIC',constant.value,'NUMERIC_created',"*",some.values = ds.test_env$local.values.1[,10],result.local = (ds.test_env$local.values[,10] * constant.value))
  .test.copy.apply.operator('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"*",some.values = ds.test_env$local.values.1[,11],result.local = (ds.test_env$local.values[,11] * constant.value))
  .test.copy.apply.operator('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"*",some.values = ds.test_env$local.values.1[,12],result.local = (ds.test_env$local.values[,12] * constant.value))
  .test.copy.apply.operator('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"*",some.values = ds.test_env$local.values.1[,13],result.local = (ds.test_env$local.values[,13] * constant.value))
})



# context("ds.make::expt::copy_data::single")
test_that("copy data without any changes applied",
{
  connect.dataset.1()
  .test.copy.data('D$INTEGER','INTEGER_created',ds.test_env$local.values.1[,6])
  .test.copy.data('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_created',ds.test_env$local.values.1[,7])
  .test.copy.data('D$POSITIVE_INTEGER','POSITIVE_INTEGER_created',ds.test_env$local.values.1[,8])
  .test.copy.data('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_created',ds.test_env$local.values.1[,9])
  .test.copy.data('D$NUMERIC','NUMERIC_created',ds.test_env$local.values.1[,10])
  .test.copy.data('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_created',ds.test_env$local.values.1[,11])
  .test.copy.data('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_created',ds.test_env$local.values.1[,12])
  .test.copy.data('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_created',ds.test_env$local.values.1[,13])
  .test.copy.data('D$FACTOR_INTEGER','FACTOR_INTEGER_created',ds.test_env$local.values.1[,15])
})

# context("ds.make::expt::copy_data::multiple")
test_that("copy data without any changes applied",
{
  connect.all.datasets()
  .test.copy.data('D$INTEGER','INTEGER_created',ds.test_env$local.values[,6])
  .test.copy.data('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_created',ds.test_env$local.values[,7])
  .test.copy.data('D$POSITIVE_INTEGER','POSITIVE_INTEGER_created',ds.test_env$local.values[,8])
  .test.copy.data('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_created',ds.test_env$local.values[,9])
  .test.copy.data('D$NUMERIC','NUMERIC_created',ds.test_env$local.values[,10])
  .test.copy.data('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_created',ds.test_env$local.values[,11])
  .test.copy.data('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_created',ds.test_env$local.values[,12])
  .test.copy.data('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_created',ds.test_env$local.values[,13])
  .test.copy.data('D$FACTOR_INTEGER','FACTOR_INTEGER_created',ds.test_env$local.values[,15])
})

# context("ds.make::expt::sum_of_two_vectors::single")
test_that("apply the the sum some vectors",
{
  connect.dataset.1()
  .test.operation.vectors('D$INTEGER','D$INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,6]))
  
  .test.operation.vectors('D$NON_NEGATIVE_INTEGER','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,7],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$POSITIVE_INTEGER','D$POSITIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,8],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$NEGATIVE_INTEGER','D$NEGATIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,9],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$NUMERIC','D$NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,10]))
  .test.operation.vectors('D$NON_NEGATIVE_NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,11],ds.test_env$local.values.1[,11]))
  .test.operation.vectors('D$POSITIVE_NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,12],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$NEGATIVE_NUMERIC','D$NEGATIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,13],ds.test_env$local.values.1[,13]))
  
  .test.operation.vectors('D$INTEGER','D$NEGATIVE_INTEGER','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_INTEGER','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_INTEGER','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$INTEGER','D$NUMERIC','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,10]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_NUMERIC','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_NUMERIC','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,11]))
  
  
  .test.operation.vectors('D$NUMERIC','D$NEGATIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,11]))
  
  
})


# context("ds.make::expt::sum_of_two_vectors::multiple")
test_that("add some vectors",
 {
   connect.all.datasets()
   .test.operation.vectors('D$INTEGER','D$INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,6]))
   
   .test.operation.vectors('D$NON_NEGATIVE_INTEGER','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,7],ds.test_env$local.values[,7]))
   .test.operation.vectors('D$POSITIVE_INTEGER','D$POSITIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,8],ds.test_env$local.values[,8]))
   .test.operation.vectors('D$NEGATIVE_INTEGER','D$NEGATIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,9],ds.test_env$local.values[,9]))
   .test.operation.vectors('D$NUMERIC','D$NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,10]))
   .test.operation.vectors('D$NON_NEGATIVE_NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,11],ds.test_env$local.values[,11]))
   .test.operation.vectors('D$POSITIVE_NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,12],ds.test_env$local.values[,12]))
   .test.operation.vectors('D$NEGATIVE_NUMERIC','D$NEGATIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,13],ds.test_env$local.values[,13]))
   
   .test.operation.vectors('D$INTEGER','D$NEGATIVE_INTEGER','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,9]))
   .test.operation.vectors('D$INTEGER','D$POSITIVE_INTEGER','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,8]))
   .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_INTEGER','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,7]))
   .test.operation.vectors('D$INTEGER','D$NUMERIC','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,10]))
   .test.operation.vectors('D$INTEGER','D$POSITIVE_NUMERIC','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,12]))
   .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_NUMERIC','INTEGER_created',"+",result.local = .add.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,11]))
   
   
   .test.operation.vectors('D$NUMERIC','D$NEGATIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,9]))
   .test.operation.vectors('D$NUMERIC','D$POSITIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,8]))
   .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,7]))
   .test.operation.vectors('D$NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,12]))
   .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"+",result.local = .add.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,11]))
 })

# context("ds.make::expt::product_of_two_vectors::single")
test_that("multiply two vectors",
{
  connect.dataset.1()
  .test.operation.vectors('D$INTEGER','D$INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,6]))
  .test.operation.vectors('D$NON_NEGATIVE_INTEGER','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,7],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$POSITIVE_INTEGER','D$POSITIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,8],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$NEGATIVE_INTEGER','D$NEGATIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,9],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$NUMERIC','D$NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,10]))
  .test.operation.vectors('D$NON_NEGATIVE_NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,11],ds.test_env$local.values.1[,11]))
  .test.operation.vectors('D$POSITIVE_NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,12],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$NEGATIVE_NUMERIC','D$NEGATIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,13],ds.test_env$local.values.1[,13]))
  
  .test.operation.vectors('D$INTEGER','D$NEGATIVE_INTEGER','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_INTEGER','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_INTEGER','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$INTEGER','D$NUMERIC','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,10]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_NUMERIC','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_NUMERIC','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,11]))
  
  
  .test.operation.vectors('D$NUMERIC','D$NEGATIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,11]))
  
  
})




# context("ds.make::expt::product_of_two_vectors::multiple")
test_that("multiply two vectors",
{
  connect.all.datasets()
  .test.operation.vectors('D$INTEGER','D$INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,6]))
  
  .test.operation.vectors('D$NON_NEGATIVE_INTEGER','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,7],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$POSITIVE_INTEGER','D$POSITIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,8],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$NEGATIVE_INTEGER','D$NEGATIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,9],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$NUMERIC','D$NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,10]))
  .test.operation.vectors('D$NON_NEGATIVE_NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,11],ds.test_env$local.values[,11]))
  .test.operation.vectors('D$POSITIVE_NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,12],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$NEGATIVE_NUMERIC','D$NEGATIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,13],ds.test_env$local.values[,13]))
  
  .test.operation.vectors('D$INTEGER','D$NEGATIVE_INTEGER','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_INTEGER','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_INTEGER','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$INTEGER','D$NUMERIC','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,10]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_NUMERIC','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_NUMERIC','INTEGER_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,11]))
  
  
  .test.operation.vectors('D$NUMERIC','D$NEGATIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"*",result.local = .mult.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,11]))
})

# context("ds.make::expt::division_of_two_vectors::multiple")
test_that("divide two vectors",
{
  connect.all.datasets()
  .test.operation.vectors('D$INTEGER','D$INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,6]))
  .test.operation.vectors('D$NON_NEGATIVE_INTEGER','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,7],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$POSITIVE_INTEGER','D$POSITIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,8],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$NEGATIVE_INTEGER','D$NEGATIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,9],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$NUMERIC','D$NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,10]))
  .test.operation.vectors('D$NON_NEGATIVE_NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,11],ds.test_env$local.values[,11]))
  .test.operation.vectors('D$POSITIVE_NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,12],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$NEGATIVE_NUMERIC','D$NEGATIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,13],ds.test_env$local.values[,13]))
  
  .test.operation.vectors('D$INTEGER','D$NEGATIVE_INTEGER','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_INTEGER','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_INTEGER','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$INTEGER','D$NUMERIC','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,10]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_NUMERIC','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_NUMERIC','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,11]))
  
  
  .test.operation.vectors('D$NUMERIC','D$NEGATIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,11]))
})


# context("ds.make::expt::division_of_two_vectors::single")
test_that("divide two vectors",
{
  connect.dataset.1()
  .test.operation.vectors('D$INTEGER','D$INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,6]))
  .test.operation.vectors('D$NON_NEGATIVE_INTEGER','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,7],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$POSITIVE_INTEGER','D$POSITIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,8],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$NEGATIVE_INTEGER','D$NEGATIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,9],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$NUMERIC','D$NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,10]))
  .test.operation.vectors('D$NON_NEGATIVE_NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,11],ds.test_env$local.values.1[,11]))
  .test.operation.vectors('D$POSITIVE_NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,12],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$NEGATIVE_NUMERIC','D$NEGATIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,13],ds.test_env$local.values.1[,13]))
  
  .test.operation.vectors('D$INTEGER','D$NEGATIVE_INTEGER','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_INTEGER','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_INTEGER','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$INTEGER','D$NUMERIC','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,10]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_NUMERIC','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_NUMERIC','INTEGER_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,11]))
  
  
  .test.operation.vectors('D$NUMERIC','D$NEGATIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"/",result.local = .divide.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,11]))
})


# context("ds.make::expt::substract_of_two_vectors::multiple")
test_that("substract two vectors",
{
  connect.all.datasets()
  .test.operation.vectors('D$INTEGER','D$INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,6]))
  .test.operation.vectors('D$NON_NEGATIVE_INTEGER','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,7],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$POSITIVE_INTEGER','D$POSITIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,8],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$NEGATIVE_INTEGER','D$NEGATIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,9],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$NUMERIC','D$NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,10]))
  .test.operation.vectors('D$NON_NEGATIVE_NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,11],ds.test_env$local.values[,11]))
  .test.operation.vectors('D$POSITIVE_NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,12],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$NEGATIVE_NUMERIC','D$NEGATIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,13],ds.test_env$local.values[,13]))
  
  .test.operation.vectors('D$INTEGER','D$NEGATIVE_INTEGER','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_INTEGER','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_INTEGER','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$INTEGER','D$NUMERIC','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,10]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_NUMERIC','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_NUMERIC','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,6],ds.test_env$local.values[,11]))
  
  
  .test.operation.vectors('D$NUMERIC','D$NEGATIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,9]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,8]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,7]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,12]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values[,10],ds.test_env$local.values[,11]))
})


# context("ds.make::expt::substract_of_two_vectors::single")
test_that("substract two vectors",
{
  connect.dataset.1()
  .test.operation.vectors('D$INTEGER','D$INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,6]))
  .test.operation.vectors('D$NON_NEGATIVE_INTEGER','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,7],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$POSITIVE_INTEGER','D$POSITIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,8],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$NEGATIVE_INTEGER','D$NEGATIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,9],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$NUMERIC','D$NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,10]))
  .test.operation.vectors('D$NON_NEGATIVE_NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,11],ds.test_env$local.values.1[,11]))
  .test.operation.vectors('D$POSITIVE_NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,12],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$NEGATIVE_NUMERIC','D$NEGATIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,13],ds.test_env$local.values.1[,13]))
  
  .test.operation.vectors('D$INTEGER','D$NEGATIVE_INTEGER','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_INTEGER','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_INTEGER','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$INTEGER','D$NUMERIC','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,10]))
  .test.operation.vectors('D$INTEGER','D$POSITIVE_NUMERIC','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$INTEGER','D$NON_NEGATIVE_NUMERIC','INTEGER_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,6],ds.test_env$local.values.1[,11]))
  
  
  .test.operation.vectors('D$NUMERIC','D$NEGATIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,9]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,8]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_INTEGER','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,7]))
  .test.operation.vectors('D$NUMERIC','D$POSITIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,12]))
  .test.operation.vectors('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','NUMERIC_created',"-",result.local = .substract.vectors(ds.test_env$local.values.1[,10],ds.test_env$local.values.1[,11]))
})


# context("ds.make::expt::sum_of_constant::single")
test_that("apply the the sum a vector and a constant value",
{
  connect.dataset.1()
  constant.value <- sample(2:200,1)

 .test.operation.constant('D$INTEGER',constant.value,'NUMERIC_created',"+", result.local = (ds.test_env$local.values.1[,6] + constant.value))
 .test.operation.constant('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values.1[,7] + constant.value))
 .test.operation.constant('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values.1[,8] + constant.value))
 .test.operation.constant('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values.1[,9] + constant.value))
 .test.operation.constant('D$NUMERIC',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values.1[,10] + constant.value))
 .test.operation.constant('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values.1[,11] + constant.value))
 .test.operation.constant('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values.1[,12] + constant.value))
 .test.operation.constant('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values.1[,13] + constant.value))
 
 })

# context("ds.make::expt::substract_of_constant::single")
test_that("substract a constant value to all the values of a vector",
{
  connect.dataset.1()
  constant.value <- sample(2:200,1)
 
  .test.operation.constant('D$INTEGER',constant.value,'NUMERIC_created',"-", result.local = (ds.test_env$local.values.1[,6] - constant.value))
  .test.operation.constant('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values.1[,7] - constant.value))
  .test.operation.constant('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values.1[,8] - constant.value))
  .test.operation.constant('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values.1[,9] - constant.value))
  .test.operation.constant('D$NUMERIC',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values.1[,10] - constant.value))
  .test.operation.constant('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values.1[,11] - constant.value))
  .test.operation.constant('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values.1[,12] - constant.value))
  .test.operation.constant('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values.1[,13] - constant.value))
  
})


# context("ds.make::expt::divide_of_constant::single")
test_that("divide a constant value to all the values of a vector",
{
  connect.dataset.1()
  constant.value <- sample(2:200,1)
        
  .test.operation.constant('D$INTEGER',constant.value,'NUMERIC_created',"/", result.local = (ds.test_env$local.values.1[,6] / constant.value))
  .test.operation.constant('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values.1[,7] / constant.value))
  .test.operation.constant('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values.1[,8] / constant.value))
  .test.operation.constant('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values.1[,9] / constant.value))
  .test.operation.constant('D$NUMERIC',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values.1[,10] / constant.value))
  .test.operation.constant('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values.1[,11] / constant.value))
  .test.operation.constant('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values.1[,12] / constant.value))
  .test.operation.constant('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values.1[,13] / constant.value))
  
})

# context("ds.make::expt::multiply_of_constant::single")
test_that("multiply a constant value to all the values of a vector",
{
  connect.dataset.1()
  constant.value <- sample(2:200,1)
#  print(constant.value)
  .test.operation.constant('D$INTEGER',constant.value,'NUMERIC_created',"*", result.local = (ds.test_env$local.values.1[,6] * constant.value))
  .test.operation.constant('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values.1[,7] * constant.value))
  .test.operation.constant('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values.1[,8] * constant.value))
  .test.operation.constant('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values.1[,9] * constant.value))
  .test.operation.constant('D$NUMERIC',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values.1[,10] * constant.value))
  .test.operation.constant('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values.1[,11] * constant.value))
  .test.operation.constant('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values.1[,12] * constant.value))
  .test.operation.constant('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values.1[,13] * constant.value))
  
})


# context("ds.make::expt::sum_of_constant::multiple")
test_that("apply the the sum a vector and a constant value",
{
  connect.all.datasets()
  constant.value <- sample(2:200,1)

  .test.operation.constant('D$INTEGER',constant.value,'NUMERIC_created',"+", result.local = (ds.test_env$local.values[,6] + constant.value))
  .test.operation.constant('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values[,7] + constant.value))
  .test.operation.constant('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values[,8] + constant.value))
  .test.operation.constant('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values[,9] + constant.value))
  .test.operation.constant('D$NUMERIC',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values[,10] + constant.value))
  .test.operation.constant('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values[,11] + constant.value))
  .test.operation.constant('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values[,12] + constant.value))
  .test.operation.constant('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"+",result.local = (ds.test_env$local.values[,13] + constant.value))
  
})

# context("ds.make::expt::substract_of_constant::multiple")
test_that("substract a constant value to all the values of a vector",
{
  connect.all.datasets()
  constant.value <- sample(2:200,1)
         
  .test.operation.constant('D$INTEGER',constant.value,'NUMERIC_created',"-", result.local = (ds.test_env$local.values[,6] - constant.value))
  .test.operation.constant('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values[,7] - constant.value))
  .test.operation.constant('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values[,8] - constant.value))
  .test.operation.constant('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values[,9] - constant.value))
  .test.operation.constant('D$NUMERIC',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values[,10] - constant.value))
  .test.operation.constant('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values[,11] - constant.value))
  .test.operation.constant('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values[,12] - constant.value))
  .test.operation.constant('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"-",result.local = (ds.test_env$local.values[,13] - constant.value))
  
})


# context("ds.make::expt::divide_of_constant::multiple")
test_that("divide a constant value to all the values of a vector",
{
  connect.all.datasets()
  constant.value <- sample(2:200,1)
         
  .test.operation.constant('D$INTEGER',constant.value,'NUMERIC_created',"/", result.local = (ds.test_env$local.values[,6] / constant.value))
  .test.operation.constant('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values[,7] / constant.value))
  .test.operation.constant('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values[,8] / constant.value))
  .test.operation.constant('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values[,9] / constant.value))
  .test.operation.constant('D$NUMERIC',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values[,10] / constant.value))
  .test.operation.constant('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values[,11] / constant.value))
  .test.operation.constant('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values[,12] / constant.value))
  .test.operation.constant('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"/",result.local = (ds.test_env$local.values[,13] / constant.value))
  
})

# context("ds.make::expt::multiply_of_constant::multiple")
test_that("multiply a constant value to all the values of a vector",
{
  connect.all.datasets()
  constant.value <- sample(2:200,1)
 
  .test.operation.constant('D$INTEGER',constant.value,'NUMERIC_created',"*", result.local = (ds.test_env$local.values[,6] * constant.value))
  .test.operation.constant('D$NON_NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values[,7] * constant.value))
  .test.operation.constant('D$POSITIVE_INTEGER',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values[,8] * constant.value))
  .test.operation.constant('D$NEGATIVE_INTEGER',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values[,9] * constant.value))
  .test.operation.constant('D$NUMERIC',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values[,10] * constant.value))
  .test.operation.constant('D$NON_NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values[,11] * constant.value))
  .test.operation.constant('D$POSITIVE_NUMERIC',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values[,12] * constant.value))
  .test.operation.constant('D$NEGATIVE_NUMERIC',constant.value,'NUMERIC_created',"*",result.local = (ds.test_env$local.values[,13] * constant.value))
  
})

