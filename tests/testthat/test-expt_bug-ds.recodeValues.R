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
source("definition_tests/def-ds.recodeValues.R")

# context("ds.recodeValues::expt::changes_applied::single::")
test_that("changes must be applied",
{
  
   connect.dataset.1() 
   values.to.replace <- c(-1000000)
  .test.apply.changes('D$NEGATIVE_NUMERIC','NEG_NUM_recoded',ds.test_env$local.values.1,13,values.to.replace)
  .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
  .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            
   values.to.replace <- c(8000)
  .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
 
  .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
  .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
  .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            
   values.to.replace <- c(100000)
   .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEG_INT_recoded',ds.test_env$local.values.1,7, values.to.replace)
   .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEG_NUM_recoded',ds.test_env$local.values.1,11, values.to.replace)
           
   connect.dataset.1() 
   values.to.replace <- c(-100000,-200000)
  .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
  .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
  .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            
   values.to.replace <- c(0,200000)
   .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
   .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
   .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
   .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            
    values.to.replace <- c(100000,200000)
   .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
   .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
})


# context("ds.recodeValues::expt::changes_applied::multiple")
test_that("changes must be applied",
{
    
     connect.all.datasets()
   
     values.to.replace <- c(-100)
    .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
    .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
    .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
     values.to.replace <- c(0)
     .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
     .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
     .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
     .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
     values.to.replace <- c(100)
     .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
     .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
    
     connect.all.datasets()
     values.to.replace <- c(-100,-200)
     .test.apply.changes('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
     .test.apply.changes('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
     .test.apply.changes('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
     values.to.replace <- c(0,200)
     .test.apply.changes('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
     .test.apply.changes('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
      .test.apply.changes('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
      .test.apply.changes('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
      values.to.replace <- c(100,200)
      .test.apply.changes('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
      .test.apply.changes('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
    
})



  # context("ds.recodeValues::expt::no_change_applied::single")
  test_that("no_change_applied",
  {
      connect.dataset.1() 
      values.to.replace <- c()
      .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
      .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
      .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
      .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
      .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
      .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
      .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
      .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
      .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
              
      connect.dataset.1() 
      values.to.replace <- c(-100)
      .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
      .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
      .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
              
      values.to.replace <- c(0)
      .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
      .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
      .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
      .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
              
      values.to.replace <- c(100)
      .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEG_INT_recoded',ds.test_env$local.values.1,7, values.to.replace)
      .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEG_NUM_recoded',ds.test_env$local.values.1,11, values.to.replace)
              
      values.to.replace <- c(-100,-200)
      .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
      .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
      .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
              
      values.to.replace <- c(0,200)
      .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
      .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
      .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
      .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
              
      values.to.replace <- c(100,200)
      .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
      .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
  })            
              
          
# context("ds.recodeValues::expt::no_change_applied::multiple")
test_that("no_change_applied",
{
    connect.all.datasets()
    values.to.replace <- c()
    .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
    .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
   .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
    values.to.replace <- c()
    .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
    .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
    .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
    .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
    values.to.replace <- c()
    .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
    .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
            
    values.to.replace <- c(-100)
    .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
    .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
    .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
    values.to.replace <- c(0)
    .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
    .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
    .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
    .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
    values.to.replace <- c(100)
    .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
    .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
     
    connect.all.datasets()
    values.to.replace <- c(-100,-200)
    .test.apply.no.change('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,13,values.to.replace)
    .test.apply.no.change('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values,15,values.to.replace)
    .test.apply.no.change('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values,9,values.to.replace)
            
    values.to.replace <- c(0,200)
    .test.apply.no.change('D$INTEGER','INTEGER_recoded',ds.test_env$local.values,6,values.to.replace)
    .test.apply.no.change('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values,8,values.to.replace)
    .test.apply.no.change('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values,10,values.to.replace)
    .test.apply.no.change('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values,12,values.to.replace)
            
    values.to.replace <- c(100,200)
    .test.apply.no.change('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values,7, values.to.replace)
    .test.apply.no.change('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values,11, values.to.replace)
})
