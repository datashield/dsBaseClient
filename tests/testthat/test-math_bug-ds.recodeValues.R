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

source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-ds.recodeValues.R")

# context("ds.recodeValues::math::single")
test_that("difference",
{
  connect.dataset.1() 
  values.to.replace <- c()
  .test.differences.in.sets('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
  .test.differences.in.sets('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
  .test.differences.in.sets('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
  
  .test.differences.in.sets('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
  .test.differences.in.sets('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
  .test.differences.in.sets('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
  .test.differences.in.sets('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
  .test.differences.in.sets('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
  .test.differences.in.sets('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
  
  
  connect.dataset.1() 
  values.to.replace <- c(1000)
  .test.differences.in.sets('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
  .test.differences.in.sets('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
  .test.differences.in.sets('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
  .test.differences.in.sets('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
  .test.differences.in.sets('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
  .test.differences.in.sets('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
  .test.differences.in.sets('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)

  values.to.replace <- c(-1000)
  .test.differences.in.sets('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
  
  .test.differences.in.sets('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
  
  values.to.replace <- c(100,200)
 .test.differences.in.sets('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
 .test.differences.in.sets('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
 .test.differences.in.sets('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
 .test.differences.in.sets('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
 .test.differences.in.sets('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
 .test.differences.in.sets('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
 .test.differences.in.sets('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
 
 values.to.replace <- c(-1000,-2000)
 .test.differences.in.sets('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
 .test.differences.in.sets('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
 
 values.to.replace <- c(100,200,300)
 .test.differences.in.sets('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
 .test.differences.in.sets('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
 .test.differences.in.sets('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
 .test.differences.in.sets('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
 .test.differences.in.sets('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
 .test.differences.in.sets('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
 .test.differences.in.sets('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
 
 values.to.replace <- c(-1000,-2000,-3000)
 .test.differences.in.sets('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
 .test.differences.in.sets('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
 

})



# context("ds.recodeValues::math::multiple")
test_that("difference",
          {
            connect.all.datasets() 
            values.to.replace <- c(0,200)
            .test.differences.in.sets('D$NEGATIVE_NUMERIC','NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,13,values.to.replace)
            .test.differences.in.sets('D$FACTOR_INTEGER','FACTOR_INTEGER_recoded',ds.test_env$local.values.1,15,values.to.replace)
            .test.differences.in.sets('D$NEGATIVE_INTEGER','NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,9,values.to.replace)
            .test.differences.in.sets('D$INTEGER','INTEGER_recoded',ds.test_env$local.values.1,6,values.to.replace)
            .test.differences.in.sets('D$POSITIVE_INTEGER','POSITIVE_INTEGER_recoded',ds.test_env$local.values.1,8,values.to.replace)
            .test.differences.in.sets('D$NUMERIC','NUMERIC_recoded',ds.test_env$local.values.1,10,values.to.replace)
            .test.differences.in.sets('D$POSITIVE_NUMERIC','POSITIVE_NUMERIC_recoded',ds.test_env$local.values.1,12,values.to.replace)
            .test.differences.in.sets('D$NON_NEGATIVE_INTEGER','NON_NEGATIVE_INTEGER_recoded',ds.test_env$local.values.1,7, values.to.replace)
            .test.differences.in.sets('D$NON_NEGATIVE_NUMERIC','NON_NEGATIVE_NUMERIC_recoded',ds.test_env$local.values.1,11, values.to.replace)
          })
