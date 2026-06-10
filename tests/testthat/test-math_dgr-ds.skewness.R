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
source("definition_tests/def-ds.skewness.R")

# context("ds.skewness::math_dgr::mean.median.mode::multiple")
test_that("skewness >0 & mode<median<mean or skewness<0 & mean<median<mode",
{
 connect.all.datasets()
            
  .test.skewness.mean.median.mode('D$INTEGER', method = 1)
  .test.skewness.mean.median.mode('D$NON_NEGATIVE_INTEGER', method = 2)
  .test.skewness.mean.median.mode('D$POSITIVE_INTEGER', method = 3)
  #.test.skewness.mean.median.mode('D$NEGATIVE_INTEGER', method = 1) 
  #.test.skewness.mean.median.mode('D$NUMERIC', method = 2)
  .test.skewness.mean.median.mode('D$NON_NEGATIVE_NUMERIC', method = 3)
  .test.skewness.mean.median.mode('D$POSITIVE_NUMERIC', method = 1)
  .test.skewness.mean.median.mode('D$NEGATIVE_NUMERIC', method = 2) 

})

# context("ds.skewness::math_dgr::Q1.Q2.Q3::multiple")
test_that("skewness >0 & Q3-Q2>Q2-Q1 or skewness<0 & Q3-Q2<Q2-Q1",
{
  connect.all.datasets()
            
  .test.skewness.Q1.Q2.Q3('D$INTEGER', method = 1)
  .test.skewness.Q1.Q2.Q3('D$NON_NEGATIVE_INTEGER', method = 2)
  .test.skewness.Q1.Q2.Q3('D$POSITIVE_INTEGER', method = 3)
  .test.skewness.Q1.Q2.Q3('D$NEGATIVE_INTEGER', method = 1) 
  #.test.skewness.Q1.Q2.Q3('D$NUMERIC', method = 2)
  .test.skewness.Q1.Q2.Q3('D$NON_NEGATIVE_NUMERIC', method = 3)
  .test.skewness.Q1.Q2.Q3('D$POSITIVE_NUMERIC', method = 1)
  .test.skewness.Q1.Q2.Q3('D$NEGATIVE_NUMERIC', method = 2) 
       
})
