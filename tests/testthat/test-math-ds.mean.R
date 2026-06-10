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
source("definition_tests/def-ds.mean.R")

# context("ds.mean::math::residual::multiple")
test_that("residual deviation tends to 0",
{
  connect.all.datasets()
  .test.residual.combined('D$INTEGER',ds.test_env$local.values[,'INTEGER'])
  .test.residual.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,'NON_NEGATIVE_INTEGER'])
  .test.residual.combined('D$POSITIVE_INTEGER',ds.test_env$local.values[,'POSITIVE_INTEGER'])
  .test.residual.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values[,'NEGATIVE_INTEGER']) 
  .test.residual.combined('D$NUMERIC',ds.test_env$local.values[,'NUMERIC'])
  .test.residual.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,'NON_NEGATIVE_NUMERIC'])
  .test.residual.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values[,'POSITIVE_NUMERIC'])
  .test.residual.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,'NEGATIVE_NUMERIC']) 
  
  .test.residual.split('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'],ds.test_env$local.values.2[,'INTEGER'],ds.test_env$local.values.3[,'INTEGER'])
  .test.residual.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NON_NEGATIVE_INTEGER'])
  .test.residual.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'],ds.test_env$local.values.2[,'POSITIVE_INTEGER'],ds.test_env$local.values.3[,'POSITIVE_INTEGER'])
  .test.residual.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NEGATIVE_INTEGER'])
  .test.residual.split('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'],ds.test_env$local.values.2[,'NUMERIC'],ds.test_env$local.values.3[,'NUMERIC'])
  .test.residual.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NON_NEGATIVE_NUMERIC'])
  .test.residual.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'],ds.test_env$local.values.2[,'POSITIVE_NUMERIC'],ds.test_env$local.values.3[,'POSITIVE_NUMERIC'])
  .test.residual.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NEGATIVE_NUMERIC'])
   
})

# context("ds.mean::math::residual::multiple")
test_that("residual deviation tends to 0",
{
  connect.all.datasets()
  
  .test.residual.combined('D$INTEGER',ds.test_env$local.values[,'INTEGER'])
  .test.residual.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,'NON_NEGATIVE_INTEGER'])
  .test.residual.combined('D$POSITIVE_INTEGER',ds.test_env$local.values[,'POSITIVE_INTEGER'])
  .test.residual.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values[,'NEGATIVE_INTEGER']) 
  .test.residual.combined('D$NUMERIC',ds.test_env$local.values[,'NUMERIC'])
  .test.residual.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,'NON_NEGATIVE_NUMERIC'])
  .test.residual.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values[,'POSITIVE_NUMERIC'])
  .test.residual.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,'NEGATIVE_NUMERIC']) 
  
  .test.residual.split('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'],ds.test_env$local.values.2[,'INTEGER'],ds.test_env$local.values.3[,'INTEGER'])
  .test.residual.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NON_NEGATIVE_INTEGER'])
  .test.residual.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'],ds.test_env$local.values.2[,'POSITIVE_INTEGER'],ds.test_env$local.values.3[,'POSITIVE_INTEGER'])
  .test.residual.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NEGATIVE_INTEGER'])
  .test.residual.split('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'],ds.test_env$local.values.2[,'NUMERIC'],ds.test_env$local.values.3[,'NUMERIC'])
  .test.residual.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NON_NEGATIVE_NUMERIC'])
  .test.residual.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'],ds.test_env$local.values.2[,'POSITIVE_NUMERIC'],ds.test_env$local.values.3[,'POSITIVE_NUMERIC'])
  .test.residual.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NEGATIVE_NUMERIC'])
  
})


# context("ds.mean::math::location_parameter::single")
test_that("mean(X+a) - mean(X) = a",
{
  connect.dataset.1()
  
  .test.location.parameter('D$INTEGER')
  .test.location.parameter('D$NON_NEGATIVE_INTEGER')
  .test.location.parameter('D$POSITIVE_INTEGER')
  .test.location.parameter('D$NEGATIVE_INTEGER') 
  .test.location.parameter('D$NUMERIC')
  .test.location.parameter('D$NON_NEGATIVE_NUMERIC')
  .test.location.parameter('D$POSITIVE_NUMERIC')
  .test.location.parameter('D$NEGATIVE_NUMERIC') 
})

# context("ds.mean::math::location_parameter::multiple")
test_that("mean(X+a) - mean(X) = a",
{
  connect.all.datasets()
  
  .test.location.parameter('D$INTEGER')
  .test.location.parameter('D$NON_NEGATIVE_INTEGER')
  .test.location.parameter('D$POSITIVE_INTEGER')
  .test.location.parameter('D$NEGATIVE_INTEGER') 
  .test.location.parameter('D$NUMERIC')
  .test.location.parameter('D$NON_NEGATIVE_NUMERIC')
  .test.location.parameter('D$POSITIVE_NUMERIC')
  .test.location.parameter('D$NEGATIVE_NUMERIC') 
})




# context("ds.mean::math::scale::multiple")
test_that("mean(X+a) / mean(X) = a",
{
  connect.all.datasets()
  
  .test.scale('D$INTEGER')
  .test.scale('D$NON_NEGATIVE_INTEGER')
  .test.scale('D$POSITIVE_INTEGER')
  .test.scale('D$NEGATIVE_INTEGER') 
  .test.scale('D$NUMERIC')
  .test.scale('D$NON_NEGATIVE_NUMERIC')
  .test.scale('D$POSITIVE_NUMERIC')
  .test.scale('D$NEGATIVE_NUMERIC') 
})

# context("ds.mean::math::scale::single")
test_that("mean(X*a) / mean(X) = a",
{
  connect.dataset.1()
  
  .test.scale('D$INTEGER')
  .test.scale('D$NON_NEGATIVE_INTEGER')
  .test.scale('D$POSITIVE_INTEGER')
  .test.scale('D$NEGATIVE_INTEGER') 
  .test.scale('D$NUMERIC')
  .test.scale('D$NON_NEGATIVE_NUMERIC')
  .test.scale('D$POSITIVE_NUMERIC')
  .test.scale('D$NEGATIVE_NUMERIC') 
})
