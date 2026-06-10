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
source("definition_tests/def-ds.var.R")

# context("ds.var::math::positive_result::multiple")
test_that("variance >=0",
{
  connect.all.datasets()
  .test.variance.positive.combine('D$INTEGER')
  .test.variance.positive.combine('D$NON_NEGATIVE_INTEGER')
  .test.variance.positive.combine('D$POSITIVE_INTEGER')
  .test.variance.positive.combine('D$NEGATIVE_INTEGER')
  .test.variance.positive.combine('D$NUMERIC')
  .test.variance.positive.combine('D$NON_NEGATIVE_NUMERIC')
  .test.variance.positive.combine('D$POSITIVE_NUMERIC')
  .test.variance.positive.combine('D$NEGATIVE_NUMERIC')
  
  .test.variance.positive.split('D$INTEGER')
  .test.variance.positive.split('D$NON_NEGATIVE_INTEGER')
  .test.variance.positive.split('D$POSITIVE_INTEGER')
  .test.variance.positive.split('D$NEGATIVE_INTEGER')
  .test.variance.positive.split('D$NUMERIC')
  .test.variance.positive.split('D$NON_NEGATIVE_NUMERIC')
  .test.variance.positive.split('D$POSITIVE_NUMERIC')
  .test.variance.positive.split('D$NEGATIVE_NUMERIC')
})


# context("ds.var::math::positive_result::single")
test_that("variance >=0",
{
  connect.dataset.1()
  .test.variance.positive.combine('D$INTEGER')
  .test.variance.positive.combine('D$NON_NEGATIVE_INTEGER')
  .test.variance.positive.combine('D$POSITIVE_INTEGER')
  .test.variance.positive.combine('D$NEGATIVE_INTEGER')
  .test.variance.positive.combine('D$NUMERIC')
  .test.variance.positive.combine('D$NON_NEGATIVE_NUMERIC')
  .test.variance.positive.combine('D$POSITIVE_NUMERIC')
  .test.variance.positive.combine('D$NEGATIVE_NUMERIC')
  
  .test.variance.positive.split('D$INTEGER')
  .test.variance.positive.split('D$NON_NEGATIVE_INTEGER')
  .test.variance.positive.split('D$POSITIVE_INTEGER')
  .test.variance.positive.split('D$NEGATIVE_INTEGER')
  .test.variance.positive.split('D$NUMERIC')
  .test.variance.positive.split('D$NON_NEGATIVE_NUMERIC')
  .test.variance.positive.split('D$POSITIVE_NUMERIC')
  .test.variance.positive.split('D$NEGATIVE_NUMERIC')
})


# context("ds.var::math::square_root_std::single")
test_that("variance is to the power of 2 of the standard deviation",
{
  connect.dataset.1()
  .test.standard.dev.combine('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'])
  .test.standard.dev.combine('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'])
  .test.standard.dev.combine('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'])
  .test.standard.dev.combine('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER'])
  .test.standard.dev.combine('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'])
  .test.standard.dev.combine('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'])
  .test.standard.dev.combine('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'])
  .test.standard.dev.combine('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC'])
  
})


# context("ds.var::math::square_root_std::multiple")
test_that("variance is to the power of 2 of the standard deviation",
 {
   connect.all.datasets()
   .test.standard.dev.combine('D$INTEGER',ds.test_env$local.values[,'INTEGER'])
   .test.standard.dev.combine('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,'NON_NEGATIVE_INTEGER'])
   .test.standard.dev.combine('D$POSITIVE_INTEGER',ds.test_env$local.values[,'POSITIVE_INTEGER'])
   .test.standard.dev.combine('D$NEGATIVE_INTEGER',ds.test_env$local.values[,'NEGATIVE_INTEGER'])
   .test.standard.dev.combine('D$NUMERIC',ds.test_env$local.values[,'NUMERIC'])
   .test.standard.dev.combine('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,'NON_NEGATIVE_NUMERIC'])
   .test.standard.dev.combine('D$POSITIVE_NUMERIC',ds.test_env$local.values[,'POSITIVE_NUMERIC'])
   .test.standard.dev.combine('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,'NEGATIVE_NUMERIC'])
   
   .test.standard.dev.split('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'],ds.test_env$local.values.2[,'INTEGER'],ds.test_env$local.values.3[,'INTEGER'])
   .test.standard.dev.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NON_NEGATIVE_INTEGER'])
   .test.standard.dev.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'],ds.test_env$local.values.2[,'POSITIVE_INTEGER'],ds.test_env$local.values.3[,'POSITIVE_INTEGER'])
   .test.standard.dev.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NEGATIVE_INTEGER'])
   .test.standard.dev.split('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'],ds.test_env$local.values.2[,'NUMERIC'],ds.test_env$local.values.3[,'NUMERIC'])
   .test.standard.dev.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NON_NEGATIVE_NUMERIC'])
   .test.standard.dev.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'],ds.test_env$local.values.2[,'POSITIVE_NUMERIC'],ds.test_env$local.values.3[,'POSITIVE_NUMERIC'])
   .test.standard.dev.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NEGATIVE_NUMERIC'])
 })

# context("ds.var::math::location::parameter::single")
test_that("var (X+a) = Var(X)",
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

# context("ds.var::math::location::parameter::multiple")
test_that("var (X+a) = Var(X)",
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

# context("ds.var::math::scale::single")
test_that("var (aX) = a^2Var(X)",
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

# context("ds.var::math::scale::multiple")
test_that("var (aX) = a^2Var(X)",
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
