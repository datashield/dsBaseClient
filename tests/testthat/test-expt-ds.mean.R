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


# context("ds.mean::expt::combine::multiple")
test_that("combined data set",
{
  connect.all.datasets()

  .test.mean.combined('D$INTEGER',ds.test_env$local.values[,'INTEGER'])
  .test.mean.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,'NON_NEGATIVE_INTEGER'])
  .test.mean.combined('D$POSITIVE_INTEGER',ds.test_env$local.values[,'POSITIVE_INTEGER'])
  .test.mean.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values[,'NEGATIVE_INTEGER']) 
  .test.mean.combined('D$NUMERIC',ds.test_env$local.values[,'NUMERIC'])
  .test.mean.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,'NON_NEGATIVE_NUMERIC'])
  .test.mean.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values[,'POSITIVE_NUMERIC'])
  .test.mean.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,'NEGATIVE_NUMERIC']) 
})

# context("ds.mean::expt::split::multiple")
test_that("split data set",
{
  connect.all.datasets()

  .test.mean.split('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'],ds.test_env$local.values.2[,'INTEGER'],ds.test_env$local.values.3[,'INTEGER'])
  .test.mean.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NON_NEGATIVE_INTEGER'])
  .test.mean.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'],ds.test_env$local.values.2[,'POSITIVE_INTEGER'],ds.test_env$local.values.3[,'POSITIVE_INTEGER'])
  .test.mean.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NEGATIVE_INTEGER'])
  .test.mean.split('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'],ds.test_env$local.values.2[,'NUMERIC'],ds.test_env$local.values.3[,'NUMERIC'])
  .test.mean.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NON_NEGATIVE_NUMERIC'])
  .test.mean.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'],ds.test_env$local.values.2[,'POSITIVE_NUMERIC'],ds.test_env$local.values.3[,'POSITIVE_NUMERIC'])
  .test.mean.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NEGATIVE_NUMERIC'])
})

# context("ds.mean::expt::single")
test_that("combined data set",
{
  connect.dataset.1()
  .test.mean.combined('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'])
  .test.mean.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'])
  .test.mean.combined('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'])
  .test.mean.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER']) 
  .test.mean.combined('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'])
  .test.mean.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'])
  .test.mean.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'])
  .test.mean.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC']) 
})

# context("ds.mean::expt::large_values::single")
test_that("combined data set",
{
  connect.dataset.1()
  .test.mean.large('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'])
  .test.mean.large('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'])
  .test.mean.large('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'])
  .test.mean.large('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER']) 
  .test.mean.large('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'])
  .test.mean.large('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'])
  .test.mean.large('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'])
  .test.mean.large('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC']) 
})

# context("ds.mean::expt::large_values::multiple")
test_that("combined data set",
{
  connect.all.datasets()
  .test.mean.large('D$INTEGER',ds.test_env$local.values[,'INTEGER'])
  .test.mean.large('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,'NON_NEGATIVE_INTEGER'])
  .test.mean.large('D$POSITIVE_INTEGER',ds.test_env$local.values[,'POSITIVE_INTEGER'])
  .test.mean.large('D$NEGATIVE_INTEGER',ds.test_env$local.values[,'NEGATIVE_INTEGER']) 
  .test.mean.large('D$NUMERIC',ds.test_env$local.values[,'NUMERIC'])
  .test.mean.large('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,'NON_NEGATIVE_NUMERIC'])
  .test.mean.large('D$POSITIVE_NUMERIC',ds.test_env$local.values[,'POSITIVE_NUMERIC'])
  .test.mean.large('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,'NEGATIVE_NUMERIC']) 
})

