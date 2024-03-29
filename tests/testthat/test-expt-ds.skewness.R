#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------



source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-ds.skewness.R")

context("ds.skewness::expt::combine::multiple")

test_that("combined data set",
          {
            connect.all.datasets()
            .test.skewness.combined('D$INTEGER',ds.test_env$local.values[,'INTEGER'], method = 1)
            .test.skewness.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,'NON_NEGATIVE_INTEGER'], method = 1)
            .test.skewness.combined('D$POSITIVE_INTEGER',ds.test_env$local.values[,'POSITIVE_INTEGER'], method = 1)
            .test.skewness.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values[,'NEGATIVE_INTEGER'], method = 1) 
            .test.skewness.combined('D$NUMERIC',ds.test_env$local.values[,'NUMERIC'], method = 1)
            .test.skewness.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,'NON_NEGATIVE_NUMERIC'], method = 1)
            .test.skewness.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values[,'POSITIVE_NUMERIC'], method = 1)
            .test.skewness.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,'NEGATIVE_NUMERIC'], method = 1) 
          })

context("ds.skewness::expt::split::multiple")
test_that("split data set",
          {
            connect.all.datasets()
            .test.skewness.split('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'],ds.test_env$local.values.2[,'INTEGER'],ds.test_env$local.values.3[,'INTEGER'], method = 2)
            .test.skewness.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NON_NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NON_NEGATIVE_INTEGER'], method = 2)
            .test.skewness.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'],ds.test_env$local.values.2[,'POSITIVE_INTEGER'],ds.test_env$local.values.3[,'POSITIVE_INTEGER'], method = 2)
            .test.skewness.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER'],ds.test_env$local.values.2[,'NEGATIVE_INTEGER'],ds.test_env$local.values.3[,'NEGATIVE_INTEGER'], method = 2)
            .test.skewness.split('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'],ds.test_env$local.values.2[,'NUMERIC'],ds.test_env$local.values.3[,'NUMERIC'], method = 2)
            .test.skewness.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NON_NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NON_NEGATIVE_NUMERIC'], method = 2)
            .test.skewness.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'],ds.test_env$local.values.2[,'POSITIVE_NUMERIC'],ds.test_env$local.values.3[,'POSITIVE_NUMERIC'], method = 2)
            .test.skewness.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.2[,'NEGATIVE_NUMERIC'],ds.test_env$local.values.3[,'NEGATIVE_NUMERIC'], method = 2)
          })

context("ds.skewness::expt::single")
test_that("combined data set",
          {
            connect.dataset.1()
            .test.skewness.combined('D$INTEGER',ds.test_env$local.values.1[,'INTEGER'], method = 3)
            .test.skewness.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NON_NEGATIVE_INTEGER'], method = 3)
            .test.skewness.combined('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,'POSITIVE_INTEGER'], method = 3)
            .test.skewness.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,'NEGATIVE_INTEGER'], method = 3) 
            .test.skewness.combined('D$NUMERIC',ds.test_env$local.values.1[,'NUMERIC'], method = 3)
            .test.skewness.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NON_NEGATIVE_NUMERIC'], method = 3)
            .test.skewness.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,'POSITIVE_NUMERIC'], method = 3)
            .test.skewness.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,'NEGATIVE_NUMERIC'], method = 3) 
          })
