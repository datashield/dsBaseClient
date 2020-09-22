#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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
            .test.skewness.combined('D$INTEGER',ds.test_env$local.values[,6], method = 1)
            .test.skewness.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values[,7], method = 1)
            .test.skewness.combined('D$POSITIVE_INTEGER',ds.test_env$local.values[,8], method = 1)
            .test.skewness.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values[,9], method = 1) 
            .test.skewness.combined('D$NUMERIC',ds.test_env$local.values[,10], method = 1)
            .test.skewness.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values[,11], method = 1)
            .test.skewness.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values[,12], method = 1)
            .test.skewness.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values[,13], method = 1) 
          })

context("ds.skewness::expt::split::multiple")
test_that("split data set",
          {
            connect.all.datasets()
            .test.skewness.split('D$INTEGER',ds.test_env$local.values.1[,6],ds.test_env$local.values.2[,6],ds.test_env$local.values.3[,6], method = 2)
            .test.skewness.split('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7],ds.test_env$local.values.2[,7],ds.test_env$local.values.3[,7], method = 2)
            .test.skewness.split('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8],ds.test_env$local.values.2[,8],ds.test_env$local.values.3[,8], method = 2)
            .test.skewness.split('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9],ds.test_env$local.values.2[,9],ds.test_env$local.values.3[,9], method = 2)
            .test.skewness.split('D$NUMERIC',ds.test_env$local.values.1[,10],ds.test_env$local.values.2[,10],ds.test_env$local.values.3[,10], method = 2)
            .test.skewness.split('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11],ds.test_env$local.values.2[,11],ds.test_env$local.values.3[,11], method = 2)
            .test.skewness.split('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12],ds.test_env$local.values.2[,12],ds.test_env$local.values.3[,12], method = 2)
            .test.skewness.split('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13],ds.test_env$local.values.2[,13],ds.test_env$local.values.3[,13], method = 2)
          })

context("ds.skewness::expt::single")
test_that("combined data set",
          {
            connect.dataset.1()
            .test.skewness.combined('D$INTEGER',ds.test_env$local.values.1[,6], method = 3)
            .test.skewness.combined('D$NON_NEGATIVE_INTEGER',ds.test_env$local.values.1[,7], method = 3)
            .test.skewness.combined('D$POSITIVE_INTEGER',ds.test_env$local.values.1[,8], method = 3)
            .test.skewness.combined('D$NEGATIVE_INTEGER',ds.test_env$local.values.1[,9], method = 3) 
            .test.skewness.combined('D$NUMERIC',ds.test_env$local.values.1[,10], method = 3)
            .test.skewness.combined('D$NON_NEGATIVE_NUMERIC',ds.test_env$local.values.1[,11], method = 3)
            .test.skewness.combined('D$POSITIVE_NUMERIC',ds.test_env$local.values.1[,12], method = 3)
            .test.skewness.combined('D$NEGATIVE_NUMERIC',ds.test_env$local.values.1[,13], method = 3) 
          })
