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
source("definition_tests/def-ds.make.R")

# context("ds.make::math::transformation::multiple")
test_that("transformation",
{
  connect.all.datasets()
  constant.value <- sample(2:200,1)
  .test.operation.mean("D$INTEGER", constant.value, "INTEGER_CREATED", "+")
  .test.operation.mean("D$INTEGER", constant.value, "INTEGER_CREATED", "-")
  .test.operation.mean("D$INTEGER", constant.value, "INTEGER_CREATED", "/")
  .test.operation.mean("D$INTEGER", constant.value, "INTEGER_CREATED", "*")
  
  constant.value <- sample(2:200,1)
  .test.operation.mean("D$POSITIVE_INTEGER", constant.value, "INTEGER_CREATED", "+")
  .test.operation.mean("D$POSITIVE_INTEGER", constant.value, "INTEGER_CREATED", "-")
  .test.operation.mean("D$POSITIVE_INTEGER", constant.value, "INTEGER_CREATED", "/")
  .test.operation.mean("D$POSITIVE_INTEGER", constant.value, "INTEGER_CREATED", "*")
  
  constant.value <- sample(2:200,1)
  .test.operation.mean("D$NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "+")
  .test.operation.mean("D$NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "-")
  .test.operation.mean("D$NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "/")
  .test.operation.mean("D$NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "*")
  
  constant.value <- sample(2:200,1)
  .test.operation.mean("D$NON_NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "+")
  .test.operation.mean("D$NON_NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "-")
  .test.operation.mean("D$NON_NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "/")
  .test.operation.mean("D$NON_NEGATIVE_INTEGER", constant.value, "INTEGER_CREATED", "*")
  
  constant.value <- sample(2:200,1)
  .test.operation.mean("D$NUMERIC", constant.value, "NUMERIC_CREATED", "+")
  .test.operation.mean("D$NUMERIC", constant.value, "NUMERIC_CREATED", "-")
  .test.operation.mean("D$NUMERIC", constant.value, "NUMERIC_CREATED", "/")
  .test.operation.mean("D$NUMERIC", constant.value, "NUMERIC_CREATED", "*")
  
  constant.value <- sample(2:200,1)
  .test.operation.mean("D$POSITIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "+")
  .test.operation.mean("D$POSITIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "-")
  .test.operation.mean("D$POSITIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "/")
  .test.operation.mean("D$POSITIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "*")
  
  constant.value <- sample(2:200,1)
  .test.operation.mean("D$NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "+")
  .test.operation.mean("D$NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "-")
  .test.operation.mean("D$NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "/")
  .test.operation.mean("D$NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "*")
  
  constant.value <- sample(2:200,1)
  .test.operation.mean("D$NON_NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "+")
  .test.operation.mean("D$NON_NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "-")
  .test.operation.mean("D$NON_NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "/")
  .test.operation.mean("D$NON_NEGATIVE_NUMERIC", constant.value, "NUMERIC_CREATED", "*")
})

