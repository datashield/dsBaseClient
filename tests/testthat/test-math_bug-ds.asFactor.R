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
#

source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-ds.asFactor.R")

# context("ds.asFactor::math::multiple")
test_that("every factor should be unique",
{
  connect.all.datasets()
  .test.uniqueness('D$INTEGER','INT.f')
  .test.uniqueness('D$NON_NEGATIVE_INTEGER','NON_NEG_INT.f')
  .test.uniqueness('D$POSITIVE_INTEGER', 'POS_INT.f')
  .test.uniqueness('D$NEGATIVE_INTEGER','NEG_INT.f')
  .test.uniqueness('D$NUMERIC','NUM.f')
  .test.uniqueness('D$NON_NEGATIVE_NUMERIC','NON_NEG_NUM.f')
  .test.uniqueness('D$POSITIVE_NUMERIC','POS_NUM.f')
  .test.uniqueness('D$NEGATIVE_NUMERIC','NEG_NUM.f')
  .test.uniqueness('D$FACTOR_INTEGER', 'FACTOR_INT.f')
})

# context("ds.asFactor::math::multiple")
test_that("every factor should be unique",
{
   connect.dataset.1()
   .test.uniqueness('D$INTEGER','INT.f')
   .test.uniqueness('D$NON_NEGATIVE_INTEGER','NON_NEG_INT.f')
   .test.uniqueness('D$POSITIVE_INTEGER', 'POS_INT.f')
   .test.uniqueness('D$NEGATIVE_INTEGER','NEG_INT.f')
   .test.uniqueness('D$NUMERIC','NUM.f')
   .test.uniqueness('D$NON_NEGATIVE_NUMERIC','NON_NEG_NUM.f')
   .test.uniqueness('D$POSITIVE_NUMERIC','POS_NUM.f')
   .test.uniqueness('D$NEGATIVE_NUMERIC','NEG_NUM.f')
   .test.uniqueness('D$FACTOR_INTEGER', 'FACTOR_INT.f')
})
