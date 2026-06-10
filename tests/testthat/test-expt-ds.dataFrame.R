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

# context('ds.dataFrame::expt::setup')

source('connection_to_datasets/init_testing_datasets.R')
source('definition_tests/def-ds.data.frame.R')

# context('ds.dataFrame::expt::one_column::single')
test_that('numeric data',
{
  connect.dataset.2()
  .test.data.frame.creation(c('D$NUMERIC'),'numeric_1_df')
  .test.data.frame.creation(c('D$NON_NEGATIVE_NUMERIC'),'numeric_2_df')
  .test.data.frame.creation(c('D$POSITIVE_NUMERIC'),'numeric_3_df')
  .test.data.frame.creation(c('D$NEGATIVE_NUMERIC'),'numeric_4_df')
  .test.data.frame.creation(c('D$INTEGER'),'integer_df')
  .test.data.frame.creation(c('D$NON_NEGATIVE_INTEGER'),'integer_5_df')
  .test.data.frame.creation(c('D$POSITIVE_INTEGER'),'integer_6_df')
  .test.data.frame.creation(c('D$NEGATIVE_INTEGER'),'integer_7_df')
})

# context('ds.dataFrame::expt::several_columns::single')
test_that('numeric data',
{
  connect.dataset.2()
  .test.data.frame.creation(c('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','D$POSITIVE_NUMERIC','D$NEGATIVE_NUMERIC'),'numeric_8_df')
  .test.data.frame.creation(c('D$INTEGER','D$NON_NEGATIVE_INTEGER','D$POSITIVE_INTEGER','D$NEGATIVE_INTEGER'),'integer_9_df')
})

# context('ds.dataFrame::expt::one_column::multiple')
test_that('numeric data',
{
  connect.all.datasets()
  .test.data.frame.creation(c('D$NUMERIC'),'numeric_9_df')
  .test.data.frame.creation(c('D$NON_NEGATIVE_NUMERIC'),'numeric_10_df')
  .test.data.frame.creation(c('D$POSITIVE_NUMERIC'),'numeric_11_df')
  .test.data.frame.creation(c('D$NEGATIVE_NUMERIC'),'numeric_12_df')
  .test.data.frame.creation(c('D$INTEGER'),'integer_13_df')
  .test.data.frame.creation(c('D$NON_NEGATIVE_INTEGER'),'integer_14_df')
  .test.data.frame.creation(c('D$POSITIVE_INTEGER'),'integer_15_df')
  .test.data.frame.creation(c('D$NEGATIVE_INTEGER'),'integer_16_df')
})

# context('ds.dataFrame::expt::several_columns::single')
test_that('numeric data',
{
  connect.all.datasets()
  .test.data.frame.creation(c('D$NUMERIC','D$NON_NEGATIVE_NUMERIC','D$POSITIVE_NUMERIC','D$NEGATIVE_NUMERIC'),'numeric_17_df')
  .test.data.frame.creation(c('D$INTEGER','D$NON_NEGATIVE_INTEGER','D$POSITIVE_INTEGER','D$NEGATIVE_INTEGER'),'integer_17_df')
})

# context('ds.dataFrame::expt::one_column_from_objects::single')
test_that('numeric data',
{
  connect.dataset.2()
  .test.data.frame.from.objects('D$NUMERIC','numeric_created' ,'numeric_19_df')
  .test.data.frame.from.objects('D$NON_NEGATIVE_NUMERIC','numeric_created' ,'numeric_20_df')
  .test.data.frame.from.objects('D$POSITIVE_NUMERIC','numeric_created' ,'numeric_21_df')
  .test.data.frame.from.objects('D$NEGATIVE_NUMERIC','numeric_created' ,'numeric_22_df')
  .test.data.frame.from.objects('D$INTEGER','numeric_created','numeric_23_df')
  .test.data.frame.from.objects('D$NON_NEGATIVE_INTEGER','numeric_created','numeri_25c_df')
  .test.data.frame.from.objects('D$POSITIVE_INTEGER','numeric_created','numeric_26_df')
  .test.data.frame.from.objects('D$NEGATIVE_INTEGER','numeric_created','numeric_27_df')
})

# context('ds.dataFrame::expt::object::multiple')
test_that('numeric data',
{
  connect.all.datasets()
  .test.data.frame.from.objects('D$NUMERIC','numeric_created' ,'numeric_28_df')
  .test.data.frame.from.objects('D$NON_NEGATIVE_NUMERIC','numeric_created' ,'numeric_29_df')
  .test.data.frame.from.objects('D$POSITIVE_NUMERIC','numeric_created' ,'numeric_30_df')
  .test.data.frame.from.objects('D$NEGATIVE_NUMERIC','numeric_created' ,'numeric_31_df')
  .test.data.frame.from.objects('D$INTEGER','numeric_created','numeric_32_df')
  .test.data.frame.from.objects('D$NON_NEGATIVE_INTEGER','numeric_created','numeric_33_df')
  .test.data.frame.from.objects('D$POSITIVE_INTEGER','numeric_created','numeric_34_df')
  .test.data.frame.from.objects('D$NEGATIVE_INTEGER','numeric_created','numeric_35_df')
})

# context('ds.dataFrame::expt::several_objects::multiple')
test_that('several objects',
{
  connect.all.datasets()
  .test.data.frame.from.different.objects('multiple_df')
})

# context('ds.dataFrame::expt::non_numeric::multiple')
test_that("non_numeric",
{
  connect.all.datasets()
  .test.data.frame.creation(c('D$CHARACTER'),'character_df')
  .test.data.frame.creation(c('D$LOGICAL'),'boolean_df')
  
  .test.data.frame.from.objects('D$CHARACTER','character_created' ,'character_df')
  .test.data.frame.from.objects('D$LOGICAL','boolean_created' ,'boolean_df')
})

# context('ds.dataFrame::expt::non_numeric::multiple')
{
  connect.dataset.2()
  .test.data.frame.creation(c('D$CHARACTER'),'character_df')
  .test.data.frame.creation(c('D$LOGICAL'),'boolean_df')
  
  .test.data.frame.from.objects('D$CHARACTER','character_created' ,'character_df')
  .test.data.frame.from.objects('D$LOGICAL','boolean_created' ,'boolean_df')
}

# context('ds.dataFrame::expt::shutdown')

# context('ds.dataFrame::expt::done')
