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
source("definition_tests/def-ds.dataFrameSubset.R")


context("ds.dataFrameSubset()::expt::multiple::all_columns")
test_that('numeric data',
{  
   connect.all.datasets()
   print(ds.colnames("D"))
  .test.data.frame.creation("D","INTEGER","NUMERIC",">","subset.server")
  .test.data.frame.creation("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=","subset.server")
  .test.data.frame.creation("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=","subset.server")
  .test.data.frame.creation("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER","<=","subset.server")
  .test.data.frame.creation("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=","subset.server")
  .test.data.frame.creation("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=","subset.server")
})






          