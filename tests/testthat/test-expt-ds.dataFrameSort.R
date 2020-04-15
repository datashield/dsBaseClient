#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------


context("ds.dataFrameSort::expt::setup")

source('connection_to_datasets/init_testing_datasets.R')
#source('definition_tests/def-ds.dataFrameSort.R')

###!!! test the data frame creation

context("ds.dataFrameSort::expt::numeric::increasing")
test_that("combined data set",
{
  connect.all.datasets()
#  .sort.numeric.increasing("D","INTEGER")
})

context("ds.dataFrameSort::expt::done")
