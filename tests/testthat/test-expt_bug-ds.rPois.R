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

source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-ds.rPois.R")


# context("ds.rPois::expt::no seeds::single")
test_that("Poisson without seeds",
{
  connect.dataset.1()
  .test.basic.expectation(-1,'poisson_dist_1')
  .test.basic.expectation(0,'poisson_dist_2')
  .test.basic.expectation(1,'poisson_dist_3')
  .test.basic.expectation(20,'poisson_dist_4')
  .test.basic.expectation(30,'poisson_dist_5')
  .test.basic.expectation(2^31-1,'poisson_dist_6')
})


# context("ds.rPois::expt::no seeds::multiple")
test_that("Poisson without seeds",
{
  connect.all.datasets()
  .test.basic.expectation(-1,'poisson_dist_1')
  .test.basic.expectation(0,'poisson_dist_2')
  .test.basic.expectation(1,'poisson_dist_3')
  .test.basic.expectation(20,'poisson_dist_4')
  .test.basic.expectation(30,'poisson_dist_5')
  .test.basic.expectation(2^31-1,'poisson_dist_6')
})



# context("ds.rPois::expt::with seeds::single")
test_that("Poisson with seeds",
{
  
  connect.dataset.1()
  
  .test.basic.expectation.with.seeds(-1,'poisson_dist_1')
  .test.basic.expectation.with.seeds(0,'poisson_dist_2')
  .test.basic.expectation.with.seeds(1,'poisson_dist_3')
  .test.basic.expectation.with.seeds(20,'poisson_dist_4')
  .test.basic.expectation.with.seeds(30,'poisson_dist_5')
  .test.basic.expectation.with.seeds(2^31-1,'poisson_dist_6')
  .test.basic.expectation.with.seeds(2^31,'poisson_dist_7')
})

# context("ds.rPois::expt::with seeds::multiple")
test_that("Poisson with seeds",
{
  connect.all.datasets()
  .test.basic.expectation.with.seeds(-1,'poisson_dist_1')
  .test.basic.expectation.with.seeds(0,'poisson_dist_2')
  .test.basic.expectation.with.seeds(1,'poisson_dist_3')
  .test.basic.expectation.with.seeds(20,'poisson_dist_4')
  .test.basic.expectation.with.seeds(30,'poisson_dist_5')
  .test.basic.expectation.with.seeds(2^31-1,'poisson_dist_6')
  .test.basic.expectation.with.seeds(2^31,'poisson_dist_7')
  .test.too.large.seed(3)
  .test.too.negative.seed()
})


