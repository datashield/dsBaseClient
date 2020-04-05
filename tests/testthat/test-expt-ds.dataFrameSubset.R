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
  .test.data.frame.creation("D","INTEGER","NUMERIC",">","subset.server")
  .test.data.frame.creation("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=","subset.server")
  .test.data.frame.creation("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=","subset.server")
  .test.data.frame.creation("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER","<=","subset.server")
  .test.data.frame.creation("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=","subset.server")
  .test.data.frame.creation("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=","subset.server")
  #.test.data.frame.creation("D","INTEGER","INTEGER_ONE_CHANGE","==","subset.server")
  #.test.data.frame.creation("D","NUMERIC","NUMERIC_ONE_CHANGE","==","subset.server")
})

context("ds.dataFrameSubset()::expt::multiple::all_columns::with_NA")
test_that("numeric data",
{ 
  connect.all.datasets()
  subset.by.rows.NA("D","INTEGER","NUMERIC",">","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.rows.NA("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.rows.NA("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.rows.NA("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER","<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.rows.NA("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.rows.NA("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  #subset.by.rows.NA("D","INTEGER","INTEGER_ONE_CHANGE","==","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  #subset.by.rows.NA("D","NUMERIC","NUMERIC_ONE_CHANGE","==","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  
})


context("ds.dataFrameSubset()::expt::multiple::all_columns::without_NA")
test_that("numeric data",
{ 
 connect.all.datasets()
 subset.by.rows.noNA("D","INTEGER","NUMERIC",">","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.noNA("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.noNA("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.noNA("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER","<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.noNA("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.noNA("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 #subset.by.rows.noNA("D","INTEGER","INTEGER_ONE_CHANGE","==","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 #subset.by.rows.noNA("D","NUMERIC","NUMERIC_ONE_CHANGE","==","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
            
})

context("ds.dataFrameSubset()::expt::multiple::some_columns::with_NA")
test_that("numeric data",
{ 
 connect.all.datasets()
 subset.by.rows.cols.NA("D","INTEGER","NUMERIC",1:4,">","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.cols.NA("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.cols.NA("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 #subset.by.rows.cols.NA("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.cols.NA("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.cols.NA("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 #subset.by.rows.cols.NA("D","INTEGER","INTEGER_ONE_CHANGE",c(3:8,11:18),"==","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 #subset.by.rows.cols.NA("D","NUMERIC","NUMERIC_ONE_CHANGE",c(4:10,14:18),"==","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
})

context("ds.dataFrameSubset()::expt::multiple::some_columns::without_NA")
test_that("numeric data",
{ 
 connect.all.datasets()
 subset.by.rows.cols.nonNA("D","INTEGER","NUMERIC",1:4,">","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.cols.nonNA("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.cols.nonNA("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:13),"<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
# subset.by.rows.cols.nonNA("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.cols.nonNA("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 subset.by.rows.cols.nonNA("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 #subset.by.rows.cols.nonNA("D","INTEGER","INTEGER_ONE_CHANGE",c(3:8,11:18),"==","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
 #subset.by.rows.cols.nonNA("D","NUMERIC","NUMERIC_ONE_CHANGE",c(4:10,14:18),"==","subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
})

context("ds.dataFrameSubset()::expt::multiple::some_columns::with_NA")
test_that("numeric data",
{ 
 connect.all.datasets()
  subset.by.cols.NA("D","INTEGER",1:4,"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.NA("D","NEGATIVE_INTEGER",c(2,5,10:15),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.NA("D","NON_NEGATIVE_INTEGER",c(2,5,10:15),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.NA("D","NUMERIC",c(3:7,10,15),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.NA("D","POSITIVE_NUMERIC",c(2:5,12:14),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.NA("D","NON_NEGATIVE_NUMERIC",c(1:7,13:16),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
})

context("ds.dataFrameSubset()::expt::multiple::some_columns::without_NA")
test_that("numeric data",
{ 
 connect.all.datasets()
  subset.by.cols.nonNA("D","INTEGER",1:4,"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.nonNA("D","NEGATIVE_INTEGER",c(2,5,10:15),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.nonNA("D","NON_NEGATIVE_INTEGER",c(2,5,10:15),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.nonNA("D","NUMERIC",c(3:7,10,15),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.nonNA("D","POSITIVE_NUMERIC",c(2:5,12:14),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
  subset.by.cols.nonNA("D","NON_NEGATIVE_NUMERIC",c(1:7,13:16),"subset.server",list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1]))
})



          