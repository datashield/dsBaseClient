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
# Set up
#

# context("ds.dataFrameSubset::expt_dgr::setup")

source("connection_to_datasets/init_testing_datasets.R")
source("definition_tests/def-ds.dataFrameSubset.R")

#
# Tests
#

# context("ds.dataFrameSubset::expt_dgr::multiple::parameter_class")
test_that('all datasets',
          {  
            connect.all.datasets()
            .test.function.parameters("D","D$INTEGER","D$NUMERIC",">",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters("K","D$INTEGER","D$NUMERIC",">",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters(2,"D$INTEGER","D$NUMERIC",">",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters("D",10,"D$NUMERIC",">",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters("D","D$POSITIVE_INTEGER",30,"<",1:3,7:9,TRUE,"subset.server")
            #.test.function.parameters("D","D$POSITIVE_INTEGER","D$NUMERIC",10,1:3,7:9,TRUE,"subset.server")
            #.test.function.parameters("D","D$POSITIVE_INTEGER","D$POSITIVE_NUMERIC","!=",1:3,7:9,10,"subset.server")
            .test.function.parameters("D","D$POSITIVE_INTEGER","D$NON_NEGATIVE_INTEGER","!=",1:3,7:9,FALSE,20)
            #.test.function.parameters("D","D$HOLA","D$NON_NEGATIVE_INTEGER","!=",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters("D","INTEGER","D$NON_NEGATIVE_INTEGER","!=",1:3,7:9,FALSE,"subset.server")
            #.test.function.parameters("D","D$POSITIVE_INTEGER","D$NON_NEGATIVE_INTEGER","!=","INTEGER",7:9,FALSE,"subset.server")
            #.test.function.parameters("D","D$POSITIVE_INTEGER","D$NON_NEGATIVE_INTEGER","!=",7:9,"INTEGER",FALSE,"subset.server")
            
            
          })

# context("ds.dataFrameSubset::expt_dgr::single::parameter_class")
test_that('dataset 1',
          {  
            connect.dataset.1()
            .test.function.parameters("D","D$INTEGER","D$NUMERIC",">",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters("K","D$INTEGER","D$NUMERIC",">",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters(2,"D$INTEGER","D$NUMERIC",">",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters("D",10,"D$NUMERIC",">",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters("D","D$POSITIVE_INTEGER",30,"<",1:3,7:9,TRUE,"subset.server")
            #.test.function.parameters("D","D$POSITIVE_INTEGER","D$NUMERIC",10,1:3,7:9,TRUE,"subset.server")
            #.test.function.parameters("D","D$POSITIVE_INTEGER","D$POSITIVE_NUMERIC","!=",1:3,7:9,10,"subset.server")
            .test.function.parameters("D","D$POSITIVE_INTEGER","D$NON_NEGATIVE_INTEGER","!=",1:3,7:9,FALSE,20)
            #.test.function.parameters("D","D$HOLA","D$NON_NEGATIVE_INTEGER","!=",1:3,7:9,FALSE,"subset.server")
            .test.function.parameters("D","INTEGER","D$NON_NEGATIVE_INTEGER","!=",1:3,7:9,FALSE,"subset.server")
            #.test.function.parameters("D","D$POSITIVE_INTEGER","D$NON_NEGATIVE_INTEGER","!=","INTEGER",7:9,FALSE,"subset.server")
            #.test.function.parameters("D","D$POSITIVE_INTEGER","D$NON_NEGATIVE_INTEGER","!=",7:9,"INTEGER",FALSE,"subset.server")
            
            
          })

# context("ds.dataFrameSubset::expt_dgr::multiple::all_columns")
test_that('all datasets',
          {  
            connect.all.datasets()
            .test.data.frame.creation("D","INTEGER","NUMERIC",">","subset.server")
            .test.data.frame.creation("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=","subset.server")
            .test.data.frame.creation("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=","subset.server")
            .test.data.frame.creation("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER","<=","subset.server")
            .test.data.frame.creation("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=","subset.server")
            .test.data.frame.creation("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=","subset.server")
          })

# context("ds.dataFrameSubset::expt_dgr::single::all_columns")
test_that('dataset 2',
          {  
            connect.dataset.2()
            .test.data.frame.creation("D","INTEGER","NUMERIC",">","subset.server")
            .test.data.frame.creation("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=","subset.server")
            .test.data.frame.creation("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=","subset.server")
            .test.data.frame.creation("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER","<=","subset.server")
            .test.data.frame.creation("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=","subset.server")
            .test.data.frame.creation("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=","subset.server")
          })

# context("ds.dataFrameSubset::expt_dgr::multiple::all_columns::subset_by_rows")
test_that("all datasets",
          { 
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
            subset.by.rows("D","INTEGER","NUMERIC",">",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","INTEGER","NUMERIC",">",FALSE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=",FALSE,"subset.server",local.df.list)
            subset.by.rows("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=",FALSE,"subset.server",local.df.list)
            #subset.by.rows("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",TRUE,"<=","subset.server",local.df.list)
            #subset.by.rows("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",FALSE,"<=","subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=",FALSE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=",FALSE,"subset.server",local.df.list)
          })

# context("ds.dataFrameSubset::expt_dgr::single::all_columns::subset_by_rows")
test_that("dataset 3",
          { 
            connect.dataset.3()
            local.df.list<-list(ds.test_env$local.values.3)
            subset.by.rows("D","INTEGER","NUMERIC",">",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","INTEGER","NUMERIC",">",FALSE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=",FALSE,"subset.server",local.df.list)
            subset.by.rows("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=",FALSE,"subset.server",local.df.list)
            #subset.by.rows("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",TRUE,"<=","subset.server",local.df.list)
            #subset.by.rows("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",FALSE,"<=","subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=",FALSE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=",TRUE,"subset.server",local.df.list)
            subset.by.rows("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=",FALSE,"subset.server",local.df.list)
          })

# context("ds.dataFrameSubset::expt_dgr::multiple::subset_by_rows_columns")
test_that("all datasets",
          { 
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
            subset.by.rows.cols("D","INTEGER","NUMERIC",1:4,">",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","INTEGER","NUMERIC",1:4,">",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=",FALSE,"subset.server",local.df.list)
            #subset.by.rows.cols("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=",TRUE,"subset.server",local.df.list)
            #subset.by.rows.cols("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=",FALSE,"subset.server",local.df.list)
          })

# context("ds.dataFrameSubset::expt_dgr::single::subset_by_rows_columns")
test_that("dataset 1",
          { 
            connect.dataset.1()
            local.df.list<-list(ds.test_env$local.values.1)
            subset.by.rows.cols("D","INTEGER","NUMERIC",1:4,">",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","INTEGER","NUMERIC",1:4,">",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=",FALSE,"subset.server",local.df.list)
            #subset.by.rows.cols("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=",TRUE,"subset.server",local.df.list)
            #subset.by.rows.cols("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=",FALSE,"subset.server",local.df.list)
          })


# context("ds.dataFrameSubset::expt_dgr::multiple::subset_by_rows_columns")
test_that("all datasets",
          { 
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
            subset.by.rows.cols("D","INTEGER","NUMERIC",1:4,">",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","INTEGER","NUMERIC",1:4,">",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=",FALSE,"subset.server",local.df.list)
            #subset.by.rows.cols("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=",TRUE,"subset.server",local.df.list)
            #subset.by.rows.cols("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=",FALSE,"subset.server",local.df.list)
          })
# context("ds.dataFrameSubset::expt_dgr::single::subset_by_rows_columns")
test_that("dataset 2",
          { 
            connect.dataset.2()
            local.df.list<-list(ds.test_env$local.values.2)
            subset.by.rows.cols("D","INTEGER","NUMERIC",1:4,">",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","INTEGER","NUMERIC",1:4,">",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",c(1,4,6:9),">=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC",c(2,4,9:16),"<=",FALSE,"subset.server",local.df.list)
            #subset.by.rows.cols("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=",TRUE,"subset.server",local.df.list)
            #subset.by.rows.cols("D","NON_NEGATIVE_NUMERIC","NON_NEGATIVE_INTEGER",c(3,6,10:13),"<=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER",c(1:7,10:13),"<=",FALSE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=",TRUE,"subset.server",local.df.list)
            subset.by.rows.cols("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC",1:15,"<=",FALSE,"subset.server",local.df.list)
          })

# context("ds.dataFrameSubset::expt_dgr::multiple::all_rows_subset_by_columns")
test_that("all datasets",
          {  connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
            subset.by.cols("D","INTEGER",1:4,TRUE,"subset.server",local.df.list)
            subset.by.cols("D","NEGATIVE_INTEGER",c(2,5,10:15),FALSE,"subset.server",local.df.list)
            subset.by.cols("D","NON_NEGATIVE_INTEGER",c(2,5,10:15),TRUE,"subset.server",local.df.list)
            subset.by.cols("D","NUMERIC",c(3:7,10,15),FALSE,"subset.server",local.df.list)
            subset.by.cols("D","POSITIVE_NUMERIC",c(2:5,12:14),TRUE,"subset.server",local.df.list)
            subset.by.cols("D","NON_NEGATIVE_NUMERIC",c(1:7,13:16),FALSE,"subset.server",local.df.list)
          })

# context("ds.dataFrameSubset::expt_dgr::multiple::all_rows_subset_by_columns")
test_that("dataset 3",
          {  connect.dataset.3()
            local.df.list<-list(ds.test_env$local.values.3)
            subset.by.cols("D","INTEGER",1:4,TRUE,"subset.server",local.df.list)
            subset.by.cols("D","NEGATIVE_INTEGER",c(2,5,10:15),FALSE,"subset.server",local.df.list)
            subset.by.cols("D","NON_NEGATIVE_INTEGER",c(2,5,10:15),TRUE,"subset.server",local.df.list)
            subset.by.cols("D","NUMERIC",c(3:7,10,15),FALSE,"subset.server",local.df.list)
            subset.by.cols("D","POSITIVE_NUMERIC",c(2:5,12:14),TRUE,"subset.server",local.df.list)
            subset.by.cols("D","NON_NEGATIVE_NUMERIC",c(1:7,13:16),FALSE,"subset.server",local.df.list)
          })

#
# Shutdown
#

#
# Done
#
