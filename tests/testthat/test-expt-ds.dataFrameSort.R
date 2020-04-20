#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

source('connection_to_datasets/init_testing_datasets.R')
source('definition_tests/def-ds.dataFrameSort.R')

context("ds.dataFrameSort()::expt::multiple::df::creation")
test_that('numeric data',
          {  
            connect.all.datasets()
            .test.data.frame.creation("D","INTEGER",TRUE,"numeric","server.data")
          })
context("ds.dataFrameSort()::expt::multiple::numeric::ascending")
test_that('numeric data',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            .test.data.frame.sorting("D","INTEGER",FALSE,"numeric","server.data",local.df.list)
          })
context("ds.dataFrameSort()::expt::multiple::numeric::descending")
test_that('numeric data',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            .test.data.frame.sorting("D","INTEGER",TRUE,"numeric","server.data",local.df.list)
          })
context("ds.dataFrameSort()::expt::multiple::alphabetic::ascending")
test_that('numeric data',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            .test.data.frame.sorting("D","INTEGER",FALSE,"alphabetic","server.data",local.df.list)
          })
context("ds.dataFrameSort()::expt::multiple::alphabetic::descending")
test_that('numeric data',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            .test.data.frame.sorting("D","INTEGER",TRUE,"alphabetic","server.data",local.df.list)
          })
context("ds.dataFrameSort()::expt::multiple::alphabetic::ascending")
test_that('character data',
          {  
            #Convert character column into a character file
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
            }
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            .test.data.frame.sorting("D","CHARACTER",FALSE,"alphabetic","server.data",local.df.list)
          })




