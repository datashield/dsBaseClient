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

# context('ds.dataFrameSort::expt::setup')

source('connection_to_datasets/init_testing_datasets.R')
source('definition_tests/def-ds.dataFrameSort.R')

# context("ds.dataFrameSort::expt::multiple::correct_parameter_class")
test_that('all datasets',
          {  
            connect.all.datasets()
            .test.function.parameters("D","D$LOGICAL",TRUE,"numeric","server.data")
            .test.function.parameters("D","D$LOGICAL",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$INTEGER",TRUE,"numeric","server.data")
            .test.function.parameters("D","D$INTEGER",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$POSITIVE_INTEGER",FALSE,"alphabetic","server.data")
            .test.function.parameters("D","D$NEGATIVE_INTEGER",TRUE,"numeric","server.data")
            .test.function.parameters("D","D$NUMERIC",TRUE,"numeric","server.data")
            .test.function.parameters("D","D$POSITIVE_NUMERIC",FALSE,"numeric","server.data")
            .test.function.parameters("D","D$NEGATIVE_NUMERIC",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$IDENTIFIER",FALSE,"numeric","server.data")
            .test.function.parameters("D","D$CATEGORY",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$NUMERIC_ONE_CHANGE",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$INTEGER_ONE_CHANGE",TRUE,"numeric","server.data")
          })
# context("ds.dataFrameSort::expt::single::correct_parameter_class")
test_that('dataset 2',
          {  
            connect.dataset.2()
            .test.function.parameters("D","D$LOGICAL",TRUE,"numeric","server.data")
            .test.function.parameters("D","D$LOGICAL",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$INTEGER",TRUE,"numeric","server.data")
            .test.function.parameters("D","D$INTEGER",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$POSITIVE_INTEGER",FALSE,"alphabetic","server.data")
            .test.function.parameters("D","D$NEGATIVE_INTEGER",TRUE,"numeric","server.data")
            .test.function.parameters("D","D$NUMERIC",TRUE,"numeric","server.data")
            .test.function.parameters("D","D$POSITIVE_NUMERIC",FALSE,"numeric","server.data")
            .test.function.parameters("D","D$NEGATIVE_NUMERIC",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$IDENTIFIER",FALSE,"numeric","server.data")
            .test.function.parameters("D","D$CATEGORY",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$NUMERIC_ONE_CHANGE",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","D$INTEGER_ONE_CHANGE",TRUE,"numeric","server.data")
          })
# context("ds.dataFrameSort::expt::multiple::incorrect_parameter_class")
test_that('all datasets',
          {  
            connect.all.datasets()
            .test.function.parameters(D,"D$LOGICAL",TRUE,"numeric","server.data")
            .test.function.parameters(2,"D$INTEGER",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","LHD",TRUE,"alphabetic","server.data")
            .test.function.parameters("K","D$NEGATIVE_INTEGER",FALSE,"numeric","server.data")
            #.test.function.parameters("D","D$NUMERIC",10,"numeric","server.data")
            .test.function.parameters("D","D$POSITIVE_NUMERIC","D","numeric","server.data")
            #.test.function.parameters("D","D$NEGATIVE_NUMERIC",TRUE,23,"server.data")
            .test.function.parameters("D","D$IDENTIFIER",TRUE,"CHARACTER","server.data")
            .test.function.parameters("D","D$CATEGORY",FALSE,"numeric","server.data")
            .test.function.parameters("D","D$NUMERIC_ONE_CHANGE",TRUE,"alphabetic",23)
            .test.function.parameters("dataframe","D$INTEGER_ONE_CHANGE",TRUE,"numeric","server.data")
          })
# context("ds.dataFrameSort::expt::single::incorrect_parameter_class")
test_that('dataset 2',
          {  
            connect.dataset.2()
            .test.function.parameters(D,"D$LOGICAL",TRUE,"numeric","server.data")
            .test.function.parameters(2,"D$INTEGER",TRUE,"alphabetic","server.data")
            .test.function.parameters("D","LHD",TRUE,"alphabetic","server.data")
            .test.function.parameters("K","D$NEGATIVE_INTEGER",FALSE,"numeric","server.data")
            #.test.function.parameters("D","D$NUMERIC",10,"numeric","server.data")
            .test.function.parameters("D","D$POSITIVE_NUMERIC","D","numeric","server.data")
            #.test.function.parameters("D","D$NEGATIVE_NUMERIC",TRUE,23,"server.data")
            .test.function.parameters("D","D$IDENTIFIER",TRUE,"CHARACTER","server.data")
            .test.function.parameters("D","D$CATEGORY",FALSE,"numeric","server.data")
            .test.function.parameters("D","D$NUMERIC_ONE_CHANGE",TRUE,"alphabetic",23)
            .test.function.parameters("dataframe","D$INTEGER_ONE_CHANGE",TRUE,"numeric","server.data")
          })

# context("ds.dataFrameSort::expt::multiple::df::creation")
test_that('all datasets',
          {  
            connect.all.datasets()
            .test.data.frame.creation("D","LOGICAL",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","INTEGER",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","POSITIVE_INTEGER",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","NEGATIVE_INTEGER",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","NUMERIC",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","POSITIVE_NUMERIC",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","NEGATIVE_NUMERIC",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","IDENTIFIER",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","CATEGORY",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","NUMERIC_ONE_CHANGE",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","INTEGER_ONE_CHANGE",TRUE,"numeric","server.data")
          })
# context("ds.dataFrameSort::expt::single::df::creation")
test_that('dataset 3',
          {  
            connect.dataset.3()
            .test.data.frame.creation("D","LOGICAL",FALSE,"numeric","server.data")
            .test.data.frame.creation("D","INTEGER",TRUE,"alphabetic","server.data")
            .test.data.frame.creation("D","POSITIVE_INTEGER",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","NEGATIVE_INTEGER",TRUE,"alphabetic","server.data")
            .test.data.frame.creation("D","NUMERIC",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","POSITIVE_NUMERIC",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","NEGATIVE_NUMERIC",FALSE,"alphabetic","server.data")
            .test.data.frame.creation("D","IDENTIFIER",TRUE,"alphabetic","server.data")
            .test.data.frame.creation("D","CATEGORY",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","NUMERIC_ONE_CHANGE",TRUE,"numeric","server.data")
            .test.data.frame.creation("D","INTEGER_ONE_CHANGE",FALSE,"alphabetic","server.data")
          })
# context("ds.dataFrameSort::expt::multiple::numeric::ascending")
test_that('all datasets',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
            }
            .test.data.frame.sorting("D","LOGICAL",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_INTEGER",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_INTEGER",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_NUMERIC",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_NUMERIC",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","IDENTIFIER",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","CATEGORY",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC_ONE_CHANGE",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER_ONE_CHANGE",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","FACTOR_INTEGER",FALSE,"numeric","server.data",local.df.list)
            #.test.data.frame.sorting("D","FACTOR_CHARACTER",FALSE,"numeric","server.data",local.df.list)
            #.test.data.frame.sorting("D","CHARACTER",FALSE,"numeric","server.data",local.df.list)
            
            
          })
# context("ds.dataFrameSort::expt::single::numeric::ascending")
test_that('dataset 1',
          {  
            connect.dataset.1()
            local.df.list<-list(ds.test_env$local.values.1[,-1])
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
            }
            .test.data.frame.sorting("D","LOGICAL",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_INTEGER",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_INTEGER",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_NUMERIC",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_NUMERIC",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","IDENTIFIER",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","CATEGORY",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC_ONE_CHANGE",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER_ONE_CHANGE",FALSE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","FACTOR_INTEGER",FALSE,"numeric","server.data",local.df.list)
            #.test.data.frame.sorting("D","FACTOR_CHARACTER",FALSE,"numeric","server.data",local.df.list)
            #.test.data.frame.sorting("D","CHARACTER",FALSE,"numeric","server.data",local.df.list)
            
            
          })
# context("ds.dataFrameSort::expt::multiple::numeric::descending")
test_that('all datasets',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
            }
            .test.data.frame.sorting("D","LOGICAL",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_INTEGER",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_INTEGER",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_NUMERIC",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_NUMERIC",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","IDENTIFIER",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","CATEGORY",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC_ONE_CHANGE",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER_ONE_CHANGE",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","FACTOR_INTEGER",TRUE,"numeric","server.data",local.df.list)
            #.test.data.frame.sorting("D","FACTOR_CHARACTER",TRUE,"numeric","server.data",local.df.list)
            #.test.data.frame.sorting("D","CHARACTER",TRUE,"numeric","server.data",local.df.list)
            
            
          })
# context("ds.dataFrameSort::expt::single::numeric::descending")
test_that('dataset 1',
          {  
            connect.dataset.1()
            local.df.list<-list(ds.test_env$local.values.1[,-1])
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
            }
            .test.data.frame.sorting("D","LOGICAL",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_INTEGER",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_INTEGER",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_NUMERIC",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_NUMERIC",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","IDENTIFIER",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","CATEGORY",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC_ONE_CHANGE",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER_ONE_CHANGE",TRUE,"numeric","server.data",local.df.list)
            .test.data.frame.sorting("D","FACTOR_INTEGER",TRUE,"numeric","server.data",local.df.list)
            #.test.data.frame.sorting("D","FACTOR_CHARACTER",TRUE,"numeric","server.data",local.df.list)
            #.test.data.frame.sorting("D","CHARACTER",TRUE,"numeric","server.data",local.df.list)
            
            
          })


# context("ds.dataFrameSort::expt::multiple::alphabetic::ascending")
test_that('all datasets',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            server.FC_levels<-ds.levels("D$FACTOR_CHARACTER")
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
              levels(local.df.list[[i]][,"FACTOR_CHARACTER"])<-server.FC_levels[[i]]
            }
            .test.data.frame.sorting("D","LOGICAL",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_INTEGER",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_INTEGER",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_NUMERIC",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_NUMERIC",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","IDENTIFIER",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","CATEGORY",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC_ONE_CHANGE",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER_ONE_CHANGE",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","FACTOR_INTEGER",FALSE,"alphabetic","server.data",local.df.list)
            #.test.data.frame.sorting("D","FACTOR_CHARACTER",FALSE,"alphabetic","server.data",local.df.list) 
            .test.data.frame.sorting("D","CHARACTER",FALSE,"alphabetic","server.data",local.df.list)
          })
# context("ds.dataFrameSort::expt::single::alphabetic::ascending")
test_that('dataset 2',
          {  
            connect.dataset.2()
            local.df.list<-list(ds.test_env$local.values.2[,-1])
            server.FC_levels<-ds.levels("D$FACTOR_CHARACTER")
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
              levels(local.df.list[[i]][,"FACTOR_CHARACTER"])<-server.FC_levels[[i]]
            }
            .test.data.frame.sorting("D","LOGICAL",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_INTEGER",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_INTEGER",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_NUMERIC",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_NUMERIC",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","IDENTIFIER",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","CATEGORY",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC_ONE_CHANGE",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER_ONE_CHANGE",FALSE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","FACTOR_INTEGER",FALSE,"alphabetic","server.data",local.df.list)
            #.test.data.frame.sorting("D","FACTOR_CHARACTER",FALSE,"alphabetic","server.data",local.df.list) 
            .test.data.frame.sorting("D","CHARACTER",FALSE,"alphabetic","server.data",local.df.list)
          })
# context("ds.dataFrameSort::expt::multiple::alphabetic::descending")
test_that('all datasets',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
            }
            .test.data.frame.sorting("D","LOGICAL",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_INTEGER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_INTEGER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_NUMERIC",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_NUMERIC",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","IDENTIFIER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","CATEGORY",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC_ONE_CHANGE",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER_ONE_CHANGE",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","FACTOR_INTEGER",TRUE,"alphabetic","server.data",local.df.list)
            #.test.data.frame.sorting("D","FACTOR_CHARACTER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","CHARACTER",TRUE,"alphabetic","server.data",local.df.list)
          })
# context("ds.dataFrameSort::expt::single::alphabetic::descending")
test_that('dataset 3',
          {  
            connect.dataset.3()
            local.df.list<-list(ds.test_env$local.values.3[,-1])
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
            }
            .test.data.frame.sorting("D","LOGICAL",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_INTEGER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_INTEGER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","POSITIVE_NUMERIC",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NEGATIVE_NUMERIC",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","IDENTIFIER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","CATEGORY",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","NUMERIC_ONE_CHANGE",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","INTEGER_ONE_CHANGE",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","FACTOR_INTEGER",TRUE,"alphabetic","server.data",local.df.list)
            #.test.data.frame.sorting("D","FACTOR_CHARACTER",TRUE,"alphabetic","server.data",local.df.list)
            .test.data.frame.sorting("D","CHARACTER",TRUE,"alphabetic","server.data",local.df.list)
          })


# context('ds.dataFrameSort::expt::shutdown')


# context('ds.dataFrameSort::expt::done')
