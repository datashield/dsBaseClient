#-------------------------------------------------------------------------------
# Copyright (c) 2019-2020 University of Newcastle upon Tyne. All rights reserved.
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

context('ds.dataFrameSort::expt_dgr::setup')

source('connection_to_datasets/init_testing_datasets.R')
source('definition_tests/def-ds.dataFrameSort.R')

#
# Tests
#

context("ds.dataFrameSort::math_dgr::closure::multiple")
test_that('server set %in% local set',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            for(i in 1:length(local.df.list)){
              local.df.list[[i]][,"CHARACTER"]<-as.character(local.df.list[[i]][,"CHARACTER"])
              local.df.list[[i]][,"FACTOR_INTEGER"]<-as.factor(local.df.list[[i]][,"FACTOR_INTEGER"])
              colnames(local.df.list[[i]])<-ds.colnames("D",datasources = ds.test_env$connections[i][[1]])
            }
            .closure.test("D","LOGICAL",FALSE,"numeric","server.data",local.df.list)
          })