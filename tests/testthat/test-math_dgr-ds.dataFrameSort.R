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

# context('ds.dataFrameSort::expt_dgr::setup')

source('connection_to_datasets/init_testing_datasets.R')
source('definition_tests/def-ds.dataFrameSort.R')

#
# Tests
#

# context("ds.dataFrameSort::math_dgr::closure::multiple")
test_that('server set %in% local set',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1,ds.test_env$local.values.2,ds.test_env$local.values.3)
            
#            .closure.test("D","LOGICAL",FALSE,"numeric","server.data",local.df.list)
#            .closure.test("D","LOGICAL",TRUE,"alphabetic","server.data",local.df.list)
#            .closure.test("D","NUMERIC",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","INTEGER",TRUE,"alphabetic","server.data",local.df.list)
#            .closure.test("D","POSITIVE_NUMERIC",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER",TRUE,"alphabetic","server.data",local.df.list)
#            .closure.test("D","CATEGORY",TRUE,"alphabetic","server.data",local.df.list)
#            .closure.test("D","NEGATIVE_NUMERIC",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","CHARACTER",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","NEGATIVE_INTEGER",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","IDENTIFIER",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","NUMERIC_ONE_CHANGE",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","INTEGER_ONE_CHANGE",TRUE,"numeric","server.data",local.df.list)
            
          })

# context("ds.dataFrameSort::math_dgr::closure::simple")
test_that('server set %in% local set',
          {  
            connect.dataset.1()
            local.df.list<-list(ds.test_env$local.values.1)
            
#            .closure.test("D","LOGICAL",FALSE,"numeric","server.data",local.df.list)
#            .closure.test("D","LOGICAL",TRUE,"alphabetic","server.data",local.df.list)
#            .closure.test("D","NUMERIC",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","INTEGER",TRUE,"alphabetic","server.data",local.df.list)
#            .closure.test("D","POSITIVE_NUMERIC",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER",TRUE,"alphabetic","server.data",local.df.list)
#            .closure.test("D","CATEGORY",TRUE,"alphabetic","server.data",local.df.list)
#            .closure.test("D","NEGATIVE_NUMERIC",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","CHARACTER",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","NEGATIVE_INTEGER",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","IDENTIFIER",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","NUMERIC_ONE_CHANGE",TRUE,"numeric","server.data",local.df.list)
#            .closure.test("D","INTEGER_ONE_CHANGE",TRUE,"numeric","server.data",local.df.list)
            
          })
