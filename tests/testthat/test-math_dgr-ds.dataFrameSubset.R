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

# context('ds.dataFrameSubset::expt_dgr::setup')

source('connection_to_datasets/init_testing_datasets.R')
source('definition_tests/def-ds.dataFrameSubset.R')

#
# Tests
#

# context("ds.dataFrameSubset::math_dgr::closure::multiple")
test_that('server set %in% local set',
          {  
            connect.all.datasets()
            local.df.list<-list(ds.test_env$local.values.1[,-1],ds.test_env$local.values.2[,-1],ds.test_env$local.values.3[,-1])
            
#            .closure.test("D","INTEGER","NUMERIC",">",TRUE,"subset.server",local.df.list)
#            .closure.test("D","INTEGER","NUMERIC",">",FALSE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=",TRUE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=",FALSE,"subset.server",local.df.list)
#            .closure.test("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=",TRUE,"subset.server",local.df.list)
#            .closure.test("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=",FALSE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=",TRUE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=",FALSE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=",TRUE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=",FALSE,"subset.server",local.df.list)
          })

# context("ds.dataFrameSubset::math_dgr::closure::simple")
test_that('server set %in% local set',
          {  
            connect.dataset.2()
            local.df.list<-list(ds.test_env$local.values.2[,-1])
            
#            .closure.test("D","INTEGER","NUMERIC",">",TRUE,"subset.server",local.df.list)
#            .closure.test("D","INTEGER","NUMERIC",">",FALSE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=",TRUE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER","POSITIVE_NUMERIC",">=",FALSE,"subset.server",local.df.list)
#            .closure.test("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=",TRUE,"subset.server",local.df.list)
#            .closure.test("D","NEGATIVE_INTEGER","NEGATIVE_NUMERIC","<=",FALSE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=",TRUE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_INTEGER","NON_NEGATIVE_INTEGER","<=",FALSE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=",TRUE,"subset.server",local.df.list)
#            .closure.test("D","POSITIVE_NUMERIC","NON_NEGATIVE_NUMERIC","<=",FALSE,"subset.server",local.df.list)
          })
