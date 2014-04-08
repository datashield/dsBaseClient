#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
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

context("dsbaseclient::ds.table1d")

options(datashield.variables=list("DIS_CVA","LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.table1d() generate a one dimensional table, outputting combined contingency tables")
res <- ds.table1d(datasources=opals, xvect=quote(D$DIS_CVA))
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.table1d() generate a one dimensional table, outputting study specific contingency tables")
res <- ds.table1d(datasources=opals, xvect=quote(D$DIS_CVA), type="split")
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.table1d() generate a one dimensional table, outputting study specific contingency tables for study 1 and 2")
res <- ds.table1d(datasources=opals[1:2], xvect=quote(D$DIS_CVA), type="split")
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.table1d() generate a one dimensional table, outputting study specific and combined contingency tables")
res <- ds.table1d(datasources=opals, xvect=quote(D$LAB_HDL)) 
print(res)
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")