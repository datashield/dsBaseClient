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

context("dsbaseclient::ds.table2d")

options(datashield.variables=list("DIS_DIAB", "DIS_CVA", "GENDER", "LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.table2d() generate a two dimensional table, outputting combined contingency tables - default behaviour")
res <- ds.table2d(datasources=opals, xvect=quote(D$DIS_DIAB), yvect=quote(D$GENDER))
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.table2d() generate a two dimensional table, outputting study specific contingency tables")
res <- ds.table2d(datasources=opals, xvect=quote(D$DIS_DIAB), yvect=quote(D$GENDER), type="split")
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.table2d() generate a two dimensional table, outputting study specific contingency tables for the first two studies")
res <- ds.table2d(datasources=opals[1:2], quote(D$DIS_DIAB), quote(D$GENDER), type="split")
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.table2d() generate a two dimensional table, outputting combined contingency tables (in this case some studies are invalid)")
res <- ds.table2d(datasources=opals, quote(D$DIS_CVA), quote(D$GENDER))
print(res)
# TODO do more than a smoke test

context("dsbaseclient::ds.table2d() generate a two dimensional table, outputting study specific contingency tables (in this case some studies are invalid)")
res <- ds.table2d(datasources=opals, quote(D$DIS_CVA), quote(D$GENDER), type="split")
print(res)
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")