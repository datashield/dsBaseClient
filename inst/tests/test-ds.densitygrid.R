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

context("dsbaseclient::ds.densitygrid")

options(datashield.variables=list("LAB_TSC","LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.densitygrid(type=combine)")
grid <- ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="combine")
print(grid)
# TODO do more than a smoke test

context("dsbaseclient::ds.densitygrid(type=split)")
grid <- ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split")
print(grid)
# TODO do more than a smoke test

context("dsbaseclient::ds.densitygrid(type=split, numints=15)")
grid <- ds.densitygrid(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split", numints=15)
print(grid)
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")