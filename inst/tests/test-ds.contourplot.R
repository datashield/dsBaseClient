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

context("dsbaseclient::ds.contourplot")

options(datashield.variables=list("LAB_TSC","LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.contourplot(type=combine)")
ds.contourplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="combine")
# TODO do more than a smoke test

context("dsbaseclient::ds.contourplot(type=combine,show=zoomed)")
ds.contourplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="combine", show="zoomed")
# TODO do more than a smoke test

context("dsbaseclient::ds.contourplot(type=split)")
ds.contourplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split")
# TODO do more than a smoke test

context("dsbaseclient::ds.contourplot(type=split,show=zoomed)")
ds.contourplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split", show="zoomed")
# TODO do more than a smoke test

context("dsbaseclient::ds.contourplot(type=split, numints=15)")
ds.contourplot(datasources=opals, quote(D$LAB_TSC), quote(D$LAB_HDL), type="split", numints=15)
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")