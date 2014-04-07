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

context("dsbaseclient::ds.histogram")

options(datashield.variables=list("LAB_TSC"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.histogram(type=combine)")

hist <- ds.histogram(datasources=opals, xvect=quote(D$LAB_TSC))
#print(hist)
# TODO do more than a smoke test

context("dsbaseclient::ds.histogram(type=split)")

hist <- ds.histogram(datasources=opals, xvect=quote(D$LAB_TSC), type="split")
#print(hist)
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")