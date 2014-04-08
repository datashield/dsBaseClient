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

context("dsbaseclient::ds.rowcolCalc")

options(datashield.variables=list("LAB_TSC", "LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.rowcolCalc()")
datashield.assign(opals, "hdl_tsc", quote(data.frame(cbind(D$LAB_HDL, D$LAB_TSC))))
ds.rowcolCalc(datasources=opals, dataset=quote(hdl_tsc), operation="rowSums", newobj="rsum_hdl_tsc")
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")