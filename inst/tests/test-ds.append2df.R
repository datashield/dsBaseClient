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

context("dsbaseclient::ds.mean")

options(datashield.variables=list("LAB_HDL","LAB_TSC"))
source("setup.R")

# generate a new variable (e.g. a mean centered LAB_HDL)
# get the mean of LAB_HDL
mean.lab_hdl <- ds.mean(opals, 'D$LAB_HDL', type='split')
# center LAB_HDL for each study
for(i in 1:length(opals)){
  call.object <- call("-", quote(D$LAB_HDL), mean.lab_hdl[[i]])
  datashield.assign(opals[i], "lab_hdl.c", call.object)
}

#
# Tests
#

context("dsbaseclient::ds.append2df() append the variable 'lab_hdl.c' to 'D' and generate 'Dnew'")
ds.append2df(datasources=opals, quote(D), quote(lab_hdl.c))
# TODO do more than a smoke test

context("dsbaseclient::ds.append2df() append the variable 'lab_hdl.c' to 'D' and update 'D'")
ds.append2df(datasources=opals, dataset=quote(D), xvect=quote(lab_hdl.c), replace=quote(TRUE))
# TODO do more than a smoke test


#
# Tear down
#

source("teardown.R")