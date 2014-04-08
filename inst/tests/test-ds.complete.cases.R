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

context("dsbaseclient::ds.complete.cases")

options(datashield.variables=NULL)
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.complete.cases() create a vector with indices for complete observations (rows)")
ds.complete.cases(datasources=opals, x=quote(D))
# TODO do more than a smoke test

context("dsbaseclient::ds.complete.cases() create a vector with indices for complete observations for LAB_TSC and PM_BMI_CONTINUOUS variables")
input = list(quote(D$LAB_TSC),quote(D$PM_BMI_CONTINUOUS))
ds.complete.cases(datasources=opals, x=input, newobj='TSC_BMI_complete')
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")