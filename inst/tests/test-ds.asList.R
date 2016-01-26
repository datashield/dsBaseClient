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

options(datashield.variables=list("LAB_TSC","GENDER"))
context("dsBaseClient::ds.asList")

source("setup.R")

#
# Tests
#

context("dsBaseClient::ds.asList() turn the data frame D into a list")
ds.asList(datasources=opals, x='D')
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")