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

context("dsbaseclient::ds.asCharacter")

options(datashield.variables=list("GENDER"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.asCharacter() turn the factor variable 'GENDER' into a character vector")
ds.asCharacter(datasources=opals, xvect=quote(D$GENDER), newobj="gender_as_char")
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")