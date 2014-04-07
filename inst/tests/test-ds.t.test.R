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

context("dsbaseclient::ds.t.test")

options(datashield.variables=list("LAB_TSC","LAB_HDL"))
source("setup.R")

#
# Tests
#

context("dsbaseclient::ds.t.test(type=combine)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC))
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=split)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), type="split")
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=combine,paired=TRUE)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), paired=TRUE)
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=split,paired=TRUE)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), paired=TRUE, type='split')
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=combine,alternative=greater)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), alternative='greater')
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=combine,alternative=less)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), alternative='less')
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=combine,mu=-4)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), mu=-4)
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=combine,var.equal=TRUE)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), var.equal=TRUE)
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=combine,conf.level=0.90)")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL), y=quote(D$LAB_TSC), conf.level=0.90)
# TODO do more than a smoke test

context("dsbaseclient::ds.t.test(type=combine) [one sample]")
ds.t.test(datasources=opals, x=quote(D$LAB_HDL))
# TODO do more than a smoke test

#
# Tear down
#

source("teardown.R")