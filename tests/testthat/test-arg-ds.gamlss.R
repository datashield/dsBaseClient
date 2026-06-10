#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
# Copyright (c) 2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.gamlss(list("e3_bw", "e3_gac_None"))

#
# Tests
#

# context("ds.gamlss::arg::test errors")
test_that("gamlss_errors", {
    expect_error(ds.gamlss(), " Please provide a valid formula!", fixed=TRUE)
    expect_error(ds.gamlss(formula="e3_bw ~ e3_gac_None", method="RG"), "Argument 'method' must be either 'RS', 'CG' or 'mixed'", fixed=TRUE)
    expect_error(ds.gamlss(formula="e3_bw ~ e3_gac_None", method="RS", centiles=TRUE), "Provide the name of the explanatory variable in 'xvar' argument", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.gamlss()
