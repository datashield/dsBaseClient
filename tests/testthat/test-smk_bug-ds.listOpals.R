#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa,
#               2018-2022 University of Newcastle upon Tyne. All rights reserved.
#               2022-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_HDL"))

#
# Tests
#

# context("ds.listOpals::smk::check results")
test_that("check results", {
    message <- "\n*  This function lists all Opal objects in the R analysis environment\n\n\n*  There is only one set of opals available,\n that is: 'ds.test_env$connections'\n\n\n\n*  This set of Opals has been copied to create 'default.opals',\n which all DataSHIELD functions will now use by default.\n If you want to change the default Opal object,\n please run the function ds.setDefaultOpals() again. \n\n\n\n"

    expect_message(res <- ds.listOpals(), message, fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
