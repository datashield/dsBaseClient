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

connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.recodeValues::arg::test errors")
test_that("recodeValues_erros", {
    expect_error(ds.recodeValues(), "Please provide the name of the variable to be recoded: eg 'xxx'", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC'), "Please provide a vector in the 'values2replace.vector' argument specifying\n         the values to be replaced eg c(1,7)", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', values2replace.vector=c(0,1)), "Please ensure that values2replace.vector and new.values.vector have same length and are in the same order", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', values2replace.vector=c(0,1), new.values.vector=c(-10)), "Please ensure that values2replace.vector and new.values.vector have same length and are in the same order", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', values2replace.vector=c(1,1), new.values.vector=c(-10, 10)), "No value may appear more than once in the values2replace.vector", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', values2replace.vector=c(NA), new.values.vector=c(10)), "To recode NAs you need to use the 'missing' argument", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', missing=99), "Please provide a vector in the 'values2replace.vector' argument specifying\n         the values to be replaced eg c(1,7)", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
