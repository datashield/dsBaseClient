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
    expect_error(ds.recodeValues(), "must be the name of the variable to recode", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC'), "must specify the values to replace", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', values2replace.vector=c(0,1)), "must have the same", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', values2replace.vector=c(0,1), new.values.vector=c(-10)), "must have the same", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', values2replace.vector=c(1,1), new.values.vector=c(-10, 10)), "must not contain duplicate values", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', values2replace.vector=c(NA), new.values.vector=c(10)), "to recode", fixed=TRUE)
    expect_error(ds.recodeValues('D$LAB_TSC', missing=99), "must specify the values to replace", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
