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

# context("ds.merge::arg::test errors")
test_that("merge_erros", {
    expect_error(ds.merge(), "must be the name of the first data.frame to merge", fixed=TRUE)
    expect_error(ds.merge(x.name="A"), "must be the name of the second data.frame to merge", fixed=TRUE)
    expect_error(ds.merge(x.name="D$LAB_TSC", y.name="B", by.x.names="LAB_TSC", by.y.names="LAB_TSC"), "DataSHIELD errors")
    expect_error(ds.merge(x.name="D$LAB_TSC", y.name="D$LAB_TSC", by.x.names="C"), "must be the names of the columns in y to merge on", fixed=TRUE)
    expect_error(ds.merge(x.name="D$LAB_TSC", y.name="D$LAB_TSC", by.x.names="D$LAB_TSC", by.y.names="D", suffixes=NULL), "must be the suffixes that separate duplicate column names", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
