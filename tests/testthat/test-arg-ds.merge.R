#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.merge::arg::test errors")
test_that("merge_erros", {
    expect_error(ds.merge(), "Please provide the name (eg 'name1') of first dataframe to be merged (called x) ", fixed=TRUE)
    expect_error(ds.merge(x.name="A"), "Please provide the name (eg 'name2') of second dataframe to be merged (called y) ", fixed=TRUE)
    expect_error(ds.merge(x.name="A", y.name="B"), "Please provide the names of columns in x dataframe on which to merge (eg c('id', 'time'))", fixed=TRUE)
    expect_error(ds.merge(x.name="A", y.name="B", by.x.names="C"), "Please provide the names of columns in y dataframe on which to merge (eg c('id', 'time'))", fixed=TRUE)
    expect_error(ds.merge(x.name="A", y.name="B", by.x.names="C", by.y.names="D", suffixes=NULL), "Please provide the suffixes to append to disambiguate duplicate column names  (default = c('.x','.y/))", fixed=TRUE)
})

#
# Done
#

disconnect.studies.dataset.cnsim()
