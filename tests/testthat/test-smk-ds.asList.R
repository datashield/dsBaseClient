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

connect.studies.dataset.cnsim(list("GENDER"))

#
# Tests
#

context("ds.asList::smk::simple test")
test_that("simple test", {
    res <- ds.asList(x.name="D$GENDER")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_equal(res$sim1$return.message, "New object <D$GENDER.list> created")
    expect_equal(res$sim1$class.of.newobj, "Class of <D$GENDER.list> is 'list'")
    expect_length(res$sim2, 2)
    expect_equal(res$sim2$return.message, "New object <D$GENDER.list> created")
    expect_equal(res$sim2$class.of.newobj, "Class of <D$GENDER.list> is 'list'")
    expect_length(res$sim3, 2)
    expect_equal(res$sim3$return.message, "New object <D$GENDER.list> created")
    expect_equal(res$sim3$class.of.newobj, "Class of <D$GENDER.list> is 'list'")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
