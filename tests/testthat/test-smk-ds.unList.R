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

context("ds.unList::smk::simple test")
test_that("simple test", {
    ds.asList(x.name="D$GENDER")

    res <- ds.unList("D$GENDER.list")

    expect_length(res, 3)
    expect_length(res$sim1, 2)
    expect_equal(res$sim1$return.message, "New object <D$GENDER.list.unlist> created")
    expect_equal(res$sim1$class.of.newobj, "Class of <D$GENDER.list.unlist> is 'factor'")
    expect_length(res$sim2, 2)
    expect_equal(res$sim2$return.message, "New object <D$GENDER.list.unlist> created")
    expect_equal(res$sim2$class.of.newobj, "Class of <D$GENDER.list.unlist> is 'factor'")
    expect_length(res$sim3, 2)
    expect_equal(res$sim3$return.message, "New object <D$GENDER.list.unlist> created")
    expect_equal(res$sim3$class.of.newobj, "Class of <D$GENDER.list.unlist> is 'factor'")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
