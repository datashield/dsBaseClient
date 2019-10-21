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

    expect_length(res, 2)
    expect_equal(res$is.object.created, "A data object <D$GENDER.list.unlist> has been created in all specified data sources")
    expect_equal(res$validity.check, "<D$GENDER.list.unlist> appears valid in all sources")
})

context("ds.unList::smk::simple test")
test_that("simple test", {
    ds.asList(x.name="D$GENDER", newobj="GENDER.list")

    res <- ds.unList("GENDER.list")

    expect_length(res, 3)
    expect_equal(res$is.object.created, "A data object <GENDER.list.unlist> has been created in all specified data sources")
    expect_equal(res$validity.check, "<GENDER.list.unlist> invalid in at least one source. See studyside.messages:")
    expect_length(res$studyside.messages, 3)
    expect_equal(res$studyside.messages$sim1, "Outcome object is a list without names. So a studysideMessage may be hidden. Please check output is OK")
    expect_equal(res$studyside.messages$sim2, "Outcome object is a list without names. So a studysideMessage may be hidden. Please check output is OK")
    expect_equal(res$studyside.messages$sim3, "Outcome object is a list without names. So a studysideMessage may be hidden. Please check output is OK")
})

#
# Done
#

disconnect.studies.dataset.cnsim()
