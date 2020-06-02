#-------------------------------------------------------------------------------
# Copyright (c) 2018-2020 University of Newcastle upon Tyne. All rights reserved.
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

context("ds.sample::disc::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.sample::disc::test disclosure")
test_that("cov_erros", {
    res1 <- ds.sample(x="This_line_is_very_very_very_very_very_very_long", size=1234, newobj="obj1")
    # 'FAILED: the character string denoting the argument <x> is too long and may be disclosive - please shorten'
    expect_length(res1, 3)
    expect_equal(res1$is.object.created, "A data object <obj1> has been created in all specified data sources", fixed=TRUE)
    expect_equal(res1$validity.check, "<obj1> invalid in at least one source. See studyside.messages:", fixed=TRUE)
    expect_length(res1$studyside.messages, 3)
    expect_equal(res1$studyside.messages$sim1, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res1$studyside.messages$sim2, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res1$studyside.messages$sim3, "NOT ALL OK: there are studysideMessage(s) on this datasource")

    res2 <- ds.sample(x="D", size=1234, prob="This_line_is_very_very_very_very_very_very_long", newobj="obj2")
    # 'FAILED: the character string denoting the argument <prob> is too long and may be disclosive - please shorten'
    expect_length(res2, 3)
    expect_equal(res2$is.object.created, "A data object <obj2> has been created in all specified data sources", fixed=TRUE)
    expect_equal(res2$validity.check, "<obj2> invalid in at least one source. See studyside.messages:", fixed=TRUE)
    expect_length(res2$studyside.messages, 3)
    expect_equal(res2$studyside.messages$sim1, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res2$studyside.messages$sim2, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res2$studyside.messages$sim3, "NOT ALL OK: there are studysideMessage(s) on this datasource")

    res3 <- ds.sample(x="D", size=2, newobj="obj3")
    # 'FAILED: disclosure risk, as the length of the subset to be created is less than nfilter.subset'
    expect_length(res3, 3)
    expect_equal(res3$is.object.created, "A data object <obj3> has been created in all specified data sources", fixed=TRUE)
    expect_equal(res3$validity.check, "<obj3> invalid in at least one source. See studyside.messages:", fixed=TRUE)
    expect_length(res3$studyside.messages, 3)
    expect_equal(res3$studyside.messages$sim1, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res3$studyside.messages$sim2, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res3$studyside.messages$sim3, "NOT ALL OK: there are studysideMessage(s) on this datasource")

    res4 <- ds.sample(x="D", size=2162, newobj="obj4")
    # 'FAILED: disclosure risk using differencing: original object length minus subset length less than nfilter.subset'
    expect_length(res4, 3)
    expect_equal(res4$is.object.created, "A data object <obj4> has been created in all specified data sources", fixed=TRUE)
    expect_equal(res4$validity.check, "<obj4> invalid in at least one source. See studyside.messages:", fixed=TRUE)
    expect_length(res4$studyside.messages, 3)
    expect_equal(res4$studyside.messages$sim1, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res4$studyside.messages$sim2, "ALL OK: there are no studysideMessage(s) on this datasource")
    expect_equal(res4$studyside.messages$sim3, "ALL OK: there are no studysideMessage(s) on this datasource")

    res5 <- ds.sample(x="D", size=3086, newobj="obj5")
    # 'FAILED: disclosure risk using differencing: original object length minus subset length less than nfilter.subset'
    expect_length(res5, 3)
    expect_equal(res5$is.object.created, "A data object <obj5> has been created in all specified data sources", fixed=TRUE)
    expect_equal(res5$validity.check, "<obj5> invalid in at least one source. See studyside.messages:", fixed=TRUE)
    expect_length(res5$studyside.messages, 3)
    expect_equal(res5$studyside.messages$sim1, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res5$studyside.messages$sim2, "NOT ALL OK: there are studysideMessage(s) on this datasource")
    expect_equal(res5$studyside.messages$sim3, "ALL OK: there are no studysideMessage(s) on this datasource")
})

#
# Shutdown
#

context("ds.sample::disc::shutdown")

disconnect.studies.dataset.cnsim()

#
# Done
#

context("ds.sample::disc::done")
