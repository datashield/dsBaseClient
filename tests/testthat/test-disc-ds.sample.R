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

# context("ds.sample::disc::setup")

connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

# context("ds.sample::disc::test disclosure")
test_that("cov_erros", {
    expect_error(ds.sample(x="This_line_is_very_very_very_very_very_very_long", size=1234, newobj="obj1"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    res1.errors <- DSI::datashield.errors()

    expect_length(res1.errors, 3)
    expect_match(res1.errors$sim1, "* Error : FAILED: the character string denoting the argument <x> is too long and may be disclosive - please shorten")
    expect_match(res1.errors$sim2, "* Error : FAILED: the character string denoting the argument <x> is too long and may be disclosive - please shorten")
    expect_match(res1.errors$sim3, "* Error : FAILED: the character string denoting the argument <x> is too long and may be disclosive - please shorten")

    expect_error(ds.sample(x="D", size=1234, prob="This_line_is_very_very_very_very_very_very_long", newobj="obj2"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    res2.errors <- DSI::datashield.errors()

    expect_length(res2.errors, 3)
    expect_match(res2.errors$sim1, "* Error : FAILED: the character string denoting the argument <prob> is too long and may be disclosive - please shorten")
    expect_match(res2.errors$sim2, "* Error : FAILED: the character string denoting the argument <prob> is too long and may be disclosive - please shorten")
    expect_match(res2.errors$sim3, "* Error : FAILED: the character string denoting the argument <prob> is too long and may be disclosive - please shorten")

    expect_error(ds.sample(x="D", size=2, newobj="obj3"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    res3.errors <- DSI::datashield.errors()

    expect_length(res3.errors, 3)
    expect_match(res3.errors$sim1, "* Error : FAILED: disclosure risk, as the length of the subset to be created is less than nfilter.subset")
    expect_match(res3.errors$sim2, "* Error : FAILED: disclosure risk, as the length of the subset to be created is less than nfilter.subset")
    expect_match(res3.errors$sim3, "* Error : FAILED: disclosure risk, as the length of the subset to be created is less than nfilter.subset")

    expect_error(ds.sample(x="D", size=2162, newobj="obj4"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    res4.errors <- DSI::datashield.errors()

    expect_length(res4.errors, 1)
    expect_match(res4.errors$sim1, "* Error : FAILED: disclosure risk using differencing: original object length minus subset length less than nfilter.subset")

    expect_error(ds.sample(x="D", size=3086, newobj="obj5"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)
    res5.errors <- DSI::datashield.errors()

    expect_length(res5.errors, 2)
    expect_match(res5.errors$sim1, "* Error : FAILED: if sampling without replacement size must be less than or equal to length\\(x\\)")
    expect_match(res5.errors$sim2, "* Error : FAILED: disclosure risk using differencing: original object length minus subset length less than nfilter.subset")
})

#
# Shutdown
#

# context("ds.sample::disc::shutdown")

disconnect.studies.dataset.cnsim()

#
# Done
#

# context("ds.sample::disc::done")
