#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.ranksSecure::arg::setup")

connect.all.datasets()

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.ranksSecure::arg::missing variable")
test_that("missing variable", {
    expect_error(ds.ranksSecure("LAB_MISSING"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$study1, "* object 'LAB_MISSING' not found")
    expect_match(res.errors$study2, "* object 'LAB_MISSING' not found")
    expect_match(res.errors$study3, "* object 'LAB_MISSING' not found")
})

# context("ds.ranksSecure::arg::NULL variable")
test_that("NULL variable", {
    DSI::datashield.assign.expr(conns = ds.test_env$connections, symbol = "LAB_NULL", expr = "NULL")

    res.class <- ds.class("LAB_NULL")

    expect_length(res.class, 3)
    expect_equal(res.class$study1, "NULL")
    expect_equal(res.class$study2, "NULL")
    expect_equal(res.class$study3, "NULL")

    expect_error(expect_warning(ds.ranksSecure("LAB_NULL"), "no non-missing arguments to max; returning -Inf", fixed = TRUE), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$study1, "* Error in stats::complete.cases\\(input.var\\) : \n  no input has determined the number of cases")
    expect_match(res.errors$study2, "* Error in stats::complete.cases\\(input.var\\) : \n  no input has determined the number of cases")
    expect_match(res.errors$study3, "* Error in stats::complete.cases\\(input.var\\) : \n  no input has determined the number of cases")
})

# context("ds.ranksSecure::arg::'text' variable")
test_that("'text' variable", {
    expect_error(ds.ranksSecure("D$CHARACTER"), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$study1, "* Error in \\(1 - h\\) \\* qs\\[i\\] : non-numeric argument to binary operator")
    expect_match(res.errors$study2, "* Error in \\(1 - h\\) \\* qs\\[i\\] : non-numeric argument to binary operator")
    expect_match(res.errors$study3, "* Error in \\(1 - h\\) \\* qs\\[i\\] : non-numeric argument to binary operator")
})

# context("ds.ranksSecure::arg::'logical' variable")
test_that("'logical' variable", {
    expect_error(ds.ranksSecure("D$LOGICAL"), "FAILED: one of the extreme quantile estimates is NA probably because of a cluster of values at one end of the range of possible values. Try setting a narrower range of quantile values via the <quantiles.for.estimation> argument", fixed = TRUE)
})

# context("ds.ranksSecure::arg::'integer factor' variable")
test_that("'integer factor' variable", {
    expect_error(expect_warning(ds.ranksSecure("D$INTEGER_FACTOR"), "no non-missing arguments to max; returning -Inf", fixed = TRUE), "There are some DataSHIELD errors, list them with datashield.errors()", fixed = TRUE)

    res.errors <- DSI::datashield.errors()

    expect_length(res.errors, 3)
    expect_match(res.errors$study1, "* Error in stats::complete.cases\\(input.var\\) : \n  no input has determined the number of cases")
    expect_match(res.errors$study2, "* Error in stats::complete.cases\\(input.var\\) : \n  no input has determined the number of cases")
    expect_match(res.errors$study3, "* Error in stats::complete.cases\\(input.var\\) : \n  no input has determined the number of cases")
})

#
# Done
#

# context("ds.ranksSecure::arg::shutdown")

test_that("setup", {
    ds_expect_variables(c("D", "LAB_NULL", "input.mean.sd.df", "min.max.df", "summary.ranks.df", "testvar.ranks"))
})

disconnect.all.datasets()

# context("ds.ranksSecure::arg::done")
