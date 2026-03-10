#-------------------------------------------------------------------------------
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

# context("ds.gamlss::perf::setup")
connect.studies.dataset.gamlss(list("e3_bw", "e3_gac_None"))

#
# Tests
#

# context("ds.gamlss::perf:0")
test_that("combine - performance", {
    .durationSec  <- as.numeric(Sys.getenv("PERF_DURATION_SEC", unset = "30"))
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.gamlss(formula = "e3_bw ~ e3_gac_None", sigma.formula = "e3_bw ~ e3_gac_None", data = "D", family = "NO()", newobj = "perf.newobj")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.gamlss::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.gamlss::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.gamlss::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.gamlss::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.gamlss::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.gamlss::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.gamlss::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.gamlss::perf::shutdown")
disconnect.studies.dataset.gamlss()
# context("ds.gamlss::perf::done")
