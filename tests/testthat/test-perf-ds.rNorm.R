#-------------------------------------------------------------------------------
# Copyright (c) 2024-2025 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

# context("ds.rNorm::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TSC"))

#
# Tests
#

# context("ds.rNorm::perf::0")
test_that("performance", {
    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.rNorm(samp.size=50, mean=0, sd=1, newobj="rnorm.newobj", seed.as.integer=123)

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.rNorm::perf::0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.rNorm::perf::0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.rNorm::perf::0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.rNorm::perf::0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.rNorm::perf::0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.rNorm::perf::0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.rNorm::perf::0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.rNorm::perf::shutdown")
disconnect.studies.dataset.cnsim()
# context("ds.rNorm::perf::done")
