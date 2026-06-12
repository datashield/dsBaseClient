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

# context("ds.quantileMean::perf::setup")
connect.studies.dataset.cnsim(list('LAB_HDL'))

#
# Tests
#

# context("ds.quantileMean::perf::combine:0")
test_that("combine - performance", {
    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.quantileMean("D$LAB_HDL")

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("ds.quantileMean::perf::combine:0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("ds.quantileMean::perf::combine:0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("ds.quantileMean::perf::combine:0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("ds.quantileMean::perf::combine:0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("ds.quantileMean::perf::combine:0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("ds.quantileMean::perf::combine:0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("ds.quantileMean::perf::combine:0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

# context("ds.quantileMean::perf::shutdown")
disconnect.studies.dataset.cnsim()
# context("ds.quantileMean::perf::done")
