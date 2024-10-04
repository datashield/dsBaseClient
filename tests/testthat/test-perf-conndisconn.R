#-------------------------------------------------------------------------------
# Copyright (c) 2024 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
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

context("conndisconn::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))
disconnect.studies.dataset.cnsim()

#
# Tests
#

context("conndisconn::perf::simple0")
test_that("simple connect - disconnect performance", {
    .durationSec  <- 120 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))
        disconnect.studies.dataset.cnsim()

        .count <- .count + 1
        .current.time <- Sys.time()
    }

    .current.rate   <- .count / (difftime(.current.time, .start.time, units = "secs")[[1]])
    .reference.rate <- perf.reference.rate("conndisconn::perf::simple0")
    if (any(length(.reference.rate) == 0) || any(is.null(.reference.rate))) {
        print(paste("conndisconn::perf::simple0 ", .current.rate, 0.5, 2.0))
        perf.reference.save("conndisconn::perf::simple0", .current.rate, 0.5, 2.0)
    } else {
        print(paste("conndisconn::perf::simple0 ", format(.current.rate, digits = 8), ", ", format(100.0 * .current.rate / .reference.rate, digits = 4), "%", sep = ''))
    }

    .reference.rate            <- perf.reference.rate("conndisconn::perf::simple0")
    .reference.tolerance.lower <- perf.reference.tolerance.lower("conndisconn::perf::simple0")
    .reference.tolerance.upper <- perf.reference.tolerance.upper("conndisconn::perf::simple0")

    expect_gt(.current.rate, .reference.rate * .reference.tolerance.lower, label = "Observed rate", expected.label = "lower threshold on rate")
    expect_lt(.current.rate, .reference.rate * .reference.tolerance.upper, label = "Observed rate", expected.label = "upper threshold on rate")
})

#
# Done
#

context("conndisconn::perf::shutdown")
disconnect.studies.dataset.cnsim()
context("conndisconn::perf::done")
