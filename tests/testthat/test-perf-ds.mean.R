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

context("ds.mean::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.mean::perf::combine:0")
test_that("combine - performance", {
    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.mean("D$LAB_TSC", type='combine')

        .count <- .count + 1
        .current.time <- Sys.time()
    }
    expect_true(TRUE)

    print(paste("ds.mean::perf::combine::0:", format(.count / (difftime(.current.time, .start.time, units = "secs")[[1]]), digits = 8)))
})

context("ds.mean::perf::split:0")
test_that("split - performance", {
    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.mean("D$LAB_TSC", type='split')

        .count <- .count + 1
        .current.time <- Sys.time()
    }
    expect_true(TRUE)

    print(paste("ds.mean::perf::split::0:", format(.count / (difftime(.current.time, .start.time, units = "secs")[[1]]), digits = 8)))
})

#
# Done
#

context("ds.mean::perf::shutdown")
disconnect.studies.dataset.cnsim()
context("ds.mean::perf::done")
