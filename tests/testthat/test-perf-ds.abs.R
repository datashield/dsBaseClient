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

context("ds.abs::perf::setup")
connect.studies.dataset.cnsim(list("LAB_TSC", "LAB_TRIG"))

#
# Tests
#

context("ds.abs::perf:0")
test_that("combine - performance", {
    .durationSec  <- 30 # seconds
    .count        <- 0
    .start.time   <- Sys.time()
    .current.time <- .start.time

    while (difftime(.current.time, .start.time, units = "secs")[[1]] < .durationSec) {
        ds.abs("D$LAB_TSC", newobj = "abs.newobj")

        .count <- .count + 1
        .current.time <- Sys.time()
    }
    expect_true(TRUE)

    print(paste("ds.abs::perf::0:", format(.count / (difftime(.current.time, .start.time, units = "secs")[[1]]), digits = 8)))
})

#
# Done
#

context("ds.abs::perf::shutdown")
disconnect.studies.dataset.cnsim()
context("ds.abs::perf::done")
