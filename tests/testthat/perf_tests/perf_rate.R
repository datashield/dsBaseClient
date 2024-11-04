#-------------------------------------------------------------------------------
# Copyright (c) 2024 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

.perf.reference.filename <- 'perf_files/default_perf_profile.csv'

.perf.reference <- NULL

.load.pref <- function() {
    .perf.reference <<- read.csv(.perf.reference.filename, header = TRUE, sep = ",")
}

perf.reference.save <- function(perf.ref.name, rate, tolerance.lower, tolerance.upper) {
    if (is.null(.perf.reference))
        load.pref()

    .perf.reference[nrow(.perf.reference)+1,] <- c(perf.ref.name, rate, tolerance.lower, tolerance.upper)

    write.csv(.perf.reference, .perf.reference.filename, row.names = FALSE)

    .perf.reference <<- .perf.reference
}

perf.reference.rate <- function(perf.ref.name) {
    if (is.null(.perf.reference))
        .load.pref()

    return(as.numeric(.perf.reference[which(.perf.reference$refer_name == perf.ref.name),]$rate))
}

perf.reference.tolerance.lower <- function(perf.ref.name) {
    if (is.null(.perf.reference))
        .load.pref()

    return(as.numeric(.perf.reference[which(.perf.reference$refer_name == perf.ref.name),]$lower_tolerance))
}

perf.reference.tolerance.upper <- function(perf.ref.name) {
    if (is.null(.perf.reference))
        .load.pref()

    return(as.numeric(.perf.reference[which(.perf.reference$refer_name == perf.ref.name),]$upper_tolerance))
}

