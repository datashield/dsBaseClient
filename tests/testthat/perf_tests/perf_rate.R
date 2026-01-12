#-------------------------------------------------------------------------------
# Copyright (c) 2024-2026 Arjuna Technologies, Newcastle upon Tyne. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

.perf.reference.filename.base.prefix  <- 'perf_files/'
.perf.reference.filename.base.postfix <- '_perf-profile.csv'
.perf.reference.save.filename         <- NULL

.perf.reference <- NULL

.load.pref <- function() {
    if (ds.test_env$driver == "OpalDriver")
        perf.reference.filename.driver.infix <- "opal"
    else if (ds.test_env$driver == "ArmadilloDriver")
        perf.reference.filename.driver.infix <- "armadillo"
    else if (ds.test_env$driver == "DSLiteDriver")
        perf.reference.filename.driver.infix <- "dslite"
    else
    {
        perf.reference.filename.infix <- "unknown"
        warning("Unknown performance profile driver, using 'unknown'")
    }

    perf.profile <- base::Sys.getenv("PERF_PROFILE")
    if (nchar(perf.profile) > 0)
        perf.reference.filename.platform.infix <- base::tolower(perf.profile)
    else
    {
        perf.reference.filename.platform.infix <- "default"
        warning("Unknown performance profile platform, using 'default'")
    }

    perf.reference.filename <-  paste(.perf.reference.filename.base.prefix, perf.reference.filename.driver.infix, '_', perf.reference.filename.platform.infix, .perf.reference.filename.base.postfix, sep = "")
    .perf.reference         <<- read.csv(perf.reference.filename, header = TRUE, sep = ",")
}

perf.reference.save <- function(perf.ref.name, rate, tolerance.lower, tolerance.upper) {
    if (is.null(.perf.reference))
        load.pref()

    .perf.reference[nrow(.perf.reference)+1,] <- c(perf.ref.name, rate, tolerance.lower, tolerance.upper)

    if (is.null(.perf.reference.save.filename))
    {
        .perf.reference.save.filename <<- base::tempfile(pattern = "perf_file_", fileext = ".csv")
        message(paste0("Additional perf record added to '", .perf.reference.save.filename, "'"))
    }

    write.csv(.perf.reference, .perf.reference.save.filename, row.names = FALSE)

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
