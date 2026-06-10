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
# Datashield test suite set up
#

# context("setup - start")

# Convert 'warnings' to 'errors'
# options(warn = 2)

library(lme4)
library(e1071)
library(DescTools)

library(DSOpal)
library(DSMolgenisArmadillo)
library(DSLite)

source("dstest_functions/ds_expect_variables.R")
source("perf_tests/perf_rate.R")
source("connection_to_datasets/login_details.R")
source("connection_to_datasets/init_testing_datasets.R")
source("connection_to_datasets/init_studies_datasets.R")
source("connection_to_datasets/init_discordant_datasets.R")
source("connection_to_datasets/init_mediation_datasets.R")

# context("setup - done")
