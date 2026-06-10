#-------------------------------------------------------------------------------
# Copyright (c) 2018-2022 University of Newcastle upon Tyne. All rights reserved.
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

# context("ds.metadata::smk::setup")

connect.studies.dataset.cnsim(list('LAB_TSC', 'LAB_TRIG', 'LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.metadata::smk")
test_that("data.frame metadata", {
    res <- ds.metadata(x='D')

    if (ds.test_env$driver == "ArmadilloDriver") {
        expect_true(all(class(res) %in% c('list')))
        expect_length(res, 3)
        expect_length(res$sim1, 2)
        expect_true(all(class(res$sim1) %in% c('list')))
        expect_true(all(names(res$sim1) %in% c('names', 'spec', 'class')))
        expect_length(res$sim1$names, 11)
        expect_length(res$sim1$class, 1)
        expect_length(res$sim2, 2)
        expect_true(all(class(res$sim2) %in% c('list')))
        expect_true(all(names(res$sim2) %in% c('names', 'spec', 'class')))
        expect_length(res$sim2$names, 11)
        expect_length(res$sim2$class, 1)
        expect_length(res$sim3, 2)
        expect_true(all(class(res$sim3) %in% c('list')))
        expect_true(all(names(res$sim3) %in% c('names', 'spec', 'class')))
        expect_length(res$sim3$names, 11)
        expect_length(res$sim3$class, 1)
    } else if (ds.test_env$driver == "OpalDriver") {
        expect_true(all(class(res) %in% c('list')))
        expect_length(res, 3)
        expect_length(res$sim1, 2)
        expect_true(all(class(res$sim1) %in% c('list')))
        expect_true(all(names(res$sim1) %in% c('names', 'spec', 'class')))
        expect_length(res$sim1$names, 11)
        expect_length(res$sim1$class, 1)
        expect_length(res$sim2, 2)
        expect_true(all(class(res$sim2) %in% c('list')))
        expect_true(all(names(res$sim2) %in% c('names', 'spec', 'class')))
        expect_length(res$sim2$names, 11)
        expect_length(res$sim2$class, 1)
        expect_length(res$sim3, 2)
        expect_true(all(class(res$sim3) %in% c('list')))
        expect_true(all(names(res$sim3) %in% c('names', 'spec', 'class')))
        expect_length(res$sim3$names, 11)
        expect_length(res$sim3$class, 1)
    }
})

test_that("column metadata", {
    res <- ds.metadata(x='D$LAB_TSC')

    if (ds.test_env$driver == "ArmadilloDriver") {
        expect_true(all(class(res) %in% c('list')))
        expect_length(res, 3)
        expect_length(res$sim1, 7)
        expect_length(res$sim2, 7)
        expect_length(res$sim3, 7)
    } else if (ds.test_env$driver == "OpalDriver") {
        expect_length(res$sim1, 6)
        expect_true(all(class(res$sim1) %in% c('list')))
        expect_true(all(names(res$sim1) %in% c('label', 'opal.value_type', 'opal.entity_type', 'opal.repeatable', 'opal.index', 'opal.nature')))
        expect_length(res$sim1$label, 1)
        expect_length(res$sim1$opal.value_type, 1)
        expect_length(res$sim1$opal.entity_type, 1)
        expect_length(res$sim1$opal.repeatable, 1)
        expect_length(res$sim1$opal.index, 1)
        expect_length(res$sim1$opal.nature, 1)
        expect_length(res$sim2, 6)
        expect_true(all(class(res$sim2) %in% c('list')))
        expect_true(all(names(res$sim2) %in% c('label', 'opal.value_type', 'opal.entity_type', 'opal.repeatable', 'opal.index', 'opal.nature')))
        expect_length(res$sim2$label, 1)
        expect_length(res$sim2$opal.value_type, 1)
        expect_length(res$sim2$opal.entity_type, 1)
        expect_length(res$sim2$opal.repeatable, 1)
        expect_length(res$sim2$opal.index, 1)
        expect_length(res$sim2$opal.nature, 1)
        expect_length(res$sim3, 6)
        expect_true(all(class(res$sim3) %in% c('list')))
        expect_true(all(names(res$sim3) %in% c('label', 'opal.value_type', 'opal.entity_type', 'opal.repeatable', 'opal.index', 'opal.nature')))
        expect_length(res$sim3$label, 1)
        expect_length(res$sim3$opal.value_type, 1)
        expect_length(res$sim3$opal.entity_type, 1)
        expect_length(res$sim3$opal.repeatable, 1)
        expect_length(res$sim3$opal.index, 1)
        expect_length(res$sim3$opal.nature, 1)
    } else {
        fail(message = "Unknown driver type", info = ds.test_env$driver)
    }
})

#
# Tear down
#

# context("ds.metadata::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cnsim()

# context("ds.metadata::smk::done")
