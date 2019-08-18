#-------------------------------------------------------------------------------
# Copyright (c) 2019 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.cnsim(list('LAB_TSC', 'LAB_TRIG','LAB_HDL', 'LAB_GLUC_ADJUSTED', 'PM_BMI_CONTINUOUS', 'DIS_CVA', 'MEDI_LPD', 'DIS_DIAB', 'DIS_AMI', 'GENDER', 'PM_BMI_CATEGORICAL'))

#
# Tests
#

context("ds.assign::smk")
test_that("test_assign", {
    res <- ds.assign('D$LAB_TSC', 'assigned_obj')

    expect_equal(res, NULL)
})

#
# Tear down
#

disconnect.studies.dataset.cnsim()
