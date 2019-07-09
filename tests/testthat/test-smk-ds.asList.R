#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
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

context("ds.asList::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("LAB_TSC", "GENDER"))

#
# Tests
#

context("ds.asList::turn the data frame D into a list")
ds.asList(x='D')
type <- ds.summary(x='D_list')$sim3$class
test_that("Is List", {
    expect_equal(type, "list")
})

context("ds.asList::smk::test errors")
test_that("asList_erros", {
    expect_error(ds.asList(), "Please provide the name of the input object!", fixed=TRUE)
    expect_error(ds.asList(x='D$LAB_TSC'), "Only objects of type 'data.frame' or 'matrix' are allowed. Please see documentation.", fixed=TRUE)
})
#
# Tear down
#

