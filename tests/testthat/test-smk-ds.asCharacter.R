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

context("ds.asCharacter::smk")

source("connection_to_datasets/init_all_datasets.R")
source("connection_to_datasets/init_smk_datasets.R")

connect.smk.dataset.sim(list("GENDER"))

#
# Tests
#

context("ds.asCharacter::smk::turn the factor variable 'GENDER' into a character vector")
ds.asCharacter(x='D$GENDER', newobj="gender_as_char")
res <- ds.exists('gender_as_char')
test_that("asCharacter_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("ds.asCharacter::smk::no table or newobj")
ds.asCharacter(x='D$GENDER')
res <- ds.exists('GENDER_char')
test_that("defualt_Character_exists", {
    expect_true(res$sim1)
    expect_true(res$sim2)
    expect_true(res$sim3)
})

context("ds.asCharacter::smk::no x")
test_that("asCharacter_no_x", {
    expect_error(ds.asCharacter(), "Please provide the name of the input vector!", fixed=TRUE)
})

#
# Tear down
#

