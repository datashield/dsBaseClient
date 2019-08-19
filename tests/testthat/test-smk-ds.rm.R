#-------------------------------------------------------------------------------
# Copyright (c) 2018 University of Newcastle upon Tyne. All rights reserved.
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

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

#
# Tests
#

context("ds.rm::smk")
test_that("simple test", {
    res1 <- ds.rm("nonexistant_object")

    expect_length(res1, 3)
    expect_length(res1$survival1, 1)
    expect_equal(res1$survival1$return.message, "Object to be deleted, i.e. <nonexistant_object> , does not exist so does not need deleting")
    expect_length(res1$survival2, 1)
    expect_equal(res1$survival2$return.message, "Object to be deleted, i.e. <nonexistant_object> , does not exist so does not need deleting")
    expect_length(res1$survival3, 1)
    expect_equal(res1$survival3$return.message, "Object to be deleted, i.e. <nonexistant_object> , does not exist so does not need deleting")
    
    ds.rbind(c("D$survtime", "D$time.id", "D$female"), newobj="existing_object")

    res2 <- ds.rm("existing_object")

    expect_length(res2, 3)
    expect_length(res2$survival1, 1)
    expect_equal(res2$survival1$return.message, "Object <existing_object> successfully deleted")
    expect_length(res2$survival2, 1)
    expect_equal(res2$survival2$return.message, "Object <existing_object> successfully deleted")
    expect_length(res2$survival3, 1)
    expect_equal(res2$survival3$return.message, "Object <existing_object> successfully deleted")
})

#
# Done
#

disconnect.studies.dataset.survival()
