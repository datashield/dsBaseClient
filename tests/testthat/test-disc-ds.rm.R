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

context("ds.rm::disc")
test_that("justovertest", {
    res1 <- ds.rm(c("12345678910","12345678910"))

    expect_length(res1, 3)
    expect_length(res1$survival1, 1)
    expect_equal(res1$survival1$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res1$survival2, 1)
    expect_equal(res1$survival2$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res1$survival3, 1)
    expect_equal(res1$survival3$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    

    res2 <- ds.rm(c("12345678910","12345678"))
    expect_length(res2, 3)
    expect_length(res2$survival1, 1)
    expect_equal(res2$survival1$return.message[1], "Object to be deleted, i.e. <12345678910> , does not exist so does not need deleting")
    expect_equal(res2$survival1$return.message[2], "Object to be deleted, i.e. <12345678> , does not exist so does not need deleting")
    
    expect_length(res2$survival2, 1)
    expect_equal(res2$survival2$return.message[1], "Object to be deleted, i.e. <12345678910> , does not exist so does not need deleting")
    expect_equal(res2$survival2$return.message[2], "Object to be deleted, i.e. <12345678> , does not exist so does not need deleting")
    
    expect_length(res2$survival3, 1)
    expect_equal(res2$survival3$return.message[1], "Object to be deleted, i.e. <12345678910> , does not exist so does not need deleting")
    expect_equal(res2$survival3$return.message[2], "Object to be deleted, i.e. <12345678> , does not exist so does not need deleting")
    
})

test_that("simple test", {
    res1 <- ds.rm("thisisareallylongname_testing_datashield")
    
    expect_length(res1, 3)
    expect_length(res1$survival1, 1)
    expect_equal(res1$survival1$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res1$survival2, 1)
    expect_equal(res1$survival2$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res1$survival3, 1)
    expect_equal(res1$survival3$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    
    
    res2 <- ds.rm(c("erty33u88","erty33u88","erty33u88","erty33u88","erty33u88"))
    expect_length(res2, 3)
    expect_length(res2$survival1, 1)
    expect_equal(res2$survival1$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res2$survival2, 1)
    expect_equal(res2$survival2$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
    expect_length(res2$survival3, 1)
    expect_equal(res2$survival3$return.message, "Disclosure risk, number of characters in x.name must not exceed nfilter.stringShort which is currently set at: 20")
})

#
# Done
#

disconnect.studies.dataset.survival()
