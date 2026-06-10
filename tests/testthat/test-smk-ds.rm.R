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

# context("ds.rm::smk::setup")

connect.studies.dataset.survival(list("survtime", "time.id", "female"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.rm::smk")
test_that("simple test", {
    res1 <- ds.rm("nonexistant_object")

    expect_length(res1, 3)
    expect_length(res1$survival1, 4)
    expect_equal(res1$survival1$return.message, "Object(s) 'nonexistant_object' which are missing.")
    expect_equal(res1$survival1$deleted.objects, "")
    expect_equal(res1$survival1$missing.objects, "nonexistant_object")
    expect_equal(res1$survival1$problem.objects, "")
    expect_length(res1$survival2, 4)
    expect_equal(res1$survival2$return.message, "Object(s) 'nonexistant_object' which are missing.")
    expect_equal(res1$survival2$deleted.objects, "")
    expect_equal(res1$survival2$missing.objects, "nonexistant_object")
    expect_equal(res1$survival2$problem.objects, "")
    expect_length(res1$survival3, 4)
    expect_equal(res1$survival3$return.message, "Object(s) 'nonexistant_object' which are missing.")
    expect_equal(res1$survival3$deleted.objects, "")
    expect_equal(res1$survival3$missing.objects, "nonexistant_object")
    expect_equal(res1$survival3$problem.objects, "")
    
    ds.rbind(c("D$survtime", "D$time.id", "D$female"), newobj="existing_object")

    res2 <- ds.rm("existing_object")

    expect_length(res2, 3)
    expect_length(res2$survival1, 4)
    expect_equal(res2$survival1$return.message, "Object(s) 'existing_object' was deleted.")
    expect_equal(res2$survival1$deleted.objects, "existing_object")
    expect_equal(res2$survival1$missing.objects, "")
    expect_equal(res2$survival1$problem.objects, "")
    expect_length(res2$survival2, 4)
    expect_equal(res2$survival2$return.message, "Object(s) 'existing_object' was deleted.")
    expect_equal(res2$survival1$deleted.objects, "existing_object")
    expect_equal(res2$survival1$missing.objects, "")
    expect_equal(res2$survival1$problem.objects, "")
    expect_length(res2$survival3, 4)
    expect_equal(res2$survival3$return.message, "Object(s) 'existing_object' was deleted.")
    expect_equal(res2$survival1$deleted.objects, "existing_object")
    expect_equal(res2$survival1$missing.objects, "")
    expect_equal(res2$survival1$problem.objects, "")
})

# context("ds.rm::smk")
test_that("multiple test", {
    res1 <- ds.rm("nonexistant_object1,nonexistant_object2")

    expect_length(res1, 3)
    expect_length(res1$survival1, 4)
    expect_equal(res1$survival1$return.message, "Object(s) 'nonexistant_object1,nonexistant_object2' which are missing.")
    expect_equal(res1$survival1$deleted.objects, "")
    expect_equal(res1$survival1$missing.objects, "nonexistant_object1,nonexistant_object2")
    expect_equal(res1$survival1$problem.objects, "")
    expect_length(res1$survival2, 4)
    expect_equal(res1$survival2$return.message, "Object(s) 'nonexistant_object1,nonexistant_object2' which are missing.")
    expect_equal(res1$survival2$deleted.objects, "")
    expect_equal(res1$survival2$missing.objects, "nonexistant_object1,nonexistant_object2")
    expect_equal(res1$survival2$problem.objects, "")
    expect_length(res1$survival3, 4)
    expect_equal(res1$survival3$return.message, "Object(s) 'nonexistant_object1,nonexistant_object2' which are missing.")
    expect_equal(res1$survival3$deleted.objects, "")
    expect_equal(res1$survival3$missing.objects, "nonexistant_object1,nonexistant_object2")
    expect_equal(res1$survival3$problem.objects, "")
    
    ds.rbind(c("D$survtime", "D$time.id", "D$female"), newobj="existing_object1")
    ds.rbind(c("D$survtime", "D$time.id", "D$female"), newobj="existing_object2")

    res2 <- ds.rm("existing_object1,existing_object2")

    expect_length(res2, 3)
    expect_length(res2$survival1, 4)
    expect_equal(res2$survival1$return.message, "Object(s) 'existing_object1,existing_object2' was deleted.")
    expect_equal(res2$survival1$deleted.objects, "existing_object1,existing_object2")
    expect_equal(res2$survival1$missing.objects, "")
    expect_equal(res2$survival1$problem.objects, "")
    expect_length(res2$survival2, 4)
    expect_equal(res2$survival2$return.message, "Object(s) 'existing_object1,existing_object2' was deleted.")
    expect_equal(res2$survival1$deleted.objects, "existing_object1,existing_object2")
    expect_equal(res2$survival1$missing.objects, "")
    expect_equal(res2$survival1$problem.objects, "")
    expect_length(res2$survival3, 4)
    expect_equal(res2$survival3$return.message, "Object(s) 'existing_object1,existing_object2' was deleted.")
    expect_equal(res2$survival1$deleted.objects, "existing_object1,existing_object2")
    expect_equal(res2$survival1$missing.objects, "")
    expect_equal(res2$survival1$problem.objects, "")

    ds.rbind(c("D$survtime", "D$time.id", "D$female"), newobj="existing_object3")

    res3 <- ds.rm("existing_object3,nonexisting_object3")

    expect_length(res3, 3)
    expect_length(res3$survival1, 4)
    expect_equal(res3$survival1$return.message, "Object(s) 'existing_object3' was deleted. 'nonexisting_object3' which are missing.")
    expect_equal(res3$survival1$deleted.objects, "existing_object3")
    expect_equal(res3$survival1$missing.objects, "nonexisting_object3")
    expect_equal(res3$survival1$problem.objects, "")
    expect_length(res3$survival2, 4)
    expect_equal(res3$survival2$return.message, "Object(s) 'existing_object3' was deleted. 'nonexisting_object3' which are missing.")
    expect_equal(res3$survival1$deleted.objects, "existing_object3")
    expect_equal(res3$survival1$missing.objects, "nonexisting_object3")
    expect_equal(res3$survival1$problem.objects, "")
    expect_length(res3$survival3, 4)
    expect_equal(res3$survival3$return.message, "Object(s) 'existing_object3' was deleted. 'nonexisting_object3' which are missing.")
    expect_equal(res3$survival1$deleted.objects, "existing_object3")
    expect_equal(res3$survival1$missing.objects, "nonexisting_object3")
    expect_equal(res3$survival1$problem.objects, "")
})

#
# Done
#

# context("ds.rm::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.survival()

# context("ds.rm::smk::done")
