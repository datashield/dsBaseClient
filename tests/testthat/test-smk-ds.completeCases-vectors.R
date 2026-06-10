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
# Set up
#

# context("ds.completeCases::smk::vector::setup")

connect.studies.dataset.survival(list('id', 'study.id', 'time.id', 'starttime', 'endtime', 'survtime', 'cens', 'age.60', 'female', 'noise.56', 'pm10.16', 'bmi.26'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.completeCases::smk::vector::numeric")
test_that("completeCases vector", {
    ds.c("D$survtime", newobj="vec_n")

    res.completeCases <- ds.completeCases("vec_n", "vec_n_new")

    expect_length(res.completeCases, 2)
    expect_equal(res.completeCases$is.object.created, "A data object <vec_n_new> has been created in all specified data sources")
    expect_equal(res.completeCases$validity.check, "<vec_n_new> appears valid in all sources")

    res.vec.class <- ds.class("vec_n")

    expect_length(res.vec.class, 3)
    expect_equal(res.vec.class$survival1, "numeric")
    expect_equal(res.vec.class$survival2, "numeric")
    expect_equal(res.vec.class$survival3, "numeric")

    res.vec_new.class <- ds.class("vec_n_new")

    expect_length(res.vec_new.class, 3)
    expect_equal(res.vec_new.class$survival1, "numeric")
    expect_equal(res.vec_new.class$survival2, "numeric")
    expect_equal(res.vec_new.class$survival3, "numeric")

    res.vec.length <- ds.length("vec_n")

    expect_length(res.vec.length, 4)
    expect_equal(res.vec.length$`length of vec_n in survival1`, 2060)
    expect_equal(res.vec.length$`length of vec_n in survival2`, 1640)
    expect_equal(res.vec.length$`length of vec_n in survival3`, 2688)
    expect_equal(res.vec.length$`total length of vec_n in all studies combined`, 6388)

    res.vec_new.length <- ds.length("vec_n_new")

    expect_length(res.vec_new.length, 4)
    expect_equal(res.vec_new.length$`length of vec_n_new in survival1`, 2038)
    expect_equal(res.vec_new.length$`length of vec_n_new in survival2`, 1637)
    expect_equal(res.vec_new.length$`length of vec_n_new in survival3`, 2662)
    expect_equal(res.vec_new.length$`total length of vec_n_new in all studies combined`, 6337)

    res.vec_numna <- ds.numNA("vec_n")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_numna$survival1, 22)
    expect_equal(res.vec_numna$survival2, 3)
    expect_equal(res.vec_numna$survival3, 26)

    res.vec_new_numna <- ds.numNA("vec_n_new")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_new_numna$survival1, 0)
    expect_equal(res.vec_new_numna$survival2, 0)
    expect_equal(res.vec_new_numna$survival3, 0)
})

# context("ds.completeCases::smk::vector::integer")
test_that("completeCases vector", {
    ds.asInteger("D$age.60", newobj="vec_i")

    res.completeCases <- ds.completeCases("vec_i", "vec_i_new")

    expect_length(res.completeCases, 2)
    expect_equal(res.completeCases$is.object.created, "A data object <vec_i_new> has been created in all specified data sources")
    expect_equal(res.completeCases$validity.check, "<vec_i_new> appears valid in all sources")

    res.vec.class <- ds.class("vec_i")

    expect_length(res.vec.class, 3)
    expect_equal(res.vec.class$survival1, "integer")
    expect_equal(res.vec.class$survival2, "integer")
    expect_equal(res.vec.class$survival3, "integer")

    res.vec_new.class <- ds.class("vec_i_new")

    expect_length(res.vec_new.class, 3)
    expect_equal(res.vec_new.class$survival1, "integer")
    expect_equal(res.vec_new.class$survival2, "integer")
    expect_equal(res.vec_new.class$survival3, "integer")

    res.vec.length <- ds.length("vec_i")

    expect_length(res.vec.length, 4)
    expect_equal(res.vec.length$`length of vec_i in survival1`, 2060)
    expect_equal(res.vec.length$`length of vec_i in survival2`, 1640)
    expect_equal(res.vec.length$`length of vec_i in survival3`, 2688)
    expect_equal(res.vec.length$`total length of vec_i in all studies combined`, 6388)

    res.vec_new.length <- ds.length("vec_i_new")

    expect_length(res.vec_new.length, 4)
    expect_equal(res.vec_new.length$`length of vec_i_new in survival1`, 2055)
    expect_equal(res.vec_new.length$`length of vec_i_new in survival2`, 1621)
    expect_equal(res.vec_new.length$`length of vec_i_new in survival3`, 2667)
    expect_equal(res.vec_new.length$`total length of vec_i_new in all studies combined`, 6343)

    res.vec_numna <- ds.numNA("vec_i")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_numna$survival1, 5)
    expect_equal(res.vec_numna$survival2, 19)
    expect_equal(res.vec_numna$survival3, 21)

    res.vec_new_numna <- ds.numNA("vec_i_new")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_new_numna$survival1, 0)
    expect_equal(res.vec_new_numna$survival2, 0)
    expect_equal(res.vec_new_numna$survival3, 0)
})

# context("ds.completeCases::smk::vector::character")
test_that("completeCases vector", {
    ds.asCharacter("D$age.60", newobj="vec_c")

    res.completeCases <- ds.completeCases("vec_c", "vec_c_new")

    expect_length(res.completeCases, 2)
    expect_equal(res.completeCases$is.object.created, "A data object <vec_c_new> has been created in all specified data sources")
    expect_equal(res.completeCases$validity.check, "<vec_c_new> appears valid in all sources")

    res.vec.class <- ds.class("vec_c")

    expect_length(res.vec.class, 3)
    expect_equal(res.vec.class$survival1, "character")
    expect_equal(res.vec.class$survival2, "character")
    expect_equal(res.vec.class$survival3, "character")

    res.vec_new.class <- ds.class("vec_c_new")

    expect_length(res.vec_new.class, 3)
    expect_equal(res.vec_new.class$survival1, "character")
    expect_equal(res.vec_new.class$survival2, "character")
    expect_equal(res.vec_new.class$survival3, "character")

    res.vec.length <- ds.length("vec_c")

    expect_length(res.vec.length, 4)
    expect_equal(res.vec.length$`length of vec_c in survival1`, 2060)
    expect_equal(res.vec.length$`length of vec_c in survival2`, 1640)
    expect_equal(res.vec.length$`length of vec_c in survival3`, 2688)
    expect_equal(res.vec.length$`total length of vec_c in all studies combined`, 6388)

    res.vec_new.length <- ds.length("vec_c_new")

    expect_length(res.vec_new.length, 4)
    expect_equal(res.vec_new.length$`length of vec_c_new in survival1`, 2055)
    expect_equal(res.vec_new.length$`length of vec_c_new in survival2`, 1621)
    expect_equal(res.vec_new.length$`length of vec_c_new in survival3`, 2667)
    expect_equal(res.vec_new.length$`total length of vec_c_new in all studies combined`, 6343)

    res.vec_numna <- ds.numNA("vec_c")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_numna$survival1, 5)
    expect_equal(res.vec_numna$survival2, 19)
    expect_equal(res.vec_numna$survival3, 21)

    res.vec_new_numna <- ds.numNA("vec_c_new")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_new_numna$survival1, 0)
    expect_equal(res.vec_new_numna$survival2, 0)
    expect_equal(res.vec_new_numna$survival3, 0)
})

# context("ds.completeCases::smk::vector::logical")
test_that("completeCases vector", {
    ds.asLogical("D$age.60", newobj="vec_l")

    res.completeCases <- ds.completeCases("vec_l", "vec_l_new")

    expect_length(res.completeCases, 2)
    expect_equal(res.completeCases$is.object.created, "A data object <vec_l_new> has been created in all specified data sources")
    expect_equal(res.completeCases$validity.check, "<vec_l_new> appears valid in all sources")

    res.vec.class <- ds.class("vec_l")

    expect_length(res.vec.class, 3)
    expect_equal(res.vec.class$survival1, "logical")
    expect_equal(res.vec.class$survival2, "logical")
    expect_equal(res.vec.class$survival3, "logical")

    res.vec_new.class <- ds.class("vec_l_new")

    expect_length(res.vec_new.class, 3)
    expect_equal(res.vec_new.class$survival1, "logical")
    expect_equal(res.vec_new.class$survival2, "logical")
    expect_equal(res.vec_new.class$survival3, "logical")

    res.vec.length <- ds.length("vec_l")

    expect_length(res.vec.length, 4)
    expect_equal(res.vec.length$`length of vec_l in survival1`, 2060)
    expect_equal(res.vec.length$`length of vec_l in survival2`, 1640)
    expect_equal(res.vec.length$`length of vec_l in survival3`, 2688)
    expect_equal(res.vec.length$`total length of vec_l in all studies combined`, 6388)

    res.vec_new.length <- ds.length("vec_l_new")

    expect_length(res.vec_new.length, 4)
    expect_equal(res.vec_new.length$`length of vec_l_new in survival1`, 2055)
    expect_equal(res.vec_new.length$`length of vec_l_new in survival2`, 1621)
    expect_equal(res.vec_new.length$`length of vec_l_new in survival3`, 2667)
    expect_equal(res.vec_new.length$`total length of vec_l_new in all studies combined`, 6343)

    res.vec_numna <- ds.numNA("vec_l")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_numna$survival1, 5)
    expect_equal(res.vec_numna$survival2, 19)
    expect_equal(res.vec_numna$survival3, 21)

    res.vec_new_numna <- ds.numNA("vec_l_new")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_new_numna$survival1, 0)
    expect_equal(res.vec_new_numna$survival2, 0)
    expect_equal(res.vec_new_numna$survival3, 0)
})

# context("ds.completeCases::smk::vector::factor")
test_that("completeCases vector", {
    ds.c("D$female", newobj="vec_f")

    res.completeCases <- ds.completeCases("vec_f", "vec_f_new")

    expect_length(res.completeCases, 2)
    expect_equal(res.completeCases$is.object.created, "A data object <vec_f_new> has been created in all specified data sources")
    expect_equal(res.completeCases$validity.check, "<vec_f_new> appears valid in all sources")

    res.vec.class <- ds.class("vec_f")

    expect_length(res.vec.class, 3)
    expect_equal(res.vec.class$survival1, "factor")
    expect_equal(res.vec.class$survival2, "factor")
    expect_equal(res.vec.class$survival3, "factor")

    res.vec_new.class <- ds.class("vec_f_new")

    expect_length(res.vec_new.class, 3)
    expect_equal(res.vec_new.class$survival1, "factor")
    expect_equal(res.vec_new.class$survival2, "factor")
    expect_equal(res.vec_new.class$survival3, "factor")

    res.vec.length <- ds.length("vec_f")

    expect_length(res.vec.length, 4)
    expect_equal(res.vec.length$`length of vec_f in survival1`, 2060)
    expect_equal(res.vec.length$`length of vec_f in survival2`, 1640)
    expect_equal(res.vec.length$`length of vec_f in survival3`, 2688)
    expect_equal(res.vec.length$`total length of vec_f in all studies combined`, 6388)

    res.vec_new.length <- ds.length("vec_f_new")

    expect_length(res.vec_new.length, 4)
    expect_equal(res.vec_new.length$`length of vec_f_new in survival1`, 2044)
    expect_equal(res.vec_new.length$`length of vec_f_new in survival2`, 1635)
    expect_equal(res.vec_new.length$`length of vec_f_new in survival3`, 2671)
    expect_equal(res.vec_new.length$`total length of vec_f_new in all studies combined`, 6350)

    res.vec_numna <- ds.numNA("vec_f")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_numna$survival1, 16)
    expect_equal(res.vec_numna$survival2, 5)
    expect_equal(res.vec_numna$survival3, 17)

    res.vec_new_numna <- ds.numNA("vec_f_new")
    
    expect_length(res.vec_numna, 3)
    expect_equal(res.vec_new_numna$survival1, 0)
    expect_equal(res.vec_new_numna$survival2, 0)
    expect_equal(res.vec_new_numna$survival3, 0)
})

#
# Done
#

# context("ds.completeCases::smk::vector::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "vec_n", "vec_n_new", "vec_i", "vec_i_new", "vec_c", "vec_c_new", "vec_l", "vec_l_new", "vec_f", "vec_f_new"))
})

disconnect.studies.dataset.survival()

# context("ds.completeCases::smk::done")
