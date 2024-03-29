#-------------------------------------------------------------------------------
# Copyright (c) 2019-2022 University of Newcastle upon Tyne. All rights reserved.
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

context("CLUSTER_SLO::datachk::setup")

connect.studies.dataset.cluster.slo(list("idSurgery", "trtGrp", "intSurgery", "nDoctors", "idDoctor", "intDoctor", "sloDoctor", "nPatients", "Male", "age", "BMI", "private", "diabetes", "incid_rate"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("CLUSTER_SLO::datachk")
test_that("Check CLUSTER SLO dataset", {
    res.class <- ds.class(x='D')
    expect_length(res.class, 3)
    expect_gte(length(res.class$cluster.slo1), 1)
    expect_true("data.frame" %in% res.class$cluster.slo1)
    expect_gte(length(res.class$cluster.slo2), 1)
    expect_true("data.frame" %in% res.class$cluster.slo2)
    expect_gte(length(res.class$cluster.slo3), 1)
    expect_true("data.frame" %in% res.class$cluster.slo3)

    res.length <- ds.length(x='D')
    expect_length(res.length, 4)
    expect_length(res.length$`length of D in cluster.slo1`, 1)
    expect_equal(res.length$`length of D in cluster.slo1`, 14)
    expect_length(res.length$`length of D in cluster.slo2`, 1)
    expect_equal(res.length$`length of D in cluster.slo2`, 14)
    expect_length(res.length$`length of D in cluster.slo3`, 1)
    expect_equal(res.length$`length of D in cluster.slo3`, 14)
    expect_equal(res.length$`total length of D in all studies combined`, 42)

    res.colnames <- ds.colnames(x='D')
    expect_length(res.colnames, 3)
    expect_length(res.colnames$cluster.slo1, 14)
    expect_equal(res.colnames$cluster.slo1, c("idSurgery", "trtGrp", "intSurgery", "nDoctors", "idDoctor", "intDoctor", "sloDoctor", "nPatients", "Male", "age", "BMI", "private", "diabetes", "incid_rate"))
    expect_length(res.colnames$cluster.slo2, 14)
    expect_equal(res.colnames$cluster.slo2, c("idSurgery", "trtGrp", "intSurgery", "nDoctors", "idDoctor", "intDoctor", "sloDoctor", "nPatients", "Male", "age", "BMI", "private", "diabetes", "incid_rate"))
    expect_length(res.colnames$cluster.slo3, 14)
    expect_equal(res.colnames$cluster.slo3, c("idSurgery", "trtGrp", "intSurgery", "nDoctors", "idDoctor", "intDoctor", "sloDoctor", "nPatients", "Male", "age", "BMI", "private", "diabetes", "incid_rate"))

    res.class.idsurgery <- ds.class(x='D$idSurgery')
    expect_length(res.class.idsurgery, 3)
    expect_length(res.class.idsurgery$cluster.slo1, 1)
    expect_equal(res.class.idsurgery$cluster.slo1, "factor")
    expect_length(res.class.idsurgery$cluster.slo2, 1)
    expect_equal(res.class.idsurgery$cluster.slo2, "factor")
    expect_length(res.class.idsurgery$cluster.slo3, 1)
    expect_equal(res.class.idsurgery$cluster.slo3, "factor")

    res.length.idsurgery <- ds.length(x='D$idSurgery')
    expect_length(res.length.idsurgery, 4)
    expect_length(res.length.idsurgery$`length of D$idSurgery in cluster.slo1`, 1)
    expect_equal(res.length.idsurgery$`length of D$idSurgery in cluster.slo1`, 5637)
    expect_length(res.length.idsurgery$`length of D$idSurgery in cluster.slo2`, 1)
    expect_equal(res.length.idsurgery$`length of D$idSurgery in cluster.slo2`, 5563)
    expect_length(res.length.idsurgery$`length of D$idSurgery in cluster.slo3`, 1)
    expect_equal(res.length.idsurgery$`length of D$idSurgery in cluster.slo3`, 5837)
    expect_length(res.length.idsurgery$`total length of D$idSurgery in all studies combined`, 1)
    expect_equal(res.length.idsurgery$`total length of D$idSurgery in all studies combined`, 17037)

    res.class.trtgrp <- ds.class(x='D$trtGrp')
    expect_length(res.class.trtgrp, 3)
    expect_length(res.class.trtgrp$cluster.slo1, 1)
    expect_equal(res.class.trtgrp$cluster.slo1, "factor")
    expect_length(res.class.trtgrp$cluster.slo2, 1)
    expect_equal(res.class.trtgrp$cluster.slo2, "factor")
    expect_length(res.class.trtgrp$cluster.slo3, 1)
    expect_equal(res.class.trtgrp$cluster.slo3, "factor")

    res.length.trtgrp <- ds.length(x='D$trtGrp')
    expect_length(res.length.trtgrp, 4)
    expect_length(res.length.trtgrp$`length of D$trtGrp in cluster.slo1`, 1)
    expect_equal(res.length.trtgrp$`length of D$trtGrp in cluster.slo1`, 5637)
    expect_length(res.length.trtgrp$`length of D$trtGrp in cluster.slo2`, 1)
    expect_equal(res.length.trtgrp$`length of D$trtGrp in cluster.slo2`, 5563)
    expect_length(res.length.trtgrp$`length of D$trtGrp in cluster.slo3`, 1)
    expect_equal(res.length.trtgrp$`length of D$trtGrp in cluster.slo3`, 5837)
    expect_length(res.length.trtgrp$`total length of D$trtGrp in all studies combined`, 1)
    expect_equal(res.length.trtgrp$`total length of D$trtGrp in all studies combined`, 17037)

    res.class.intsurgery <- ds.class(x='D$intSurgery')
    expect_length(res.class.intsurgery, 3)
    expect_length(res.class.intsurgery$cluster.slo1, 1)
    expect_equal(res.class.intsurgery$cluster.slo1, "numeric")
    expect_length(res.class.intsurgery$cluster.slo2, 1)
    expect_equal(res.class.intsurgery$cluster.slo2, "numeric")
    expect_length(res.class.intsurgery$cluster.slo3, 1)
    expect_equal(res.class.intsurgery$cluster.slo3, "numeric")

    res.length.intsurgery <- ds.length(x='D$intSurgery')
    expect_length(res.length.intsurgery, 4)
    expect_length(res.length.intsurgery$`length of D$intSurgery in cluster.slo1`, 1)
    expect_equal(res.length.intsurgery$`length of D$intSurgery in cluster.slo1`, 5637)
    expect_length(res.length.intsurgery$`length of D$intSurgery in cluster.slo2`, 1)
    expect_equal(res.length.intsurgery$`length of D$intSurgery in cluster.slo2`, 5563)
    expect_length(res.length.intsurgery$`length of D$intSurgery in cluster.slo3`, 1)
    expect_equal(res.length.intsurgery$`length of D$intSurgery in cluster.slo3`, 5837)
    expect_length(res.length.intsurgery$`total length of D$intSurgery in all studies combined`, 1)
    expect_equal(res.length.intsurgery$`total length of D$intSurgery in all studies combined`, 17037)

    res.class.ndoctors <- ds.class(x='D$nDoctors')
    expect_length(res.class.ndoctors, 3)
    expect_length(res.class.ndoctors$cluster.slo1, 1)
    expect_equal(res.class.ndoctors$cluster.slo1, "integer")
    expect_length(res.class.ndoctors$cluster.slo2, 1)
    expect_equal(res.class.ndoctors$cluster.slo2, "integer")
    expect_length(res.class.ndoctors$cluster.slo3, 1)
    expect_equal(res.class.ndoctors$cluster.slo3, "integer")

    res.length.ndoctors <- ds.length(x='D$nDoctors')
    expect_length(res.length.ndoctors, 4)
    expect_length(res.length.ndoctors$`length of D$nDoctors in cluster.slo1`, 1)
    expect_equal(res.length.ndoctors$`length of D$nDoctors in cluster.slo1`, 5637)
    expect_length(res.length.ndoctors$`length of D$nDoctors in cluster.slo2`, 1)
    expect_equal(res.length.ndoctors$`length of D$nDoctors in cluster.slo2`, 5563)
    expect_length(res.length.ndoctors$`length of D$nDoctors in cluster.slo3`, 1)
    expect_equal(res.length.ndoctors$`length of D$nDoctors in cluster.slo3`, 5837)
    expect_length(res.length.ndoctors$`total length of D$nDoctors in all studies combined`, 1)
    expect_equal(res.length.ndoctors$`total length of D$nDoctors in all studies combined`, 17037)

    res.class.iddoctor <- ds.class(x='D$idDoctor')
    expect_length(res.class.iddoctor, 3)
    expect_length(res.class.iddoctor$cluster.slo1, 1)
    expect_equal(res.class.iddoctor$cluster.slo1, "factor")
    expect_length(res.class.iddoctor$cluster.slo2, 1)
    expect_equal(res.class.iddoctor$cluster.slo2, "factor")
    expect_length(res.class.iddoctor$cluster.slo3, 1)
    expect_equal(res.class.iddoctor$cluster.slo3, "factor")

    res.length.iddoctor <- ds.length(x='D$idDoctor')
    expect_length(res.length.iddoctor, 4)
    expect_length(res.length.iddoctor$`length of D$idDoctor in cluster.slo1`, 1)
    expect_equal(res.length.iddoctor$`length of D$idDoctor in cluster.slo1`, 5637)
    expect_length(res.length.iddoctor$`length of D$idDoctor in cluster.slo2`, 1)
    expect_equal(res.length.iddoctor$`length of D$idDoctor in cluster.slo2`, 5563)
    expect_length(res.length.iddoctor$`length of D$idDoctor in cluster.slo3`, 1)
    expect_equal(res.length.iddoctor$`length of D$idDoctor in cluster.slo3`, 5837)
    expect_length(res.length.iddoctor$`total length of D$idDoctor in all studies combined`, 1)
    expect_equal(res.length.iddoctor$`total length of D$idDoctor in all studies combined`, 17037)

    res.class.intdoctor <- ds.class(x='D$intDoctor')
    expect_length(res.class.intdoctor, 3)
    expect_length(res.class.intdoctor$cluster.slo1, 1)
    expect_equal(res.class.intdoctor$cluster.slo1, "numeric")
    expect_length(res.class.intdoctor$cluster.slo2, 1)
    expect_equal(res.class.intdoctor$cluster.slo2, "numeric")
    expect_length(res.class.intdoctor$cluster.slo3, 1)
    expect_equal(res.class.intdoctor$cluster.slo3, "numeric")

    res.length.intdoctor <- ds.length(x='D$intDoctor')
    expect_length(res.length.intdoctor, 4)
    expect_length(res.length.intdoctor$`length of D$intDoctor in cluster.slo1`, 1)
    expect_equal(res.length.intdoctor$`length of D$intDoctor in cluster.slo1`, 5637)
    expect_length(res.length.intdoctor$`length of D$intDoctor in cluster.slo2`, 1)
    expect_equal(res.length.intdoctor$`length of D$intDoctor in cluster.slo2`, 5563)
    expect_length(res.length.intdoctor$`length of D$intDoctor in cluster.slo3`, 1)
    expect_equal(res.length.intdoctor$`length of D$intDoctor in cluster.slo3`, 5837)
    expect_length(res.length.intdoctor$`total length of D$intDoctor in all studies combined`, 1)
    expect_equal(res.length.intdoctor$`total length of D$intDoctor in all studies combined`, 17037)

    res.class.npatients <- ds.class(x='D$nPatients')
    expect_length(res.class.npatients, 3)
    expect_length(res.class.npatients$cluster.slo1, 1)
    expect_equal(res.class.npatients$cluster.slo1, "integer")
    expect_length(res.class.npatients$cluster.slo2, 1)
    expect_equal(res.class.npatients$cluster.slo2, "integer")
    expect_length(res.class.npatients$cluster.slo3, 1)
    expect_equal(res.class.npatients$cluster.slo3, "integer")

    res.length.npatients <- ds.length(x='D$nPatients')
    expect_length(res.length.npatients, 4)
    expect_length(res.length.npatients$`length of D$nPatients in cluster.slo1`, 1)
    expect_equal(res.length.npatients$`length of D$nPatients in cluster.slo1`, 5637)
    expect_length(res.length.npatients$`length of D$nPatients in cluster.slo2`, 1)
    expect_equal(res.length.npatients$`length of D$nPatients in cluster.slo2`, 5563)
    expect_length(res.length.npatients$`length of D$nPatients in cluster.slo3`, 1)
    expect_equal(res.length.npatients$`length of D$nPatients in cluster.slo3`, 5837)
    expect_length(res.length.npatients$`total length of D$nPatients in all studies combined`, 1)
    expect_equal(res.length.npatients$`total length of D$nPatients in all studies combined`, 17037)

    res.class.male <- ds.class(x='D$Male')
    expect_length(res.class.male, 3)
    expect_length(res.class.male$cluster.slo1, 1)
    expect_equal(res.class.male$cluster.slo1, "factor")
    expect_length(res.class.male$cluster.slo2, 1)
    expect_equal(res.class.male$cluster.slo2, "factor")
    expect_length(res.class.male$cluster.slo3, 1)
    expect_equal(res.class.male$cluster.slo3, "factor")

    res.length.male <- ds.length(x='D$Male')
    expect_length(res.length.male, 4)
    expect_length(res.length.male$`length of D$Male in cluster.slo1`, 1)
    expect_equal(res.length.male$`length of D$Male in cluster.slo1`, 5637)
    expect_length(res.length.male$`length of D$Male in cluster.slo2`, 1)
    expect_equal(res.length.male$`length of D$Male in cluster.slo2`, 5563)
    expect_length(res.length.male$`length of D$Male in cluster.slo3`, 1)
    expect_equal(res.length.male$`length of D$Male in cluster.slo3`, 5837)
    expect_length(res.length.male$`total length of D$Male in all studies combined`, 1)
    expect_equal(res.length.male$`total length of D$Male in all studies combined`, 17037)

    res.class.age <- ds.class(x='D$age')
    expect_length(res.class.age, 3)
    expect_length(res.class.age$cluster.slo1, 1)
    expect_equal(res.class.age$cluster.slo1, "numeric")
    expect_length(res.class.age$cluster.slo2, 1)
    expect_equal(res.class.age$cluster.slo2, "numeric")
    expect_length(res.class.age$cluster.slo3, 1)
    expect_equal(res.class.age$cluster.slo3, "numeric")

    res.length.age <- ds.length(x='D$age')
    expect_length(res.length.age, 4)
    expect_length(res.length.age$`length of D$age in cluster.slo1`, 1)
    expect_equal(res.length.age$`length of D$age in cluster.slo1`, 5637)
    expect_length(res.length.age$`length of D$age in cluster.slo2`, 1)
    expect_equal(res.length.age$`length of D$age in cluster.slo2`, 5563)
    expect_length(res.length.age$`length of D$age in cluster.slo3`, 1)
    expect_equal(res.length.age$`length of D$age in cluster.slo3`, 5837)
    expect_length(res.length.age$`total length of D$age in all studies combined`, 1)
    expect_equal(res.length.age$`total length of D$age in all studies combined`, 17037)

    res.class.bmi <- ds.class(x='D$BMI')
    expect_length(res.class.bmi, 3)
    expect_length(res.class.bmi$cluster.slo1, 1)
    expect_equal(res.class.bmi$cluster.slo1, "numeric")
    expect_length(res.class.bmi$cluster.slo2, 1)
    expect_equal(res.class.bmi$cluster.slo2, "numeric")
    expect_length(res.class.bmi$cluster.slo3, 1)
    expect_equal(res.class.bmi$cluster.slo3, "numeric")

    res.length.bmi <- ds.length(x='D$BMI')
    expect_length(res.length.bmi, 4)
    expect_length(res.length.bmi$`length of D$BMI in cluster.slo1`, 1)
    expect_equal(res.length.bmi$`length of D$BMI in cluster.slo1`, 5637)
    expect_length(res.length.bmi$`length of D$BMI in cluster.slo2`, 1)
    expect_equal(res.length.bmi$`length of D$BMI in cluster.slo2`, 5563)
    expect_length(res.length.bmi$`length of D$BMI in cluster.slo3`, 1)
    expect_equal(res.length.bmi$`length of D$BMI in cluster.slo3`, 5837)
    expect_length(res.length.bmi$`total length of D$BMI in all studies combined`, 1)
    expect_equal(res.length.bmi$`total length of D$BMI in all studies combined`, 17037)

    res.class.private <- ds.class(x='D$private')
    expect_length(res.class.private, 3)
    expect_length(res.class.private$cluster.slo1, 1)
    expect_equal(res.class.private$cluster.slo1, "factor")
    expect_length(res.class.private$cluster.slo2, 1)
    expect_equal(res.class.private$cluster.slo2, "factor")
    expect_length(res.class.private$cluster.slo3, 1)
    expect_equal(res.class.private$cluster.slo3, "factor")

    res.length.private <- ds.length(x='D$private')
    expect_length(res.length.private, 4)
    expect_length(res.length.private$`length of D$private in cluster.slo1`, 1)
    expect_equal(res.length.private$`length of D$private in cluster.slo1`, 5637)
    expect_length(res.length.private$`length of D$private in cluster.slo2`, 1)
    expect_equal(res.length.private$`length of D$private in cluster.slo2`, 5563)
    expect_length(res.length.private$`length of D$private in cluster.slo3`, 1)
    expect_equal(res.length.private$`length of D$private in cluster.slo3`, 5837)
    expect_length(res.length.private$`total length of D$private in all studies combined`, 1)
    expect_equal(res.length.private$`total length of D$private in all studies combined`, 17037)

    res.class.diabetes <- ds.class(x='D$diabetes')
    expect_length(res.class.diabetes, 3)
    expect_length(res.class.diabetes$cluster.slo1, 1)
    expect_equal(res.class.diabetes$cluster.slo1, "factor")
    expect_length(res.class.diabetes$cluster.slo2, 1)
    expect_equal(res.class.diabetes$cluster.slo2, "factor")
    expect_length(res.class.diabetes$cluster.slo3, 1)
    expect_equal(res.class.diabetes$cluster.slo3, "factor")

    res.length.diabetes <- ds.length(x='D$diabetes')
    expect_length(res.length.diabetes, 4)
    expect_length(res.length.diabetes$`length of D$diabetes in cluster.slo1`, 1)
    expect_equal(res.length.diabetes$`length of D$diabetes in cluster.slo1`, 5637)
    expect_length(res.length.diabetes$`length of D$diabetes in cluster.slo2`, 1)
    expect_equal(res.length.diabetes$`length of D$diabetes in cluster.slo2`, 5563)
    expect_length(res.length.diabetes$`length of D$diabetes in cluster.slo3`, 1)
    expect_equal(res.length.diabetes$`length of D$diabetes in cluster.slo3`, 5837)
    expect_length(res.length.diabetes$`total length of D$diabetes in all studies combined`, 1)
    expect_equal(res.length.diabetes$`total length of D$diabetes in all studies combined`, 17037)

    res.class.incid_rate <- ds.class(x='D$incid_rate')
    expect_length(res.class.incid_rate, 3)
    expect_length(res.class.incid_rate$cluster.slo1, 1)
    expect_equal(res.class.incid_rate$cluster.slo1, "integer")
    expect_length(res.class.incid_rate$cluster.slo2, 1)
    expect_equal(res.class.incid_rate$cluster.slo2, "integer")
    expect_length(res.class.incid_rate$cluster.slo3, 1)
    expect_equal(res.class.incid_rate$cluster.slo3, "integer")

    res.length.incid_rate <- ds.length(x='D$incid_rate')
    expect_length(res.length.incid_rate, 4)
    expect_length(res.length.incid_rate$`length of D$incid_rate in cluster.slo1`, 1)
    expect_equal(res.length.incid_rate$`length of D$incid_rate in cluster.slo1`, 5637)
    expect_length(res.length.incid_rate$`length of D$incid_rate in cluster.slo2`, 1)
    expect_equal(res.length.incid_rate$`length of D$incid_rate in cluster.slo2`, 5563)
    expect_length(res.length.incid_rate$`length of D$incid_rate in cluster.slo3`, 1)
    expect_equal(res.length.incid_rate$`length of D$incid_rate in cluster.slo3`, 5837)
    expect_length(res.length.incid_rate$`total length of D$incid_rate in all studies combined`, 1)
    expect_equal(res.length.incid_rate$`total length of D$incid_rate in all studies combined`, 17037)
})

#
# Tear down
#

context("CLUSTER_SLO::datachk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D"))
})

disconnect.studies.dataset.cluster.slo()

context("CLUSTER_SLO::datachk::done")
