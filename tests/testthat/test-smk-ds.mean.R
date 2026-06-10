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

# context("ds.mean::smk::setup")

connect.studies.dataset.cnsim(list("LAB_TSC"))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

# context("ds.mean::smk::type=combine")
test_that("mean values [combine]", {
    ds.rm("mean.all.studies")
    ds.rm("mean.study.specific")
    ds.rm("Nvalid.all.studies")
    ds.rm("Nvalid.study.specific")

    stat.mean <- ds.mean(x='D$LAB_TSC',type='combine')

    expect_length(stat.mean, 3)
    expect_length(stat.mean$Global.Mean, 4)
    expect_equal(as.numeric(stat.mean$Global.Mean[1]), 5.85192485623003, tolerance = .000000000000001)
    expect_equal(as.integer(stat.mean$Global.Mean[2]), 1554)
    expect_equal(as.integer(stat.mean$Global.Mean[3]), 7825)
    expect_equal(as.integer(stat.mean$Global.Mean[4]), 9379)
    expect_equal(stat.mean$Nstudies, 3)
    expect_length(stat.mean$ValidityMessage, 3)
    expect_equal(stat.mean$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[3], "VALID ANALYSIS")

    ls.res <- ds.ls()
    expect_false("mean.all.studies" %in% ls.res$sim1$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim1$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim1$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim1$objects.found)
    expect_false("mean.all.studies" %in% ls.res$sim2$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim2$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim2$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim2$objects.found)
    expect_false("mean.all.studies" %in% ls.res$sim3$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim3$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim3$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim3$objects.found)
})

# context("ds.mean::smk::type=split")
test_that("mean values [split]", {
    ds.rm("mean.all.studies")
    ds.rm("mean.study.specific")
    ds.rm("Nvalid.all.studies")
    ds.rm("Nvalid.study.specific")

    stat.mean <- ds.mean(x='D$LAB_TSC', type='split')

    expect_length(stat.mean, 3)
    expect_length(stat.mean$Mean.by.Study, 12)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[1]), 5.87211344770338, tolerance = .000000000000001)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[2]), 5.84526388341867, tolerance = .000000000000001)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[3]), 5.84630008623168, tolerance = .000000000000001)
    expect_equal(as.integer(stat.mean$Mean.by.Study[4]), 356)
    expect_equal(as.integer(stat.mean$Mean.by.Study[5]), 549)
    expect_equal(as.integer(stat.mean$Mean.by.Study[6]), 649)
    expect_equal(as.integer(stat.mean$Mean.by.Study[7]), 1807)
    expect_equal(as.integer(stat.mean$Mean.by.Study[8]), 2539)
    expect_equal(as.integer(stat.mean$Mean.by.Study[9]), 3479)
    expect_equal(as.integer(stat.mean$Mean.by.Study[10]), 2163)
    expect_equal(as.integer(stat.mean$Mean.by.Study[11]), 3088)
    expect_equal(as.integer(stat.mean$Mean.by.Study[12]), 4128)
    expect_equal(stat.mean$Nstudies, 3)
    expect_length(stat.mean$ValidityMessage, 3)
    expect_equal(stat.mean$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[3], "VALID ANALYSIS")

    ls.res <- ds.ls()
    expect_false("mean.all.studies" %in% ls.res$sim1$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim1$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim1$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim1$objects.found)
    expect_false("mean.all.studies" %in% ls.res$sim2$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim2$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim2$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim2$objects.found)
    expect_false("mean.all.studies" %in% ls.res$sim3$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim3$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim3$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim3$objects.found)
})

# context("ds.mean::smk::type=both")
test_that("mean values [both]", {
    ds.rm("mean.all.studies")
    ds.rm("mean.study.specific")
    ds.rm("Nvalid.all.studies")
    ds.rm("Nvalid.study.specific")

    stat.mean <- ds.mean(x='D$LAB_TSC', type='both')

    expect_length(stat.mean, 4)
    expect_length(stat.mean$Mean.by.Study, 12)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[1]), 5.87211344770338, tolerance = .000000000000001)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[2]), 5.84526388341867, tolerance = .000000000000001)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[3]), 5.84630008623168, tolerance = .000000000000001)
    expect_equal(as.integer(stat.mean$Mean.by.Study[4]), 356)
    expect_equal(as.integer(stat.mean$Mean.by.Study[5]), 549)
    expect_equal(as.integer(stat.mean$Mean.by.Study[6]), 649)
    expect_equal(as.integer(stat.mean$Mean.by.Study[7]), 1807)
    expect_equal(as.integer(stat.mean$Mean.by.Study[8]), 2539)
    expect_equal(as.integer(stat.mean$Mean.by.Study[9]), 3479)
    expect_equal(as.integer(stat.mean$Mean.by.Study[10]), 2163)
    expect_equal(as.integer(stat.mean$Mean.by.Study[11]), 3088)
    expect_equal(as.integer(stat.mean$Mean.by.Study[12]), 4128)
    expect_length(stat.mean$Global.Mean, 4)
    expect_equal(as.numeric(stat.mean$Global.Mean[1]), 5.85192485623003, tolerance = .000000000000001)
    expect_equal(as.integer(stat.mean$Global.Mean[2]), 1554)
    expect_equal(as.integer(stat.mean$Global.Mean[3]), 7825)
    expect_equal(as.integer(stat.mean$Global.Mean[4]), 9379)
    expect_equal(stat.mean$Nstudies, 3)
    expect_length(stat.mean$ValidityMessage, 3)
    expect_equal(stat.mean$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[3], "VALID ANALYSIS")

    ls.res <- ds.ls()
    expect_false("mean.all.studies" %in% ls.res$sim1$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim1$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim1$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim1$objects.found)
    expect_false("mean.all.studies" %in% ls.res$sim2$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim2$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim2$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim2$objects.found)
    expect_false("mean.all.studies" %in% ls.res$sim3$objects.found)
    expect_false("mean.study.specific" %in% ls.res$sim3$objects.found)
    expect_false("Nvalid.all.studies" %in% ls.res$sim3$objects.found)
    expect_false("Nvalid.study.specific" %in% ls.res$sim3$objects.found)
})

# context("ds.mean::smk::type=combine,save.mean.Nvalid=TRUE")
test_that("mean values [combine]", {
    ds.rm("mean.all.studies")
    ds.rm("mean.study.specific")
    ds.rm("Nvalid.all.studies")
    ds.rm("Nvalid.study.specific")

    stat.mean <- ds.mean(x='D$LAB_TSC', type='combine', save.mean.Nvalid=TRUE)

    expect_length(stat.mean, 3)
    expect_length(stat.mean$Global.Mean, 4)
    expect_equal(as.numeric(stat.mean$Global.Mean[1]), 5.85192485623003, tolerance = .000000000000001)
    expect_equal(as.integer(stat.mean$Global.Mean[2]), 1554)
    expect_equal(as.integer(stat.mean$Global.Mean[3]), 7825)
    expect_equal(as.integer(stat.mean$Global.Mean[4]), 9379)
    expect_equal(stat.mean$Nstudies, 3)
    expect_length(stat.mean$ValidityMessage, 3)
    expect_equal(stat.mean$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[3], "VALID ANALYSIS")

    ls.res <- ds.ls()
    expect_true("mean.all.studies" %in% ls.res$sim1$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim1$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim1$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim1$objects.found)
    expect_true("mean.all.studies" %in% ls.res$sim2$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim2$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim2$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim2$objects.found)
    expect_true("mean.all.studies" %in% ls.res$sim3$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim3$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim3$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim3$objects.found)
})

# context("ds.mean::smk::type=split,save.mean.Nvalid=TRUE")
test_that("mean values [split]", {
    ds.rm("mean.all.studies")
    ds.rm("mean.study.specific")
    ds.rm("Nvalid.all.studies")
    ds.rm("Nvalid.study.specific")

    stat.mean <- ds.mean(x='D$LAB_TSC', type='split', save.mean.Nvalid=TRUE)

    expect_length(stat.mean, 3)
    expect_length(stat.mean$Mean.by.Study, 12)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[1]), 5.87211344770338, tolerance = .000000000000001)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[2]), 5.84526388341867, tolerance = .000000000000001)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[3]), 5.84630008623168, tolerance = .000000000000001)
    expect_equal(as.integer(stat.mean$Mean.by.Study[4]), 356)
    expect_equal(as.integer(stat.mean$Mean.by.Study[5]), 549)
    expect_equal(as.integer(stat.mean$Mean.by.Study[6]), 649)
    expect_equal(as.integer(stat.mean$Mean.by.Study[7]), 1807)
    expect_equal(as.integer(stat.mean$Mean.by.Study[8]), 2539)
    expect_equal(as.integer(stat.mean$Mean.by.Study[9]), 3479)
    expect_equal(as.integer(stat.mean$Mean.by.Study[10]), 2163)
    expect_equal(as.integer(stat.mean$Mean.by.Study[11]), 3088)
    expect_equal(as.integer(stat.mean$Mean.by.Study[12]), 4128)
    expect_equal(stat.mean$Nstudies, 3)
    expect_length(stat.mean$ValidityMessage, 3)
    expect_equal(stat.mean$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[3], "VALID ANALYSIS")

    ls.res <- ds.ls()
    expect_true("mean.all.studies" %in% ls.res$sim1$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim1$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim1$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim1$objects.found)
    expect_true("mean.all.studies" %in% ls.res$sim2$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim2$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim2$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim2$objects.found)
    expect_true("mean.all.studies" %in% ls.res$sim3$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim3$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim3$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim3$objects.found)
})

# context("ds.mean::smk::type=both,save.mean.Nvalid=TRUE")
test_that("mean values [both]", {
    stat.mean <- ds.mean(x='D$LAB_TSC', type='both', save.mean.Nvalid=TRUE)

    expect_length(stat.mean, 4)
    expect_length(stat.mean$Mean.by.Study, 12)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[1]), 5.87211344770338, tolerance = .000000000000001)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[2]), 5.84526388341867, tolerance = .000000000000001)
    expect_equal(as.numeric(stat.mean$Mean.by.Study[3]), 5.84630008623168, tolerance = .000000000000001)
    expect_equal(as.integer(stat.mean$Mean.by.Study[4]), 356)
    expect_equal(as.integer(stat.mean$Mean.by.Study[5]), 549)
    expect_equal(as.integer(stat.mean$Mean.by.Study[6]), 649)
    expect_equal(as.integer(stat.mean$Mean.by.Study[7]), 1807)
    expect_equal(as.integer(stat.mean$Mean.by.Study[8]), 2539)
    expect_equal(as.integer(stat.mean$Mean.by.Study[9]), 3479)
    expect_equal(as.integer(stat.mean$Mean.by.Study[10]), 2163)
    expect_equal(as.integer(stat.mean$Mean.by.Study[11]), 3088)
    expect_equal(as.integer(stat.mean$Mean.by.Study[12]), 4128)
    expect_length(stat.mean$Global.Mean, 4)
    expect_equal(as.numeric(stat.mean$Global.Mean[1]), 5.85192485623003, tolerance = .000000000000001)
    expect_equal(as.integer(stat.mean$Global.Mean[2]), 1554)
    expect_equal(as.integer(stat.mean$Global.Mean[3]), 7825)
    expect_equal(as.integer(stat.mean$Global.Mean[4]), 9379)
    expect_equal(stat.mean$Nstudies, 3)
    expect_length(stat.mean$ValidityMessage, 3)
    expect_equal(stat.mean$ValidityMessage[1], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[2], "VALID ANALYSIS")
    expect_equal(stat.mean$ValidityMessage[3], "VALID ANALYSIS")

    ls.res <- ds.ls()
    expect_true("mean.all.studies" %in% ls.res$sim1$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim1$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim1$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim1$objects.found)
    expect_true("mean.all.studies" %in% ls.res$sim2$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim2$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim2$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim2$objects.found)
    expect_true("mean.all.studies" %in% ls.res$sim3$objects.found)
    expect_true("mean.study.specific" %in% ls.res$sim3$objects.found)
    expect_true("Nvalid.all.studies" %in% ls.res$sim3$objects.found)
    expect_true("Nvalid.study.specific" %in% ls.res$sim3$objects.found)
})

#
# Done
#

# context("ds.mean::smk::shutdown")

test_that("shutdown", {
    ds_expect_variables(c("D", "mean.all.studies", "mean.study.specific", "Nvalid.all.studies", "Nvalid.study.specific"))
})

disconnect.studies.dataset.cnsim()

# context("ds.mean::smk::done")
