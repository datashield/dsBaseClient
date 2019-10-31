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

context("ds.lexis::smk:: setup")
connect.studies.dataset.survival(list("id", "starttime", "endtime", "cens"))

#
# Tests
#

context("ds.lexis::smk")
test_that("simple lexis", {
    res <- ds.lexis(data='D', intervalWidth = 2.5, idCol = 'D$ID', entryCol = 'D$STARTTIME', exitCol = 'D$ENDTIME', statusCol = 'D$CENS', expandDF = 'EM.new')

    expect_length(res, 3)
})

#
# Done
#

disconnect.studies.dataset.survival()
