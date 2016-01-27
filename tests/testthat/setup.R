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
# Datashield test suite set up
#

#library(dsbaseclient)
#library(testthat)

options(verbose=FALSE)

options(opal.username='administrator', 
        opal.password='password')

options(opal.url='http://localhost:8080')
#options(opal.url='http://demo.obiba.org:8080')

server <- c("sim1", "sim2", "sim3")
url <- c(getOption("opal.url"), getOption("opal.url"), getOption("opal.url"))
user <- c(getOption("opal.username"), getOption("opal.username"), getOption("opal.username"))
password <- c(getOption("opal.password"), getOption("opal.password"), getOption("opal.password"))
table <- c("CNSIM.CNSIM1", "CNSIM.CNSIM2", "CNSIM.CNSIM3")
logindata <- data.frame(server,url,user,password,table)

opals <- opal::datashield.login(logins=logindata,assign=TRUE,variables=getOption("datashield.variables", NULL))

