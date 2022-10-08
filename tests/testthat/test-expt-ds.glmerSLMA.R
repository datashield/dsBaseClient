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

context("ds.glmerSLMA::expt::setup")
#source("connection_to_datasets/init_all_datasets.R")
connect.studies.dataset.cluster.int(list('idChild','idSurgery','trtGrp','s0','nDoctors',
                                         'idDoctor','c0','nPatients','Male','age','BMI',
                                         'diabetes','incid_rate','private'))

test_that("setup", {
    ds_expect_variables(c("D"))
})

#
# Tests
#

context("ds.glmerSLMA::expt::int::multiple")
test_that("linear mixed model with 2 levels of hierarchy",
          {

            res.server <- ds.glmerSLMA(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor) +(1|idSurgery)', family = 'poisson', dataName = "D", combine.with.metafor = TRUE )

            num.studies <- 3
            numcoefficients <- 3
            
            betamatrix<-matrix(NA,nrow=numcoefficients,ncol=num.studies)
            sematrix<-matrix(NA,nrow=numcoefficients,ncol=num.studies)
            
            for(k in 1:num.studies){
              my_data = eval(parse(text = paste0("ds.test_env$local.values.", k)))
              res.local <- summary(lme4::glmer(formula = 'incid_rate ~ trtGrp + Male + (1|idDoctor) +(1|idSurgery)', family = 'poisson', data = my_data ))
              #eval(parse(text = paste0("ds.test_env$local.values.", k)))
              betamatrix[,k]<-res.local$coefficients[,1]
              sematrix[,k]<-res.local$coefficients[,2]
            }
              
            SLMA.pooled.ests.matrix<-matrix(NA,nrow<-numcoefficients,ncol=6)
            
            for(p in 1:numcoefficients){
              rma.ML<-metafor::rma(yi=as.matrix(betamatrix)[p,], sei=as.matrix(sematrix)[p,], method="ML")
              rma.REML<-metafor::rma(yi=as.matrix(betamatrix)[p,], sei=as.matrix(sematrix)[p,], method="REML")
              rma.FE<-metafor::rma(yi=as.matrix(betamatrix)[p,], sei=as.matrix(sematrix)[p,], method="FE")
              
              SLMA.pooled.ests.matrix[p,1]<-rma.ML$beta
              SLMA.pooled.ests.matrix[p,2]<-rma.ML$se
              
              SLMA.pooled.ests.matrix[p,3]<-rma.REML$beta
              SLMA.pooled.ests.matrix[p,4]<-rma.REML$se
              
              SLMA.pooled.ests.matrix[p,5]<-rma.FE$beta
              SLMA.pooled.ests.matrix[p,6]<-rma.FE$se   
            }
            
            #check meta analyses are the same both locally and on the server
            
#            print(SLMA.pooled.ests.matrix)
#            print(res.server$SLMA.pooled.ests.matrix)
            
            expect_equal(SLMA.pooled.ests.matrix, res.server$SLMA.pooled.ests.matrix, tolerance = 1.0e-5, check.attributes = FALSE)
          }
)

#
# Shutdown
#

context("ds.lmerSLMA::expt::shutdown")

disconnect.studies.dataset.cluster.int()

#
# Done
#

context("ds.lmerSLMA::expt::done")
