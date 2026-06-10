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

# context("ds.table::expt::setup")
connect.testing.group.dataset.1()

# context("ds.table::expt::single")
test_that("combined data set",
{
  #PRW AND ALEX WILL WRITE SOME CONNECT AND DISCONNECT FUNCTIONS ONCE WE KNOW THE DATA ARE SUITABLE   - TO DO IMPORTANT !!!!!!!! COMPLETELY INEFFICIENT AT THE MOMENT 17/2/2020
  #builder <- DSI::newDSLoginBuilder(.silent = TRUE)
  #builder$append(server = "study1", url = "http://192.168.56.100:8080/", user = "administrator", password = "datashield_test&", table = "TESTING_GROUP.GROUP1", driver = "OpalDriver")    
  #builder$append(server = "study2", url = "http://192.168.56.100:8080/", user = "administrator", password = "datashield_test&", table = "TESTING_GROUP.GROUP2", driver = "OpalDriver")
  #logindata <- builder$build()
  #print(logindata)
  
  #conns <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
  #print(conns)
  
  
  GROUP1 <- read.csv("data_files/GROUP1.csv")
  GROUP2 <- read.csv("data_files/GROUP2_test.csv")
  
  server.result <- ds.table("D$COLOURS", "D$POSITIVE.NUMBERS")
  local.result1  <-table(GROUP1$COLOURS, GROUP1$POSITIVE.NUMBERS)
  
  local.result2  <-table(GROUP2$COLOURS, GROUP2$POSITIVE.NUMBERS)
  
  vector_local_study1<-local.result1[c(seq(1,20))]
  vector_local_study2<-local.result2[c(seq(1,20))]
  vector_local_study_combined<-vector_local_study2+vector_local_study1
  vector_local_study_combined_matrix<-local.result1+local.result2
  
  vector_server <-server.result[1]$output.list[[6]][c(seq(1,20))]
  vector_server_study1<-server.result[1]$output.list[[7]][c(seq(1,20))]
  vector_server_study2<-server.result[1]$output.list[[8]][c(seq(1,20))]
  vector_server_study_combined<-server.result[1]$output.list[[9]][c(seq(1,20))]
  
  expect_true(length(vector_local_study1) == length(vector_server_study1))
  expect_true(length(vector_local_study2) == length(vector_server_study2))
  expect_true(length(vector_local_study_combined) == length(vector_server_study_combined))
  #STUDY 1 & 2 & COMBINED COUNTS 
  for (i in 1:length(vector_local_study1))
    {
      expect_equal(vector_local_study1[i], vector_server_study1[i], tolerance = ds.test_env$tolerance)
      expect_equal(vector_local_study2[i], vector_server_study2[i], tolerance = ds.test_env$tolerance)
      expect_equal(vector_local_study_combined[i], vector_server_study_combined[i], tolerance = ds.test_env$tolerance)
      }
  
  
  #STUDY 1 colprops
  for (j in 1:4) {
    study1_col_totals=c(seq(1,4))
    study1_col_totals[j]=sum(local.result1[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(server.result[1]$output.list[[2]][i], local.result1[i]/study1_col_totals[j],  tolerance =10^-3)
      }
  }
  
  #STUDY 2 colprops
  for (j in 1:4) {
    study2_col_totals=c(seq(1,4))
    study2_col_totals[j]=sum(local.result2[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(   server.result[1]$output.list[[4]][i]   ,   local.result2[i]/study2_col_totals[j],  tolerance =10^-3   )
      }
  }
  
  #combined colprops
  for (j in 1:4) {
    study3_col_totals=c(seq(1,4))
    study3_col_totals[j]=sum(vector_local_study_combined[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(   server.result[1]$output.list[[6]][i]   ,   vector_local_study_combined[i]/study3_col_totals[j],  tolerance =10^-3   )
    }
  }
  
  #STUDY 1 rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study1_current_row_total<-0
      study1_current_row_total<-sum(local.result1[[i,1]],local.result1[[i,2]],local.result1[[i,3]],local.result1[[i,4]])
      
      expect_equal(server.result[1]$output.list[[1]][[i,j]], local.result1[[i,j]]/study1_current_row_total,  tolerance =10^-3)
    }
  }
  #STUDY 2 rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study1_current_row_total<-0
      study1_current_row_total<-sum(local.result2[[i,1]],local.result2[[i,2]],local.result2[[i,3]],local.result2[[i,4]])
      
      expect_equal(server.result[1]$output.list[[3]][[i,j]], local.result2[[i,j]]/study1_current_row_total,  tolerance =10^-3)
    }
  }
  #COMBINED STUDY rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study_current_row_total<-0
      study_current_row_total<-sum(vector_local_study_combined_matrix[[i,1]],vector_local_study_combined_matrix[[i,2]],vector_local_study_combined_matrix[[i,3]],vector_local_study_combined_matrix[[i,4]])
      
      expect_equal(server.result[1]$output.list[[5]][[i,j]], vector_local_study_combined_matrix[[i,j]]/study_current_row_total,  tolerance =10^-3)
    }
  }
  #DSI::datashield.logout(conns)

  #########################################################################
  #########################################################################
  ################# checking negative numbers #############################
  #########################################################################
  #########################################################################
  
  server.result <- ds.table("D$COLOURS", "D$NEGATIVE.NUMBERS") ##############it works!!! ###############
  
  local.result1  <-table(GROUP1$COLOURS, GROUP1$NEGATIVE.NUMBERS)
  
  local.result2  <-table(GROUP2$COLOURS, GROUP2$NEGATIVE.NUMBERS)
  
  vector_local_study1<-local.result1[c(seq(1,20))]
  vector_local_study2<-local.result2[c(seq(1,20))]
  vector_local_study_combined<-vector_local_study2+vector_local_study1
  vector_local_study_combined_matrix<-local.result1+local.result2
  
  vector_server <-server.result[1]$output.list[[6]][c(seq(1,20))]
  vector_server_study1<-server.result[1]$output.list[[7]][c(seq(1,20))]
  vector_server_study2<-server.result[1]$output.list[[8]][c(seq(1,20))]
  vector_server_study_combined<-server.result[1]$output.list[[9]][c(seq(1,20))]
  
  expect_true(length(vector_local_study1) == length(vector_server_study1))
  expect_true(length(vector_local_study2) == length(vector_server_study2))
  expect_true(length(vector_local_study_combined) == length(vector_server_study_combined))
  #STUDY 1 & 2 & COMBINED COUNTS 
  for (i in 1:length(vector_local_study1))
  {
    expect_equal(vector_local_study1[i], vector_server_study1[i], tolerance = ds.test_env$tolerance)
    expect_equal(vector_local_study2[i], vector_server_study2[i], tolerance = ds.test_env$tolerance)
    expect_equal(vector_local_study_combined[i], vector_server_study_combined[i], tolerance = ds.test_env$tolerance)
  }
  
  
  #STUDY 1 colprops
  for (j in 1:4) {
    study1_col_totals=c(seq(1,4))
    study1_col_totals[j]=sum(local.result1[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(server.result[1]$output.list[[2]][i], local.result1[i]/study1_col_totals[j],  tolerance =10^-3)
    }
  }
  
  #STUDY 2 colprops
  for (j in 1:4) {
    study2_col_totals=c(seq(1,4))
    study2_col_totals[j]=sum(local.result2[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(   server.result[1]$output.list[[4]][i]   ,   local.result2[i]/study2_col_totals[j],  tolerance =10^-3   )
    }
  }
  
  #combined colprops
  for (j in 1:4) {
    study3_col_totals=c(seq(1,4))
    study3_col_totals[j]=sum(vector_local_study_combined[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(   server.result[1]$output.list[[6]][i]   ,   vector_local_study_combined[i]/study3_col_totals[j],  tolerance =10^-3   )
    }
  }
  
  #STUDY 1 rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study1_current_row_total<-0
      study1_current_row_total<-sum(local.result1[[i,1]],local.result1[[i,2]],local.result1[[i,3]],local.result1[[i,4]])
      
      expect_equal(server.result[1]$output.list[[1]][[i,j]], local.result1[[i,j]]/study1_current_row_total,  tolerance =10^-3)
    }
  }
  #STUDY 2 rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study1_current_row_total<-0
      study1_current_row_total<-sum(local.result2[[i,1]],local.result2[[i,2]],local.result2[[i,3]],local.result2[[i,4]])
      
      expect_equal(server.result[1]$output.list[[3]][[i,j]], local.result2[[i,j]]/study1_current_row_total,  tolerance =10^-3)
    }
  }
  #COMBINED STUDY rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study_current_row_total<-0
      study_current_row_total<-sum(vector_local_study_combined_matrix[[i,1]],vector_local_study_combined_matrix[[i,2]],vector_local_study_combined_matrix[[i,3]],vector_local_study_combined_matrix[[i,4]])
      
      expect_equal(server.result[1]$output.list[[5]][[i,j]], vector_local_study_combined_matrix[[i,j]]/study_current_row_total,  tolerance =10^-3)
    }
  }
  
  #########################################################################
  #########################################################################
  ################# checking character factors ############################
  #########################################################################
  #########################################################################
  
  server.result <- ds.table("D$COLOURS", "D$CHARACTERS") ##############it works!!! ###############
  
  local.result1  <-table(GROUP1$COLOURS, GROUP1$CHARACTERS)
  
  local.result2  <-table(GROUP2$COLOURS, GROUP2$CHARACTERS)
  
  vector_local_study1<-local.result1[c(seq(1,20))]
  vector_local_study2<-local.result2[c(seq(1,20))]
  vector_local_study_combined<-vector_local_study2+vector_local_study1
  vector_local_study_combined_matrix<-local.result1+local.result2
  
  vector_server <-server.result[1]$output.list[[6]][c(seq(1,20))]
  vector_server_study1<-server.result[1]$output.list[[7]][c(seq(1,20))]
  vector_server_study2<-server.result[1]$output.list[[8]][c(seq(1,20))]
  vector_server_study_combined<-server.result[1]$output.list[[9]][c(seq(1,20))]
  
  expect_true(length(vector_local_study1) == length(vector_server_study1))
  expect_true(length(vector_local_study2) == length(vector_server_study2))
  expect_true(length(vector_local_study_combined) == length(vector_server_study_combined))
  #STUDY 1 & 2 & COMBINED COUNTS 
  for (i in 1:length(vector_local_study1))
  {
    expect_equal(vector_local_study1[i], vector_server_study1[i], tolerance = ds.test_env$tolerance)
    expect_equal(vector_local_study2[i], vector_server_study2[i], tolerance = ds.test_env$tolerance)
    expect_equal(vector_local_study_combined[i], vector_server_study_combined[i], tolerance = ds.test_env$tolerance)
  }
  
  
  #STUDY 1 colprops
  for (j in 1:4) {
    study1_col_totals=c(seq(1,4))
    study1_col_totals[j]=sum(local.result1[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(server.result[1]$output.list[[2]][i], local.result1[i]/study1_col_totals[j],  tolerance =10^-3)
    }
  }
  
  #STUDY 2 colprops
  for (j in 1:4) {
    study2_col_totals=c(seq(1,4))
    study2_col_totals[j]=sum(local.result2[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(   server.result[1]$output.list[[4]][i]   ,   local.result2[i]/study2_col_totals[j],  tolerance =10^-3   )
    }
  }
  
  #combined colprops
  for (j in 1:4) {
    study3_col_totals=c(seq(1,4))
    study3_col_totals[j]=sum(vector_local_study_combined[c(seq(5*j-4,5*j))])
    for (i in (5*j-4):(5*j)) {
      expect_equal(   server.result[1]$output.list[[6]][i]   ,   vector_local_study_combined[i]/study3_col_totals[j],  tolerance =10^-3   )
    }
  }
  
  #STUDY 1 rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study1_current_row_total<-0
      study1_current_row_total<-sum(local.result1[[i,1]],local.result1[[i,2]],local.result1[[i,3]],local.result1[[i,4]])
      
      expect_equal(server.result[1]$output.list[[1]][[i,j]], local.result1[[i,j]]/study1_current_row_total,  tolerance =10^-3)
    }
  }
  #STUDY 2 rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study1_current_row_total<-0
      study1_current_row_total<-sum(local.result2[[i,1]],local.result2[[i,2]],local.result2[[i,3]],local.result2[[i,4]])
      
      expect_equal(server.result[1]$output.list[[3]][[i,j]], local.result2[[i,j]]/study1_current_row_total,  tolerance =10^-3)
    }
  }
  #COMBINED STUDY rowprops
  for (j in 1:4) {
    for(i in 1:5){
      study_current_row_total<-0
      study_current_row_total<-sum(vector_local_study_combined_matrix[[i,1]],vector_local_study_combined_matrix[[i,2]],vector_local_study_combined_matrix[[i,3]],vector_local_study_combined_matrix[[i,4]])
      
      expect_equal(server.result[1]$output.list[[5]][[i,j]], vector_local_study_combined_matrix[[i,j]]/study_current_row_total,  tolerance =10^-3)
    }
  }
  
  }
)
disconnect.testing.group.dataset.1()
