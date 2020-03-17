


library("simstudy")

set.seed(42)
gen.surgery <- defData(varname = "intSurgery", dist = "normal", formula = 0, variance = 3, 
                       id = "idSurgery")
gen.surgery <- defData(gen.surgery, varname = "nDoctors", dist = "noZeroPoisson", 
                       formula = 3)

dtSurgery <- genData(8, gen.surgery)
dtSurgery <- trtAssign(dtSurgery, n = 2)

gen.doctor <- defDataAdd(varname = "intDoctor", dist = "normal", formula = 0, variance = 2)

gen.doctor <- defDataAdd(gen.doctor, varname = "nPatients", dist = "noZeroPoisson", 
                         formula = 90)

dtDoctor <- genCluster(dtSurgery, "idSurgery", numIndsVar = "nDoctors", level1ID = "idDoctor")
dtDoctor <- addColumns(gen.doctor, dtDoctor)


gen.patient <- defDataAdd(varname = "Male", dist = "binary", 
                          formula = 0.5)
gen.patient <- defDataAdd(gen.patient, varname = "age", dist = "uniform", 
                          formula = "9.5; 10.5")
gen.patient <- defDataAdd(gen.patient, varname = "BMI", dist = "normal", 
                          formula = "20 + 5*Male + intSurgery + intDoctor + 8 * trtGrp", variance = 2)
gen.patient <- defDataAdd(gen.patient, varname = "diabetes", dist = "binary", 
                          formula = "(20 + 5*Male + intSurgery + intDoctor + 8 * trtGrp)-25", link = 'logit')
gen.patient <- defDataAdd(gen.patient, varname = "incid_rate", dist = "poisson", 
                          formula = "(20 + 5*Male + intSurgery + intDoctor + 8 * trtGrp)*0.1", link='log')
dtPatient <- genCluster(dtDoctor, cLevelVar = "idDoctor", numIndsVar = "nPatients", 
                        level1ID = "idPatient")

dtPatient <- addColumns(gen.patient, dtPatient)


dtPatient$private = 1

dtPatient <- dtPatient[, c(8, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14)]
# Split data into 3 groups
set.seed(42)
ss <- sample(1:3,size=nrow(dtPatient),replace=TRUE,prob=c(0.33,0.33,0.34))
dtPatient1 <- dtPatient[ss==1,]
dtPatient2 <- dtPatient[ss==2,]
dtPatient3 <- dtPatient[ss==3,]

#change first Patient to have a different value 
dtPatient1$private[1]=0
dtPatient2$private[1]=0 
dtPatient3$private[1]=0


save(dtPatient,file = 'CLUSTER_int.RData')
save(dtPatient1,file = 'CLUSTER_int1.RData')
save(dtPatient2,file = 'CLUSTER_int2.RData')
save(dtPatient3,file = 'CLUSTER_int3.RData')
write.csv(dtPatient1,file = 'CLUSTER_INT1.csv', row.names = FALSE)
write.csv(dtPatient2,file = 'CLUSTER_INT2.csv', row.names = FALSE)
write.csv(dtPatient3,file = 'CLUSTER_INT3.csv', row.names = FALSE)


#random intercepts for surgery and doctor, and random slope for doctor. For doctor the slope and intercept
# are not correlated because they are generated independently

set.seed(42)

gen.surgery <- defData(varname = "intSurgery", dist = "normal", formula = 0, variance = 3, 
                      id = "idSurgery")
gen.surgery <- defData(gen.surgery, varname = "nDoctors", dist = "noZeroPoisson", 
                      formula = 3)
# if wanted a random slope for surgery use this
# gen.surgery <- defData(gen.surgery, varname = "sloSurgery", dist = "normal", formula = 0, variance = 2)


dtSurgery <- genData(8, gen.surgery)
dtSurgery <- trtAssign(dtSurgery, n = 2)


gen.doctor <- defDataAdd(varname = "intDoctor", dist = "normal", formula = 0, variance = 2)
gen.doctor <- defDataAdd(gen.doctor,varname = "sloDoctor", dist = "normal", formula = 0, variance = 2)
gen.doctor <- defDataAdd(gen.doctor, varname = "nPatients", dist = "noZeroPoisson", 
                        formula = 500)

dtDoctor <- genCluster(dtSurgery, "idSurgery", numIndsVar = "nDoctors", level1ID = "idDoctor")
dtDoctor <- addColumns(gen.doctor, dtDoctor)


gen.patient <- defDataAdd(varname = "Male", dist = "binary", 
                          formula = 0.5)
gen.patient <- defDataAdd(gen.patient, varname = "age", dist = "uniform", 
                          formula = "9.5; 10.5")
gen.patient <- defDataAdd(gen.patient, varname = "BMI", dist = "normal", 
                          formula = "20 + 5*Male +intDoctor + intSurgery + ((8 + sloDoctor) * trtGrp)", variance = 2)
gen.patient <- defDataAdd(gen.patient, varname = "diabetes", dist = "binary", 
                          formula = "(20 + 5*Male + intSurgery + intDoctor + ((8 + sloDoctor) * trtGrp))-25", link = 'logit')
gen.patient <- defDataAdd(gen.patient, varname = "incid_rate", dist = "poisson", 
                          formula = "(20 + 5*Male + intSurgery + intDoctor + ((8 + sloDoctor) * trtGrp))*0.1", link='log')
dtPatient <- genCluster(dtDoctor, cLevelVar = "idDoctor", numIndsVar = "nPatients", 
                        level1ID = "idPatient")

dtPatient <- addColumns(gen.patient, dtPatient)


dtPatient$private = 1

dtPatient <- dtPatient[, c(9, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14,15)]

# Split data into 3 groups
set.seed(42)
ss <- sample(1:3,size=nrow(dtPatient),replace=TRUE,prob=c(0.33,0.33,0.34))
dtPatient1 <- dtPatient[ss==1,]
dtPatient2 <- dtPatient[ss==2,]
dtPatient3 <- dtPatient[ss==3,]

#change first Patient to have a different value
dtPatient1$private[1]=0
dtPatient2$private[1]=0 
dtPatient3$private[1]=0

save(dtPatient,file = 'CLUSTER_SLO.RData')
save(dtPatient1,file = 'CLUSTER_SLO1.RData')
save(dtPatient2,file = 'CLUSTER_SLO2.RData')
save(dtPatient3,file = 'CLUSTER_SLO3.RData')
write.csv(dtPatient1,file = 'CLUSTER_SLO1.csv', row.names = FALSE)
write.csv(dtPatient2,file = 'CLUSTER_SLO2.csv', row.names = FALSE)
write.csv(dtPatient3,file = 'CLUSTER_SLO3.csv', row.names = FALSE)

