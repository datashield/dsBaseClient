# THIS SCRIPT IS MEANT TO TEST THE FUNCTIONS IN THE PACKAGE 'dsbaseclient'

# LOAD REQUIRED LIBRARIES AND FUNCTIONS AND LOGIN TO SERVERS 
library('opal')
server1 <- opal.login('*******', '******', 'http://54.242.140.255')
server2 <- opal.login('*******', '******', 'http://54.242.46.59')
opals <- list(server1,server2)

# ASSIGN DATA FROM OPAL SOURCE TO R
datashield.assign(server1, 'D', 'HOPsim.hopsim1ob')
datashield.assign(server2, 'D', 'HOPsim.hopsim2ob')

# DATASHIELD LOADS DATA AS PARLISTS SO HERE WE CHANGE IT INTO DATAFRAMES AND ORDER BY INDIVIDUAL IDs
datashield.assign(opals, "T", quote(data.frame(as.list(D))))
datashield.assign(opals, "D", quote(order.frame(T, T$ind.id)))

# test the contour.plot function 
datashield.contour.plot(opals, quote(D$glu), quote(D$bmi))

# test the heatmap.plot function 
datashield.heatmap.plot(opals, quote(D$glu), quote(D$bmi))

# test the histogram.plot function 
datashield.histogram(opals, quote(D$glu))

# test the table.2d function 
datashield.table.2d(opals, quote(D$sex), quote(D$smoke))



