#Nectar Production

#get from nectar volume (g sugar/L nectar to Molarity)
#molecular weight of sucrose (sugar) = 342.296
molarity = function(volume){
  m = volume / 342.296
  return (round(m,3))
}

#table data from Kearns & Inouye 1993, Pollination Biology book
sucrose = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 
            11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 
            42, 44, 46, 48, 50, 52, 56,58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84)
gL = c(5, 10, 15.1, 20.1, 25.2, 30.3, 35.4, 40.6, 45.7, 50.9, 56.1, 61.3, 66.5, 71.8, 77.1, 82.4, 87.7,
       93.1, 98.4, 103.8, 114.7, 125.6, 136.6, 147.7, 158.9, 170.2, 181.5, 193.0, 204.5, 216.2, 
       239.8, 263.8, 288.1, 312.9, 338.1, 363.7, 389.8, 416.2, 443.2, 470.6, 498.4, 526.8, 555.6,
       584.9, 614.8, 645.1, 707.4, 739.3, 771.9, 804.9, 838.6, 872.8, 907.6, 943.1, 979.1, 1015.7,
       1053.0, 1090.9, 1129.4, 1168.5, 1208.2)
moles = c(0.015, 0.029, 0.044, 0.059, 0.074, 0.089, 0.103, 0.118, 0.134, 0.149, 0.164, 0.179, 0.194, 
          0.210, 0.225, 0.241, 0.256, 0.272, 0.288, 0.303, 0.335, 0.367, 0.399, 0.431, 0.464, 0.497, 
          0.530, 0.564, 0.598, 0.632, 0.701, 0.771, 0.842, 0.914, 0.988, 1.063, 1.139, 1.216, 1.295,
          1.375, 1.456, 1.539, 1.623, 1.709, 1.796, 1.885, 2.067, 2.160, 2.255, 2.352, 2.450,
          2.550, 2.652, 2.755, 2.860, 2.967, 3.076, 3.187, 3.299, 3.414, 3.530)

nectardata = as.data.frame(cbind(sucrose, gL, moles))

#relationships between the data measurements
plot(sucrose, gL, pch = 19)
plot(gL, moles, pch = 19)
  abline(lm(moles~gL), col = "red", cex = 2)
plot(sucrose, moles, pch=19)

#VOLUME
  #fit a 4th order polynomial in R
model = lm(gL ~ sucrose + I(sucrose^2) + I(sucrose^3) + I(sucrose^4))
summary(model)

  #equation relating sucrose to moles, from lm fit
sucrose_to_gL = function(sucrose){
  gL = (sucrose*1.000e+01 + (3.684e-02 * sucrose^2) + (1.772e-04 * sucrose^3) + (6.205e-08 * sucrose^4))
  return(gL)
}

##MOLARITY
  #fit a 4th order polynomial in R
molmodel = lm(moles ~ sucrose + I(sucrose^2) + I(sucrose^3) + I(sucrose^4))

  #equation relating sucrose to moles, from lm fit
sucrose_to_molarity = function(sucrose){
  molarity = (2.92e-02*sucrose + (1.097e-04 * sucrose^2) + (4.761e-07 * sucrose^3) + (4.310e-10 * sucrose^4))
  return(molarity)
}


#Check that the predictions fit. Red line should fall on top of black line
  #VOLUME
plot(sucrose, gL, pch=19, cex = 0.75)
gLpred = as.numeric()
for (i in 1:length(sucrose)){
  l = sucrose_to_gL(sucrose[i])
  gLpred = append(gLpred, l)
}
points(sucrose, gLpred, col = "red", type = "l", lwd = 3)

  #MOLARITY
plot(sucrose, moles, pch=19, cex = 0.75)
molpred = as.numeric()
for (i in 1:length(sucrose)){
  l = sucrose_to_molarity(sucrose[i])
  molpred = append(molpred, l)
}
points(sucrose, molpred, col = "red", type = "l", lwd = 3)

#check that the predicted and table data are the same
plot(gL, moles, pch = 19)
points(gLpred, molpred, col = "red", type = "l", lwd = 3)
