library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]

plot(crabs[,4:8], col=c("blue","red")[crabs$sex])
plot(crabs[,4:8], col=c("blue","orange")[crabs$sp])


#plot(crabs[,4:8], col=c("orange","blue")[crabs$sex], pch=c(0, 2)[crabs$sp])
#plot(x = crabs$RW, y = crabs$CL, col=c("orange","blue")[crabs$sex], pch=c(0, 2)[crabs$sp])

  female_crabs = crabs[crabs$sex == "F",]
cor(x = female_crabs$RW, y = female_crabs$CL)
male_crabs = crabs[crabs$sex == "M",]
cor(x = male_crabs$RW, y = male_crabs$CL)


plot(x = crabs$CL, y = crabs$RW, col=c("blue","red")[crabs$sex], xlab = "CL(mm)", ylab= "RW(mm)")
lm(crabsfemale$RW ~ crabsfemale$CL)
abline(lm(crabsfemale$RW ~ crabsfemale$CL))
lm(crabsmale$RW ~ crabsmale$CL)
abline(lm(crabsmale$RW ~ crabsmale$CL))


crabsorange = crabs[crabs$sp=="O",]
crabsblue = crabs[crabs$sp=="B",]

crabsorangequant=crabsorange[,4:8]
crabsbluequant=crabsblue[,4:8]

crabsfemale = crabs[crabs$sex=="F",]
crabsmale = crabs[crabs$sex=="M",]

crabsfemalequant=crabsfemale[,4:8]
crabsmalequant=crabsmale[,4:8]
#do colMeans for each
library(matrixStats)
#do colMedians for each

plot(crabs$sp, crabs$FL,ylab="FL (mm)")
plot(crabs$sp, crabs$RW, ylab = "RW (mm)")
plot(crabs$sp, crabs$CL, ylab = "CL (mm)")
plot(crabs$sp, crabs$BD, ylab = "BD (mm)")
plot(crabs$sp, crabs$CW, ylab = "CW (mm)")

plot(crabsquant/rowSums(crabsquant), col=c("Blue","Orange")[crabs$sp])
plot(crabsquant/rowSums(crabsquant), col=c("Blue","Red")[crabs$sex])
crabsquantpondere = crabsquant/rowSums(crabsquant)
