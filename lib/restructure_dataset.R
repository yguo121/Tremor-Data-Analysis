### Restructure the Dataset to get Train and Test set ###

library(tidyverse)
library(MASS)
library(ggplot2)
library(abind)

#setwd("~/Documents/GitHub/Tremor-Data-Analysis/lib")
source("../lib/hertz_dataframe_tools.R")


########################################
##### Construct Train Set ########
########################################


# load 11 Firsthand csv and combine to one dataframe 
dataFiles <- lapply(Sys.glob("20180708*.csv"), read.csv)

# Each person with 2 mins record and fft using 1 second
hert <- get_hertz(dataFiles[[1]][1:(120*33),])

dt_bind <- dataFiles[[1]]
for (i in 2:12) {
  dt_bind <- abind(dt_bind,dataFiles[[i]],along = 1)
}

# extract 9 varables we interested in
dt_new <- dt_bind[,7:15]
mat <- array(NA,dim = c(338,18))

for (i in 0:337) {
  dt = dt_new[(120*33*i+1):(120*33*(i+1)),]
  for (j in 1:9) {
    fft = get_fft_freq_amp(dt[,j],1,hert)
    mat[i+1,c(2*j-1,2*j)] = get_amp_freq(fft)
  }
}
# remove the row with -Inf
mat <- mat[-246,]

# transfer mat to dataframe
train_firsthand <- as.data.frame(mat)
colnames(train_firsthand) = c("ampRotation_x","freqRotation_x","ampRotation_y","freqRotation_y","ampRotation_z","freqRotation_z","ampGravity_x","freqGravity_x","ampGravity_y","freqGravity_y","ampGravity_z","freqGravity_z","ampAcceleration_x","freqAcceleration_x","ampAcceleration_y","freqAcceleration_y","ampAcceleration_z","freqAcceleration_z")
train_firsthand$label <- 0


# load 11 Secondhand csv and combine to one dataframe
dataFiles2 <- lapply(Sys.glob("2018071*.csv"), read.csv)
dt_bind2 <- dataFiles2[[1]]
for (i in 2:12) {
  dt_bind2 <- abind(dt_bind2,dataFiles[[i]],along = 1)
}

# extract 9 varables we interested in
dt_new2 <- dt_bind2[,7:15]
mat2 <- array(NA,dim = c(338,18))

for (i in 0:337) {
  dt2 = dt_new2[(120*33*i+1):(120*33*(i+1)),]
  for (j in 1:9) {
    fft = get_fft_freq_amp(dt2[,j],1,hert)
    mat2[i+1,c(2*j-1,2*j)] = get_amp_freq(fft)
  }
}
# remove the row with -Inf
mat2 <- mat2[-246,]

# transfer mat2 to dataframe
train_secondhand <- as.data.frame(mat2)
colnames(train_secondhand) = c("ampRotation_x","freqRotation_x","ampRotation_y","freqRotation_y","ampRotation_z","freqRotation_z","ampGravity_x","freqGravity_x","ampGravity_y","freqGravity_y","ampGravity_z","freqGravity_z","ampAcceleration_x","freqAcceleration_x","ampAcceleration_y","freqAcceleration_y","ampAcceleration_z","freqAcceleration_z")
train_secondhand$label <- 1


### Combine the firsthand and secondhand
dt_train <- abind(train_firsthand[c(-1:-30),],train_secondhand[c(-1:-30),],along = 1)
save(dt_train, file="../data/train_set/train.RData")


########################################
##### Construct Test Set ########
########################################

dt_test <- abind(train_firsthand[308:337,],train_secondhand[308:337,],along = 1)
save(dt_test, file="../data/test_set/test.RData")




dataframe_patient$timestamp <- seq(0,3600,length.out = length(dataframe_patient$timestamp))
fft_data <- fft(dataframe_patient$rotationRate_x)[1:as.integer(nrow(dataframe_patient)/2)]
fft_data <- mapply(abs,fft_data)
x <- seq(0, 15, length.out = as.integer(nrow(dataframe_patient)/2))
fft_data <- data.frame(x,fft_data)
sample_rate <- get_hertz(dataframe_patient)
# Plot the fft as a whole to get the frequency

# Transform the data by obtaining the frequency and amplitude and 10s/20s/.../120s interval

dataframe_patient_new <- dataframe_patient[7:15]
n_time <- seq(10,120,by = 10)
fft <- get_fft_freq_amp(dataframe_patient_new[,1],n_time,sample_rate)
total_fft<-fft[1]
for(i in 1:ncol(dataframe_patient_new)){
  fft <- get_fft_freq_amp(dataframe_patient_new[,i],n_time,sample_rate)
  colnames(fft) <- paste0(colnames(dataframe_patient_new)[i],'.',colnames(fft))
  total_fft <- cbind(total_fft,fft[-1])
}