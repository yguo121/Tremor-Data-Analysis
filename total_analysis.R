source_dir <- "/Users/Tim/Desktop/Columbia/STAT/STAT 5291/Project/"
setwd(source_dir)
source("hertz_dataframe_tools.R")
library(ggplot2)

# Put you own directory here
base_dir <- "/Users/Tim/Desktop/Columbia/STAT/STAT 5291/Project/DATA/"
# Type of hand
hand_dirs <- "FIRSTHAND" 
hand_full_dirs <- paste(base_dir, hand_dirs, sep = "")
setwd(hand_full_dirs)
# Set the number of file
csv <- list.files()[35]

dataframe_patient <- read.csv(csv)
dataframe_patient$timestamp <- seq(0,3600,length.out = length(dataframe_patient$timestamp))
fft_data <- fft(dataframe_patient$rotationRate_x)[1:as.integer(nrow(dataframe_patient)/2)]
fft_data <- mapply(abs,fft_data)
x <- seq(0, 15, length.out = as.integer(nrow(dataframe_patient)/2))
fft_data <- data.frame(x,fft_data)
# Plot the fft as a whole to get the frequency

ggplot(fft_data) + geom_line(aes(x=x, y=fft_data, color="red"))

ffts_over_interval_1s <- get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 32)
ffts_over_interval_1m <- get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 1024)
ffts_over_interval_5m <- get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 8192)
ffts_over_interval_10m <- get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 8192*2)
ffts_over_interval_20m <- get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 8192*4)

x1s <- lapply(ffts_over_interval_1s, get_max_inside_bounds, rate=sample_rate, low=3, high=6)
x1m <- lapply(ffts_over_interval_1m, get_max_inside_bounds, rate=sample_rate, low=3, high=6)
x5m <- lapply(ffts_over_interval_5m, get_max_inside_bounds, rate=sample_rate, low=3, high=6)
x10m <- lapply(ffts_over_interval_10m, get_max_inside_bounds, rate=sample_rate, low=3, high=6)
x20m <- lapply(ffts_over_interval_20m, get_max_inside_bounds, rate=sample_rate, low=3, high=6)

# Plot the frequency obtained by setting different time interval
ggplot() + geom_point(aes(x=as.numeric(names(x1s)), y=unlist(x1s), color="1s", size=1)) + 
 geom_point(aes(x=as.numeric(names(x1m)), y=unlist(x1m), color="1m", size=2)) + 
 geom_point(aes(x=as.numeric(names(x5m)), y=unlist(x5m), color="5m", size=3)) + 
 geom_point(aes(x=as.numeric(names(x10m)), y=unlist(x10m), color="10m", size=4)) + 
 geom_point(aes(x=as.numeric(names(x20m)), y=unlist(x20m), color="20m", size=5))


# PCA on the main factors

R <- cor(dataframe_patient[4:(ncol(dataframe_patient)-1)])
G.R <- eigen(R)$vectors; L.R <- diag(eigen(R)$values)
round(diag(L.R) / sum(diag(L.R)), 4)
plot(diag(L.R), type="b", main="Scree plot for Track_Records data")
plot(cumsum(diag(L.R))/sum(diag(L.R)), type="b", xlab="k", ylab="",main="Proportion of total variance explained by first k NPCs")
G.R[,1:7]

# Plot of various parameters

ggplot(dataframe_patient[0:60*20,]) + geom_line(aes(x=timestamp,y=userAcceleration_x,col = 'X')) + 
  geom_line(aes(x=timestamp,y=userAcceleration_y,col = "Y")) + 
  geom_line(aes(x=timestamp,y=userAcceleration_z,col = 'Z')) +
  labs(x="time(s)",y="userAcceleration")

ggplot(dataframe_patient[0:60*20,]) + geom_line(aes(x=timestamp,y=rotationRate_x,col = 'X')) + 
  geom_line(aes(x=timestamp,y=rotationRate_y,col = "Y")) + 
  geom_line(aes(x=timestamp,y=rotationRate_z,col = 'Z')) +
  labs(x="time(s)",y="rotationRate")

ggplot(dataframe_patient[0:60*20,]) + geom_line(aes(x=timestamp,y=gravity_x,col = 'X')) + 
  geom_line(aes(x=timestamp,y=gravity_y,col = "Y")) + 
  geom_line(aes(x=timestamp,y=gravity_z,col = 'Z')) +
  labs(x="time(s)",y="gravity")

ggplot(dataframe_patient[0:60*120,]) + geom_line(aes(x=timestamp,y=attitude_roll,col = 'attitude_roll')) + 
  geom_line(aes(x=timestamp,y=attitude_pitch,col = "attitude_pitch")) + 
  geom_line(aes(x=timestamp,y=attitude_yaw,col = 'attitude_yaw')) +
  labs(x="time(s)",y="gravity")

dev.off()

par(mfrow = c(1,3))

# Durbin Watson test on rotationRate

reg_rotation_x <- lm(rotationRate_x ~ timestamp, data = dataframe_patient)
reg_rotation_y <- lm(rotationRate_y ~ timestamp, data = dataframe_patient)
reg_rotation_z <- lm(rotationRate_z ~ timestamp, data = dataframe_patient)

library(car)
durbinWatsonTest(reg_rotation_x)
durbinWatsonTest(reg_rotation_y)
durbinWatsonTest(reg_rotation_z)

#PACF on rotationRate

rotation_x <- pacf(dataframe_patient$rotationRate_x)
rotation_y <- pacf(dataframe_patient$rotationRate_y)
rotation_z <- pacf(dataframe_patient$rotationRate_z)

plot(rotation_x, main = "rotationRate_x PACF")
plot(rotation_y, main = "rotationRate_y PACF")
plot(rotation_z, main = "rotationRate_z PACF")

