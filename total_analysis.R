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
csv <- list.files()[1]
df <- read.csv(csv)

dataframe_patient <- read.csv(csv)
sample_rate <- get_hertz(dataframe_patient)
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
