is_around_hertz <- function(df, hertz=20){
  first_frames <- head(df, 100)
  total_frame_count <- 0
  t0 <- as.numeric(first_frames$timestamp[1])
  t1 <- as.numeric(first_frames$timestamp[100])
  total_frame_count <- t1-t0
  rate <- 100 / total_frame_count
  hertz == as.integer(rate+0.5)
}

get_hertz <- function(df){
  first_frames <- head(df, 100)
  total_frame_count <- 0
  t0 <- as.numeric(first_frames$timestamp[1])
  t1 <- as.numeric(first_frames$timestamp[100])
  total_frame_count <- t1-t0
  100 / total_frame_count
}

get_max_inside_bounds <- function(fft, rate, low, high){
  low_index <- as.integer(length(fft) * (low/rate))
  high_index <- as.integer(length(fft) * (high/rate))
  import_sub <- c()
  if(low_index+1 > high_index){
    j <- low_index+1
    import_sub <- append(import_sub, fft[j]/length(fft))
  }
  else{
    for (k in seq(low_index+1, high_index, 1)){
      import_sub <- append(import_sub, fft[k]/length(fft))
    }
  }
  sum(import_sub)
}

get_fft_as_set_of_arrays_over_n_samples <- function(dataframe, frequency, FFT_N){
  if (length(dataframe) < FFT_N){
    return(c())
  }
  else{
    all_bins_summarized <- list()
    for (start_index in seq(1, length(dataframe), FFT_N)){
      if(start_index+FFT_N-1<=length(dataframe)){
        end <- start_index+FFT_N-1
      }
      else{
        end <- length(dataframe)
      }
      fft_data <- dataframe[start_index:end]
      fft_output <- fft(fft_data)
      frequency_bins <- list(mapply(abs, fft_output))
      all_bins_summarized[[start_index]] <- frequency_bins[[1]][1:as.integer(length(frequency_bins[[1]])/2)]
    }
    non_null_names <- which(!sapply(all_bins_summarized, is.null))
    all_bins_summarized <- all_bins_summarized[non_null_names]
    total_time <- length(dataframe)/frequency
    time_per_interval <- FFT_N/frequency
    times <- seq(0, total_time, length.out = as.integer(length(dataframe)/FFT_N)+1)
    names(all_bins_summarized) <- times
    return(all_bins_summarized)
  }
}

get_max_amplitude <- function(dataframe,lower){
  n <- length(dataframe)
  percent <- lower/15
  order <- as.integer(percent*n)
  new_dataframe <- dataframe[order:n]
  return(max(new_dataframe))
}
get_max_frequency <- function(dataframe,amplitude){
  return(which(dataframe %in% amplitude)/length(dataframe)*15)
}

get_fft_freq_amp <- function(dataframe, n_time,sample_rate){
  time_name <- paste0("data_set_by_", n_time,"s")
  data_list <- map(n_time*32, 
                   get_fft_as_set_of_arrays_over_n_samples,
                   dataframe = dataframe,
                   frequency = sample_rate)
  names(data_list) <- time_name
  
  amplitude <- list()
  
  for(i in 1:length(data_list)){
    amplitude[[i]] <- lapply(data_list[[i]], get_max_amplitude, lower = 2)
  }
  
  names(amplitude) <- time_name
  
  frequency <- list()
  
  for(i in 1:length(data_list)){
    frequency[[i]] <- lapply(data_list[[i]],
                             get_max_frequency, 
                             amplitude = unlist(amplitude[[i]]))
  }
  
  names(frequency) <- time_name
  
  for(i in 1:length(data_list)){
    frequency[[i]] <- unlist(frequency[[i]])
    amplitude[[i]] <- unlist(amplitude[[i]])
  }
  
  freq_amp <- data.frame(unlist(frequency),unlist(amplitude))
  row <- gsub("\\..*$", "", rownames(freq_amp))
  fft <- data.frame(row,freq_amp)
  colnames(fft) <- c("n_time","frequency","amplitude")
  rownames(fft) <- c()
  return(fft)
}


#### clean data ####
# df must be the dataframe with fft transformation containing freq & amplitude
get_amp_freq <- function(df){
  amp = max(df$amplitude[which(df$frequency > 2)])
  freq = df$frequency[which(df$amplitude == max(df$amplitude[which(df$frequency > 2)]))]
  return(c(amp,freq))
}



