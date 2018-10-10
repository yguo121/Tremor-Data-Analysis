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
  if (nrow(dataframe) < FFT_N){
    return(c())
  }
  else{
    all_bins_summarized <- list()
    for (start_index in seq(1, nrow(dataframe), FFT_N)){
      if(start_index+FFT_N-1<=nrow(dataframe)){
        end <- start_index+FFT_N-1
      }
      else{
        end <- nrow(dataframe)
      }
      fft_data <- dataframe$rotationRate_x[start_index:end]
      fft_output <- fft(fft_data)
      frequency_bins <- list(mapply(abs, fft_output))
      all_bins_summarized[[start_index]] <- frequency_bins[[1]][1:as.integer(length(frequency_bins[[1]])/2)]
    }
    non_null_names <- which(!sapply(all_bins_summarized, is.null))
    all_bins_summarized <- all_bins_summarized[non_null_names]
    total_time <- nrow(dataframe)/frequency
    time_per_interval <- FFT_N/frequency
    times <- seq(0, total_time, length.out = as.integer(nrow(dataframe)/FFT_N)+1)
    names(all_bins_summarized) <- times
    return(all_bins_summarized)
  }
}
