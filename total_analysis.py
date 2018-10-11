import pandas as pd
import os
import matplotlib.pyplot as plt
from hertz_dataframe_tools import get_hertz, get_fft_as_set_of_arrays_over_n_samples, get_max_inside_bounds
import numpy as np


base_dir = "./DATA/"
hand_dirs = ["FIRSTHAND",] #"SECONDHAND"]
hand_full_dirs = map(lambda x: base_dir+x, hand_dirs)

def add_plot(x, sample_rate, fft_arr, shape, subplot):
	y = list(map(lambda fft: get_max_inside_bounds(fft, sample_rate, 3,6), fft_arr))
	subplot.plot(x[:len(y)], y, shape)

fig, axs = plt.subplots(1, 2, constrained_layout=True)

for hand_dir_name, hand_full_dir in zip(hand_dirs, hand_full_dirs):
	print(hand_dir_name)
	for index, csv in enumerate(os.listdir(hand_full_dir)[1:2]):
		print(csv)
		dataframe_patient = pd.read_csv(base_dir+hand_dir_name+"/"+csv)
		sample_rate = get_hertz(dataframe_patient)
		fft_data = np.fft.fft(dataframe_patient["rotationRate_x"])[:len(dataframe_patient)//2]
		print(fft_data)
		axs[0].plot(np.linspace(0, 15, len(dataframe_patient)//2), list(map(lambda x: np.abs(x),fft_data)), "ro")

		x1s, ffts_over_interval_1s = get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 32) # seconds scale
		x1m, ffts_over_interval_1m = get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 1024) # mins scale
		x5m, ffts_over_interval_5m = get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 8192) # 5 mins scale
		x10m, ffts_over_interval_10m = get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 8192*2) # 5 mins scale
		x20m, ffts_over_interval_20m = get_fft_as_set_of_arrays_over_n_samples(dataframe_patient, sample_rate, 8192*4) # 5 mins scale



		# add_plot(x1s, sample_rate, ffts_over_interval_1s, 'yo', axs[0])
		# add_plot(x1m, sample_rate, ffts_over_interval_1m, 'go', axs[0])
		# add_plot(x5m, sample_rate, ffts_over_interval_5m, 'ro', axs[0])
		# add_plot(x10m, sample_rate, ffts_over_interval_10m, 'bo', axs[0])
		# add_plot(x20m, sample_rate, ffts_over_interval_20m, 'bx', axs[0])

		# sample_rate = get_hertz(dataframe_cabral)
		# x1s, ffts_over_interval_1s = get_fft_as_set_of_arrays_over_n_samples(dataframe_cabral, sample_rate, 32) # seconds scale
		# x1m, ffts_over_interval_1m = get_fft_as_set_of_arrays_over_n_samples(dataframe_cabral, sample_rate, 1024) # mins scale
		# x5m, ffts_over_interval_5m = get_fft_as_set_of_arrays_over_n_samples(dataframe_cabral, sample_rate, 8192) # 5 mins scale
		# x10m, ffts_over_interval_10m = get_fft_as_set_of_arrays_over_n_samples(dataframe_cabral, sample_rate, 8192*2) # 5 mins scale
		# x20m, ffts_over_interval_20m = get_fft_as_set_of_arrays_over_n_samples(dataframe_cabral, sample_rate, 8192*4) # 5 mins scale
		# add_plot(x1s, sample_rate, ffts_over_interval_1s, 'yo', axs[1])
		# add_plot(x1m, sample_rate, ffts_over_interval_1m, 'go', axs[1])
		# add_plot(x5m, sample_rate, ffts_over_interval_5m, 'ro', axs[1])
		# add_plot(x10m, sample_rate, ffts_over_interval_10m, 'bo', axs[1])
		# add_plot(x20m, sample_rate, ffts_over_interval_20m, 'bx', axs[1])
plt.show()
