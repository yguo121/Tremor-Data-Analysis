import numpy as np

def is_around_hertz(df, hertz=20):
	try:
		first_frames = df.head(100)
		total_frame_count = 0
		t0 = float(first_frames["timestamp"][0])
		t1 = float(first_frames["timestamp"][99])
		total_frame_count = t1-t0
		rate = 100 / total_frame_count
		return hertz == int(rate+0.5)
	except:
		return False

def get_fft_as_set_of_arrays_over_n_samples(dataframe, frequency, FFT_N):
	if len(dataframe) < FFT_N:
		return []

	all_bins_summarized = []
	for start_index in range(0, len(dataframe), FFT_N):

		fft_data = dataframe["rotationRate_x"][start_index:start_index+FFT_N]
		fft_output = np.fft.fft(fft_data)
		frequency_bins = list(map(np.absolute, fft_output))
		all_bins_summarized.append(frequency_bins[0:len(frequency_bins)//2])

	total_time = len(dataframe)/frequency
	time_per_interval = FFT_N/frequency
	times = np.linspace(0,total_time, int(len(dataframe)/FFT_N)+1)
	return times, all_bins_summarized


def get_hertz(df):
	try:
		first_frames = df.head(100)
		total_frame_count = 0
		t0 = float(first_frames["timestamp"][0])
		t1 = float(first_frames["timestamp"][99])
		total_frame_count = t1-t0
		rate = 100 / total_frame_count
		return rate
	except:
		return 20.0


def get_max_inside_bounds(fft, rate, low, high):
	low_index = int(len(fft) * (low/rate))
	high_index = int(len(fft) * (high/rate))
	import_sub = []
	for k in range(low_index, high_index, 1):
		import_sub.append(fft[k]/len(fft))
	# print("sum",sum())

	return sum(import_sub)