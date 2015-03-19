


plots : dev #nd_fft_api_fractions

backup :
	./plot_gpu_nd_fft_runtimes.R ../data/bench_gpu_3d_fft/full.data
	./plot_gpu_nd_fft_api_fractions.R ../data/bench_gpu_3d_fft/full.data

dev :
	./single_fft_runtime.R ../data/*_[cg]pu.data
	./single_fft_api_fraction.R ../data/*_gpu_prof.data
	./batched_fft_runtime.R ../data/*many_fft.data
	./batched_folds_speed_up.R ../data/islay*many_folds.data ../data/pigino-pc-4_gpu*folds.data
	./deconvolve_synthetic_speed_up.R ../data/islay_*synthetic.data ../data/pigino-pc-4_*synthetic.data

%tex : %Rnw
	./knit $<

%pdf : %tex
	pdflatex $<
