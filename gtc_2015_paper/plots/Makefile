
all : nd_fft_runtimes nd_fft_api_fractions

nd_fft_runtimes :
	./plot_gpu_nd_fft_runtimes.R ../data/bench_gpu_3d_fft/full.data
	./plot_gpu_nd_fft_api_fractions.R ../data/bench_gpu_3d_fft/full.data

%tex : %Rnw
	./knit $<

%pdf : %tex
	pdflatex $<
