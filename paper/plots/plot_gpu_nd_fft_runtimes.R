#!/usr/bin/env Rscript

argv = commandArgs(TRUE)

if(length(argv) < 1){
    stop("Usage: plot_gpu_nd_fft.R <data_file> ")
}

inputfile <- argv[1]
append_to_output <- ""

if(length(argv) > 1){
append_to_output <- argv[2]
}


gpu_nd_data <- read.table(inputfile, header = T, sep = ' ') 


head(gpu_nd_data)

library(ggplot2)
library(dplyr)


my_theme <-  theme_bw() + theme(axis.title.x = element_text(size=20),
                                axis.title.y = element_text(size=20),
                                axis.text.x = element_text(size=16),
                                axis.text.y = element_text(size=16)) + theme(legend.title = element_text(size=22, face="bold")) + theme(legend.text = element_text( size = 20)) + theme(legend.title=element_blank()) + theme(legend.position="top") + theme(axis.text.x  = element_text()) + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0., linetype='dashed'))

data_incl_tx_incl_alloc <- filter(gpu_nd_data, grepl("incl_tx",tx) & grepl("incl_alloc",alloc ))
data_incl_tx_excl_alloc <- filter(gpu_nd_data, grepl("incl_tx",tx) & grepl("excl_alloc",alloc ))
data_excl_tx_excl_alloc <- filter(gpu_nd_data, grepl("excl_tx",tx) & grepl("excl_alloc",alloc ))


runtime_incl_tx_incl_alloc <- ggplot(data_incl_tx_incl_alloc, aes(x=data_in_mb, y=total_time_ms, color=gpu)) 
runtime_incl_tx_incl_alloc <- runtime_incl_tx_incl_alloc + geom_line(size=1.5) + my_theme + scale_y_log10()
runtime_incl_tx_incl_alloc <- runtime_incl_tx_incl_alloc + ylab("runtime / ms") + xlab("input data / MB")
runtime_incl_tx_incl_alloc <- runtime_incl_tx_incl_alloc + ggtitle("R2C float32 FFT, cudaMemcpy & cudaMalloc incl.")
ggsave(paste("gpu_cufft_r2c_incl_tx_incl_alloc_",append_to_output,".png",sep=""),runtime_incl_tx_incl_alloc)

head(data_incl_tx_excl_alloc)
runtime_incl_tx_excl_alloc <- ggplot(data_incl_tx_excl_alloc, aes(x=data_in_mb, y=total_time_ms, color=gpu)) 
runtime_incl_tx_excl_alloc <- runtime_incl_tx_excl_alloc + geom_line(size=1.5) + my_theme + scale_y_log10()
runtime_incl_tx_excl_alloc <- runtime_incl_tx_excl_alloc + ylab("runtime / ms") + xlab("input data / MB")
runtime_incl_tx_excl_alloc <- runtime_incl_tx_excl_alloc + ggtitle("R2C float32 FFT, cudaMemcpy incl")
ggsave(paste("gpu_cufft_r2c_incl_tx_excl_alloc_",append_to_output,".png",sep=""),runtime_incl_tx_excl_alloc)


runtime_excl_tx_excl_alloc <- ggplot(data_excl_tx_excl_alloc, aes(x=data_in_mb, y=total_time_ms, color=gpu)) 
runtime_excl_tx_excl_alloc <- runtime_excl_tx_excl_alloc + geom_line(size=1.5) + my_theme + scale_y_log10()
runtime_excl_tx_excl_alloc <- runtime_excl_tx_excl_alloc + ylab("runtime / ms") + xlab("input data / MB")
runtime_excl_tx_excl_alloc <- runtime_excl_tx_excl_alloc + ggtitle("R2C float32 FFT")
ggsave(paste("gpu_cufft_r2c_excl_tx_excl_alloc_",append_to_output,".png",sep=""),runtime_excl_tx_excl_alloc)




data_api_asides <- select(gpu_nd_data, gpu, data_in_mb, cudaFree_perc,cudaMemcpy_perc,cudaMalloc_perc)
head(data_api_asides)
