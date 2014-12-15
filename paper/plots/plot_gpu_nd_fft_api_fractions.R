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
library(grid)

my_theme <-  theme_bw() + theme(axis.title.x = element_text(size=20),
                                axis.title.y = element_text(size=20),
                                axis.text.x = element_text(size=16),
                                axis.text.y = element_text(size=16)) + theme(legend.title = element_text(size=22, face="bold")) + theme(legend.text = element_text( size = 20)) + theme(legend.title=element_blank()) + theme(legend.position="bottom") + theme(axis.text.x  = element_text()) + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0., linetype='dashed')) + theme(legend.key.width = unit(1.5, "cm"))



gpu_nd_data$cuda_mem_perc <- gpu_nd_data$cudaFree_perc + gpu_nd_data$cudaMalloc_perc
gpu_nd_data$cuda_cpy_plus_mem <- gpu_nd_data$cuda_mem_perc + gpu_nd_data$cudaMemcpy_perc

data_api_asides_selected <- select(gpu_nd_data, gpu, alloc, tx, data_in_mb, cuda_cpy_plus_mem,cuda_mem_perc)

data_api_asides <- filter(data_api_asides_selected, !grepl("NVS",gpu))


api_fraction_incl_tx_incl_alloc <- filter(data_api_asides,
                                          grepl("incl_tx",tx) & grepl("incl_alloc",alloc ))
api_fraction_incl_tx_excl_alloc <- filter(data_api_asides,
                                          grepl("incl_tx",tx) & grepl("excl_alloc",alloc ))
api_fraction_excl_tx_excl_alloc <- filter(data_api_asides,
                                          grepl("excl_tx",tx) & grepl("excl_alloc",alloc ))



library(reshape2)
data_api_asides_melted <- melt(data_api_asides,
                               id.vars=c("data_in_mb","gpu","alloc","tx"))

levels(data_api_asides_melted$variable)[levels(data_api_asides_melted$variable)=="cuda_cpy_plus_mem"] <- "Memcpy+Malloc+Free"
levels(data_api_asides_melted$variable)[levels(data_api_asides_melted$variable)=="cuda_mem_perc"] <- "Malloc+Free"

api_data_incl_tx_incl_alloc <- filter(data_api_asides_melted,
                                          grepl("incl_tx",tx) & grepl("incl_alloc",alloc ))

api_fraction_incl_tx_incl_alloc <- ggplot(api_data_incl_tx_incl_alloc, aes(x=data_in_mb, y=value, colour=variable, linetype=gpu)) 
api_fraction_incl_tx_incl_alloc <- api_fraction_incl_tx_incl_alloc + geom_line(size=1.5) + my_theme #+ scale_y_log10()
api_fraction_incl_tx_incl_alloc <- api_fraction_incl_tx_incl_alloc + ylab("api runtime fraction / %") + xlab("input data / MB")
api_fraction_incl_tx_incl_alloc <- api_fraction_incl_tx_incl_alloc + ggtitle("R2C float32 FFT, cudaMemcpy & cudaMalloc incl.")
api_fraction_incl_tx_incl_alloc <- api_fraction_incl_tx_incl_alloc + scale_y_continuous(limits = c(0, 100))
ggsave(paste("api_fraction_cufft_r2c_incl_tx_incl_alloc_",append_to_output,".png",sep=""),api_fraction_incl_tx_incl_alloc)

api_data_incl_tx_excl_alloc <- filter(data_api_asides_melted,
                                          grepl("incl_tx",tx) & grepl("excl_alloc",alloc ))

api_fraction_incl_tx_excl_alloc <- ggplot(api_data_incl_tx_excl_alloc, aes(x=data_in_mb, y=value, colour=variable, linetype=gpu)) 
api_fraction_incl_tx_excl_alloc <- api_fraction_incl_tx_excl_alloc + geom_line(size=1.5) + my_theme #+ scale_y_log10()
api_fraction_incl_tx_excl_alloc <- api_fraction_incl_tx_excl_alloc + ylab("api runtime fraction / %") + xlab("input data / MB")
api_fraction_incl_tx_excl_alloc <- api_fraction_incl_tx_excl_alloc + ggtitle("R2C float32 FFT, cudaMemcpy incl.")
api_fraction_incl_tx_excl_alloc <- api_fraction_incl_tx_excl_alloc + scale_y_continuous(limits = c(0, 100))
ggsave(paste("api_fraction_cufft_r2c_incl_tx_excl_alloc_",append_to_output,".png",sep=""),api_fraction_incl_tx_excl_alloc)

api_data_excl_tx_excl_alloc <- filter(data_api_asides_melted,
                                          grepl("excl_tx",tx) & grepl("excl_alloc",alloc ))

api_fraction_excl_tx_excl_alloc <- ggplot(api_data_excl_tx_excl_alloc, aes(x=data_in_mb, y=value, colour=variable, linetype=gpu)) 
api_fraction_excl_tx_excl_alloc <- api_fraction_excl_tx_excl_alloc + geom_line(size=1.5) + my_theme #+ scale_y_log10()
api_fraction_excl_tx_excl_alloc <- api_fraction_excl_tx_excl_alloc + ylab("api runtime fraction / %") + xlab("input data / MB")
api_fraction_excl_tx_excl_alloc <- api_fraction_excl_tx_excl_alloc + ggtitle("R2C float32 FFT")
api_fraction_excl_tx_excl_alloc <- api_fraction_excl_tx_excl_alloc + scale_y_continuous(limits = c(0, 100))
ggsave(paste("api_fraction_cufft_r2c_excl_tx_excl_alloc_",append_to_output,".png",sep=""),api_fraction_excl_tx_excl_alloc)


