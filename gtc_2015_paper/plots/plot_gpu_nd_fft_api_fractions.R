#!/usr/bin/env Rscript

argv = commandArgs(TRUE)

if(length(argv) < 1){
    stop("Usage: plot_gpu_nd_fft.R <data_file> ")
}

inputfile <- argv[1]
type_select <- "inplace"

if(length(argv) > 1){
type_select <- argv[2]
}

gpu_nd_data <- read.table(inputfile, header = T, sep = ' ') 


library(ggplot2)
library(dplyr)
library(grid)

my_theme <-  theme_bw() + theme(axis.title.x = element_text(size=20),
                                axis.title.y = element_text(size=20),
                                axis.text.x = element_text(size=16),
                                axis.text.y = element_text(size=16)) + theme(legend.title = element_text(size=22, face="bold")) + theme(legend.text = element_text( size = 20)) + theme(legend.title=element_blank()) + theme(legend.position="top") + theme(axis.text.x  = element_text()) + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0., linetype='dashed')) + theme(legend.key.width = unit(1.5, "cm"))



gpu_nd_data$cuda_mem_perc <- gpu_nd_data$cudaFree_perc + gpu_nd_data$cudaMalloc_perc
gpu_nd_data$cuda_cpy_plus_mem <- gpu_nd_data$cuda_mem_perc + gpu_nd_data$cudaMemcpy_perc

data_api_asides_selected <- select(gpu_nd_data, gpu, alloc, tx, trafo_type, data_in_mb, cuda_cpy_plus_mem,cuda_mem_perc)

data_api_asides <- filter(data_api_asides_selected, !grepl("NVS",gpu))


api_fraction_incl_tx_incl_alloc <- filter(data_api_asides,
                                          grepl("incl_tx",tx) & grepl("incl_alloc",alloc ) & grepl(type_select,trafo_type ) )
api_fraction_incl_tx_excl_alloc <- filter(data_api_asides,
                                          grepl("incl_tx",tx) & grepl("excl_alloc",alloc ) & grepl(type_select,trafo_type ) )
api_fraction_excl_tx_excl_alloc <- filter(data_api_asides,
                                          grepl("excl_tx",tx) & grepl("excl_alloc",alloc ) & grepl(type_select,trafo_type ) )

api_fraction_excl_tx_excl_alloc_any_type <- filter(data_api_asides,
                                          grepl("excl_tx",tx) & grepl("excl_alloc",alloc )  )


library(reshape2)
data_api_excl_tx_excl_alloc_melted <- melt(api_fraction_excl_tx_excl_alloc,
                               id.vars=c("data_in_mb","gpu","alloc","tx","trafo_type"),
                               measure.vars=c("cuda_cpy_plus_mem","cuda_mem_perc"))

levels(data_api_excl_tx_excl_alloc_melted$variable)[levels(data_api_excl_tx_excl_alloc_melted$variable)=="cuda_cpy_plus_mem"] <- "Memcpy+Malloc+Free"
levels(data_api_excl_tx_excl_alloc_melted$variable)[levels(data_api_excl_tx_excl_alloc_melted$variable)=="cuda_mem_perc"] <- "Malloc+Free"

data_api_incl_tx_excl_alloc_melted <- melt(api_fraction_incl_tx_excl_alloc,
                               id.vars=c("data_in_mb","gpu","alloc","tx","trafo_type"),
                               measure.vars=c("cuda_cpy_plus_mem","cuda_mem_perc"))

levels(data_api_incl_tx_excl_alloc_melted$variable)[levels(data_api_incl_tx_excl_alloc_melted$variable)=="cuda_cpy_plus_mem"] <- "Memcpy+Malloc+Free"
levels(data_api_incl_tx_excl_alloc_melted$variable)[levels(data_api_incl_tx_excl_alloc_melted$variable)=="cuda_mem_perc"] <- "Malloc+Free"

data_api_incl_tx_incl_alloc_melted <- melt(api_fraction_incl_tx_incl_alloc,
                               id.vars=c("data_in_mb","gpu","alloc","tx","trafo_type"),
                               measure.vars=c("cuda_cpy_plus_mem","cuda_mem_perc"))

levels(data_api_incl_tx_incl_alloc_melted$variable)[levels(data_api_incl_tx_incl_alloc_melted$variable)=="cuda_cpy_plus_mem"] <- "Memcpy+Malloc+Free"
levels(data_api_incl_tx_incl_alloc_melted$variable)[levels(data_api_incl_tx_incl_alloc_melted$variable)=="cuda_mem_perc"] <- "Malloc+Free"


api_fraction_incl_tx_incl_alloc <- ggplot(data_api_incl_tx_incl_alloc_melted, aes(x=data_in_mb, y=value, colour=variable, linetype=gpu)) 
api_fraction_incl_tx_incl_alloc <- api_fraction_incl_tx_incl_alloc + geom_line(size=1.5) + my_theme #+ scale_y_log10()
api_fraction_incl_tx_incl_alloc <- api_fraction_incl_tx_incl_alloc + ylab("api runtime fraction / %") + xlab("input data / MB")
api_fraction_incl_tx_incl_alloc <- api_fraction_incl_tx_incl_alloc + ggtitle(paste(type_select,"R2C float32 FFT, cudaMemcpy & cudaMalloc incl."))
api_fraction_incl_tx_incl_alloc <- api_fraction_incl_tx_incl_alloc + scale_y_continuous(limits = c(0, 100))
ggsave(paste("api_fraction_cufft_r2c_incl_tx_incl_alloc_",type_select,".png",sep=""),api_fraction_incl_tx_incl_alloc)


api_fraction_incl_tx_excl_alloc <- ggplot(data_api_incl_tx_excl_alloc_melted, aes(x=data_in_mb, y=value, colour=variable, linetype=gpu)) 
api_fraction_incl_tx_excl_alloc <- api_fraction_incl_tx_excl_alloc + geom_line(size=1.5) + my_theme #+ scale_y_log10()
api_fraction_incl_tx_excl_alloc <- api_fraction_incl_tx_excl_alloc + ylab("api runtime fraction / %") + xlab("input data / MB")
api_fraction_incl_tx_excl_alloc <- api_fraction_incl_tx_excl_alloc + ggtitle(paste(type_select,"R2C float32 FFT, cudaMemcpy incl."))
api_fraction_incl_tx_excl_alloc <- api_fraction_incl_tx_excl_alloc + scale_y_continuous(limits = c(0, 100))
ggsave(paste("api_fraction_cufft_r2c_incl_tx_excl_alloc_",type_select,".png",sep=""),api_fraction_incl_tx_excl_alloc)


p_api_fraction_excl_tx_excl_alloc <- ggplot(data_api_excl_tx_excl_alloc_melted, aes(x=data_in_mb, y=value, colour=variable, linetype=gpu)) 
p_api_fraction_excl_tx_excl_alloc <- p_api_fraction_excl_tx_excl_alloc + geom_line(size=1.5) + my_theme #+ scale_y_log10()
p_api_fraction_excl_tx_excl_alloc <- p_api_fraction_excl_tx_excl_alloc + ylab("api runtime fraction / %") + xlab("input data / MB")
p_api_fraction_excl_tx_excl_alloc <- p_api_fraction_excl_tx_excl_alloc + ggtitle(paste(type_select,"R2C float32 FFT"))
p_api_fraction_excl_tx_excl_alloc <- p_api_fraction_excl_tx_excl_alloc + scale_y_continuous(limits = c(0, 100))
ggsave(paste("api_fraction_cufft_r2c_excl_tx_excl_alloc_",type_select,".png",sep=""),p_api_fraction_excl_tx_excl_alloc)


p_api_fraction_excl_tx_excl_alloc_gmem_ops <- ggplot(api_fraction_excl_tx_excl_alloc_any_type, aes(x=data_in_mb, y=cuda_mem_perc, colour=gpu, linetype=trafo_type)) 
p_api_fraction_excl_tx_excl_alloc_gmem_ops <- p_api_fraction_excl_tx_excl_alloc_gmem_ops + geom_line(size=1.5) + my_theme #+ scale_y_log10()
p_api_fraction_excl_tx_excl_alloc_gmem_ops <- p_api_fraction_excl_tx_excl_alloc_gmem_ops + ylab("api runtime fraction / %") + xlab("input data / MB")
p_api_fraction_excl_tx_excl_alloc_gmem_ops <- p_api_fraction_excl_tx_excl_alloc_gmem_ops + ggtitle("R2C float32 FFT cudaMalloc+cudaFree")
p_api_fraction_excl_tx_excl_alloc_gmem_ops <- p_api_fraction_excl_tx_excl_alloc_gmem_ops + scale_y_continuous(limits = c(0, 100))
ggsave("api_fraction_cufft_r2c_excl_tx_excl_alloc_gmem_ops.png",p_api_fraction_excl_tx_excl_alloc_gmem_ops)


synced_gpu_api_fraction <- ggplot(data_api_incl_tx_incl_alloc_melted, aes(x=data_in_mb, y=value, colour=gpu, linetype=variable)) 
synced_gpu_api_fraction <- synced_gpu_api_fraction + geom_line(size=1.5) + my_theme #+ scale_y_log10()
synced_gpu_api_fraction <- synced_gpu_api_fraction + ylab("api runtime fraction / %") + xlab("input data / MB")
synced_gpu_api_fraction <- synced_gpu_api_fraction
synced_gpu_api_fraction <- synced_gpu_api_fraction + scale_y_continuous(limits = c(0, 100))
ggsave(paste("synced_gpu_api_fraction",type_select,".png",sep=""),synced_gpu_api_fraction)
ggsave(paste("synced_gpu_api_fraction",type_select,".svg",sep=""),synced_gpu_api_fraction)
