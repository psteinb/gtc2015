#!/usr/bin/env Rscript

argv = commandArgs(TRUE)

if(length(argv) < 1){
    stop("Usage: single_fft_runtime.R <data_file> ")
}

write(argv)
inputfiles <- argv

file_reader <- function(fn) {
  read.table(fn, header = T, sep = ' ', stringsAsFactors = FALSE)
  
}

all_data <- do.call("rbind", lapply(inputfiles, file_reader))

library(ggplot2)
library(dplyr)
library(grid)

all_data$data_in_mb <- (as.numeric(all_data$stack_dims_x)*as.numeric(all_data$stack_dims_y)*as.numeric(all_data$stack_dims_z))*4/(1024*1024)
all_data$shape <- sprintf("%sx%sx%s",all_data$stack_dims_x,all_data$stack_dims_y,all_data$stack_dims_z)
all_data$dev_name[all_data$dev_name == 'GeForce_GTX_TITAN_Black'] <- 'Titan_Black'

my_theme <-  theme_bw() + theme(axis.title.x = element_text(size=20),
                                axis.title.y = element_text(size=20),
                                axis.text.x = element_text(size=16),
                                axis.text.y = element_text(size=16)) + theme(legend.title = element_text(size=22, face="bold")) + theme(legend.text = element_text( size = 16)) + theme(legend.title=element_blank()) + theme(legend.position="top") + theme(axis.text.x  = element_text()) + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0., linetype='dashed')) + theme(legend.key.width = unit(1.5, "cm"))


gpu_only <- filter(all_data, grepl("gpu",dev_type))
gpu_only <- filter(gpu_only, !grepl("NVS",dev_name))

gpu_only$cuda_mem_perc <- gpu_only$cudaFree_perc + gpu_only$cudaMalloc_perc
gpu_only$cuda_cpy_plus_mem <- gpu_only$cuda_mem_perc + gpu_only$cudaMemcpy_perc

library(reshape2)
gpu_only_melted <- melt(filter(gpu_only,grepl("incl_tx",comment), grepl("incl_alloc",comment )),
                        id.vars=c("data_in_mb","dev_name","comment"),
                        measure.vars=c("cuda_cpy_plus_mem","cuda_mem_perc"))

levels(gpu_only_melted$variable)[levels(gpu_only_melted$variable)=="cuda_cpy_plus_mem"] <- "Memcpy+Malloc+Free"
levels(gpu_only_melted$variable)[levels(gpu_only_melted$variable)=="cuda_mem_perc"] <- "Malloc+Free"

head(gpu_only_melted)

synced_gpu_api_fraction <- ggplot(gpu_only_melted, aes(x=data_in_mb, y=value, colour=dev_name, linetype=variable)) 
synced_gpu_api_fraction <- synced_gpu_api_fraction + geom_line(size=1.5) + my_theme #+ scale_y_log10()
synced_gpu_api_fraction <- synced_gpu_api_fraction + ylab("api runtime fraction / %") + xlab("input data / MB")
synced_gpu_api_fraction <- synced_gpu_api_fraction + scale_y_continuous(limits = c(0, 100))

ggsave("synced_gpu_api_fraction.png",synced_gpu_api_fraction)
ggsave("synced_gpu_api_fraction.pdf",synced_gpu_api_fraction)
ggsave("synced_gpu_api_fraction.svg",synced_gpu_api_fraction)
