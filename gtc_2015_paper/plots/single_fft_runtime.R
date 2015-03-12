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


my_theme <-  theme_bw() + theme(axis.title.x = element_text(size=20),
                                axis.title.y = element_text(size=20),
                                axis.text.x = element_text(size=16),
                                axis.text.y = element_text(size=16)) + theme(legend.title = element_text(size=22, face="bold")) + theme(legend.text = element_text( size = 20)) + theme(legend.title=element_blank()) + theme(legend.position="top") + theme(axis.text.x  = element_text()) + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0., linetype='dashed')) + theme(legend.key.width = unit(1.5, "cm"))

cpu_only <- filter(all_data, grepl("cpu",dev_type) , n_devices!=1, grepl("MEASURE",comment), grepl("inplace",comment) )


gpu_only <- filter(all_data, grepl("gpu",dev_type))

cpu_only

data_to_plot <- cpu_only
gpu_of_interest <- filter(gpu_only, grepl("incl_tx",comment) , grepl("incl_alloc",comment ) , grepl("inplace",comment))
data_to_plot <- rbind(data_to_plot, gpu_of_interest)

data_to_plot$dev_name[data_to_plot$dev_name == '12xE5-26400'] <- '12xE5-2640'



runtime_gpu <- ggplot(data_to_plot, aes(x=data_in_mb, y=as.numeric(total_time_ms), color=as.factor(dev_name))) 
runtime_gpu <- runtime_gpu + geom_line(size=1.5) + my_theme #+ scale_y_log10()
runtime_gpu <- runtime_gpu + ylab("runtime / ms") + xlab("input data / MB")+ guides(color=guide_legend(nrow=2,byrow=TRUE)) #+ xlim(0,1024)

ggsave("synced_gpu_runtime.png",runtime_gpu)
ggsave("synced_gpu_runtime.svg",runtime_gpu)
ggsave("synced_gpu_runtime.pdf",runtime_gpu)

## reference <- filter(all_data, !grepl("v3",dev_name), grepl("cpu",dev_type) , n_devices==1)

## cpu_shapes <- levels(as.factor(cpu_only$shape))
## cpu_names <- levels(as.factor(cpu_only$dev_name))


## cpu_ref_data <- do.call(rbind, replicate(length(cpu_names), as.matrix(reference), simplify=FALSE))
## sprintf("cpu reference to_plot %i, ref %i",nrow(cpu_only),nrow(cpu_ref_data))



## gpu_shapes <- levels(as.factor(gpu_of_interest$shape))
## gpu_names <- levels(as.factor(gpu_of_interest$dev_name))

## gpu_reference <- filter(reference, shape %in% c(gpu_shapes))


## gpu_ref_data <- do.call(rbind, replicate(length(gpu_names), as.matrix(gpu_reference), simplify=FALSE))
## sprintf("gpu reference to_plot %i, ref %i",nrow(gpu_of_interest),nrow(gpu_ref_data))
## sprintf("to_plot %i",nrow(data_to_plot))

## gpu_of_interest

## all_reference <- rbind(cpu_ref_data,gpu_ref_data)



