#!/usr/bin/env Rscript

argv = commandArgs(TRUE)

if(length(argv) < 1){
    stop("Usage: batched_folds_runtime.R <data_file> ")
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

# multiply by 8 as this is the default number of stacks in the benchmark (TODO: need to update bench util)
all_data$data_in_mb <- 2*8*(as.numeric(all_data$stack_dims_x)*as.numeric(all_data$stack_dims_y)*as.numeric(all_data$stack_dims_z))*4/(1024*1024)
all_data$shape <- sprintf("%sx%sx%s",all_data$stack_dims_x,all_data$stack_dims_y,all_data$stack_dims_z)


my_theme <-  theme_bw() + theme(axis.title.x = element_text(size=20),
                                axis.title.y = element_text(size=20),
                                axis.text.x = element_text(size=16),
                                axis.text.y = element_text(size=16)) + theme(legend.title = element_text(size=22, face="bold")) + theme(legend.text = element_text( size = 16)) + theme(legend.title=element_blank()) + theme(legend.position="top") + theme(axis.text.x  = element_text()) + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0., linetype='dashed')) + theme(legend.key.width = unit(1.5, "cm"))

cpu_only <- filter(all_data, grepl("cpu",dev_type) , n_devices!=1 )
cpu_only$is_cpu <- "cpu"
cpu_only$mode <- 'interleaved'

gpu_only <- filter(all_data, grepl("gpu",dev_type))
gpu_only$is_cpu <- "gpu"

gpu_only$mode[ grepl('plan_many',gpu_only$comment)] <- 'plan_many'
gpu_only$mode[ !grepl('plan_many',gpu_only$comment) ] <- 'interleaved'

data_to_plot <- cpu_only
data_to_plot <- rbind(data_to_plot, gpu_only)

data_to_plot$dev_name[data_to_plot$dev_name == '12xE5-26400'] <- '12xE5-2640'
data_to_plot$dev_name[data_to_plot$dev_name == 'GeForce_GTX_TITAN_Black'] <- 'Titan_Black'


runtime_gpu <- ggplot(data_to_plot, aes(x=data_in_mb, y=as.numeric(total_time_ms), color=as.factor(mode),linetype=as.factor(dev_name))) 
runtime_gpu <- runtime_gpu + geom_line(size=1.5) + my_theme + guides(linetype=FALSE) + scale_y_log10()
runtime_gpu <- runtime_gpu + ylab("runtime / ms") + xlab("input data / MB")+ guides(color=guide_legend(nrow=2,byrow=TRUE)) 

ggsave("batched_folds_gpu_runtime.png",runtime_gpu)
ggsave("batched_folds_gpu_runtime.svg",runtime_gpu)
ggsave("batched_folds_gpu_runtime.pdf",runtime_gpu)



