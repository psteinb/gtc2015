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
                                axis.text.y = element_text(size=16)) + theme(legend.title = element_text(size=22, face="bold")) + theme(legend.text = element_text( size = 16)) + theme(legend.title=element_blank()) + theme(legend.position="top") + theme(axis.text.x  = element_text()) + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0., linetype='dashed')) + theme(legend.key.width = unit(1.5, "cm"))

cpu_only <- filter(all_data, grepl("cpu",dev_type) , n_devices!=1, grepl("v3",dev_name))
cpu_only$tx_type <- "sync"

all_data <- mutate(all_data, tx_type = gsub('([a-z]+),.*','\\1',comment))

gpu_only <- filter(all_data, grepl("gpu",dev_type), grepl("global_plan",comment), grepl("OK",comment), grepl("inplace",comment), "mangd" != tx_type, tx_type != "async")
gpu_only$dev_name[gpu_only$dev_name == 'GeForce_GTX_TITAN_Black'] <- 'Titan_Black'

data_to_plot <- cpu_only
data_to_plot <- rbind(data_to_plot, gpu_only)


runtime_gpu <- ggplot(filter(data_to_plot, !grepl("2090", dev_name)), aes(x=data_in_mb, y=as.numeric(total_time_ms), color=tx_type )) 
runtime_gpu <- runtime_gpu + geom_line(aes(linetype=as.factor(dev_name)),size=1.5) + my_theme + scale_linetype_manual(values=c(5,3,1,8,10))
runtime_gpu <- runtime_gpu + ylab("runtime / ms") + xlab("input data / MB")+ guides(linetype=guide_legend(nrow=2)) + xlim(0,1024) + ylim(100,.9*max(c(data_to_plot$total_time_ms))) + scale_y_log10()

ggsave("batched_all_cgpu_runtime.png",runtime_gpu)
ggsave("batched_all_cgpu_runtime.svg",runtime_gpu)
ggsave("batched_all_cgpu_runtime.pdf",runtime_gpu)

data_to_plot <- rbind(filter(gpu_only, grepl("K20", dev_name) | grepl("Titan", dev_name) ),filter(cpu_only, n_devices<40))

runtime_gpu <- ggplot(data_to_plot, aes(x=data_in_mb, y=as.numeric(total_time_ms), color=as.factor(dev_name), linetype=tx_type )) 
runtime_gpu <- runtime_gpu + geom_line(size=1.5) + my_theme  +scale_color_brewer(palette="Set1") 
runtime_gpu <- runtime_gpu + scale_linetype_manual(values=c("solid","dashed","dotted"))
runtime_gpu <- runtime_gpu + guides(color=guide_legend(ncol=1)) + theme(legend.position="top")
runtime_gpu <- runtime_gpu + ylab("runtime / ms") + xlab("input data / MB")+ guides(linetype=guide_legend(nrow=1)) + xlim(0,1024) + ylim(100,.9*max(c(data_to_plot$total_time_ms))) + scale_y_log10()

ggsave("batched_cgpu_runtime.png",runtime_gpu)
ggsave("batched_cgpu_runtime.svg",runtime_gpu)
ggsave("batched_cgpu_runtime.pdf",runtime_gpu)

#pick only entries that are both in cpu and gpu
select_sizes <- function(tag, cpu_data, gpu_data) {

  gpu_rows <- filter(gpu_data, shape == tag)
  gpu_data_sizes <- levels(as.factor(gpu_rows$data_in_mb))
  selected_cpu_rows <- filter(cpu_data, data_in_mb %in% c(gpu_data_sizes) )
  gpu_rows$speed_up <-  as.numeric(selected_cpu_rows$total_time_ms) / as.numeric(gpu_rows$total_time_ms)
  gpu_rows$cpu_total_time_ms <- as.numeric(selected_cpu_rows$total_time_ms)
  
  gpu_rows
  
}


## data_to_plot <- gpu_only
## best_gpu <- filter(data_to_plot,tx_type == "async2plans")
## gpu_data_sizes <- levels(as.factor(filter(best_gpu, grepl("K20",dev_name) )$data_in_mb))
## gpu_names <- c(levels(as.factor(best_gpu$dev_name)))
## best_gpu <- filter(best_gpu, data_in_mb %in% c(gpu_data_sizes) )

## cpu_only_reduced <- filter(cpu_only, data_in_mb %in% c(gpu_data_sizes) , n_devices<40)
## sprintf("original cpu_only_reduced %i,found %i gpu names, gpu_data_sizes %i",nrow(cpu_only_reduced),length(gpu_names),length(gpu_data_sizes))
## cpu_only_reduced <- do.call(rbind, replicate(length(gpu_names), cpu_only_reduced, simplify=FALSE))


## ## best_gpu should match cpu_only_reduced now
## sprintf("best_gpu should match cpu_only_reduced now %i %i",nrow(best_gpu),nrow(cpu_only_reduced))
## head(cpu_only_reduced)

## best_gpu$cpu_time_ms <- as.numeric(cpu_only_reduced$total_time_ms)
## best_gpu$speed_up_over_best_cpu <- best_gpu$cpu_time_ms / best_gpu$total_time_ms


speed_up_gpu <- ggplot(best_gpu, aes(x=data_in_mb, y=as.numeric(speed_up_over_best_cpu), linetype=as.factor(dev_name) )) 
speed_up_gpu <- speed_up_gpu + geom_line(size=1.5) + my_theme
cpu_name <- c(levels(as.factor(cpu_only_reduced$dev_name)))
speed_up_gpu <- speed_up_gpu + ylab(sprintf("speed-up over %s",cpu_name))
speed_up_gpu <- speed_up_gpu + xlab("input data / MB") + ylim(0,1.5) + scale_linetype_manual(values=c("solid","dashed","dotted"))
speed_up_gpu <- speed_up_gpu + geom_hline(yintercept = 1, colour = 'blue', linetype = "dotted")

ggsave("batched_gpu_speed_up.png",speed_up_gpu)
ggsave("batched_gpu_speed_up.svg",speed_up_gpu)
ggsave("batched_gpu_speed_up.pdf",speed_up_gpu)
