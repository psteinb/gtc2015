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
all_data$data_in_mb <- 8*(as.numeric(all_data$stack_dims_x)*as.numeric(all_data$stack_dims_y)*as.numeric(all_data$stack_dims_z))*4/(1024*1024)
all_data$shape <- sprintf("%sx%sx%s",all_data$stack_dims_x,all_data$stack_dims_y,all_data$stack_dims_z)


my_theme <-  theme_bw() + theme(axis.title.x = element_text(size=20),
                                axis.title.y = element_text(size=20),
                                axis.text.x = element_text(size=16),
                                axis.text.y = element_text(size=16)) + theme(legend.title = element_text(size=22, face="bold")) + theme(legend.text = element_text( size = 16)) + theme(legend.title=element_blank()) + theme(legend.position="top") + theme(axis.text.x  = element_text()) + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0., linetype='dashed')) + theme(legend.key.width = unit(1.5, "cm"))

cpu_only <- filter(all_data, grepl("cpu",dev_type) , n_devices!=1, n_devices == 16 )
cpu_only$is_cpu <- "cpu"
cpu_only$mode <- 'interleaved'

gpu_only <- filter(all_data, grepl("gpu",dev_type)## , grepl("K20",dev_name)
                   )
gpu_only$is_cpu <- "gpu"

gpu_only$mode[ grepl('plan_many',gpu_only$comment)] <- 'plan_many'
gpu_only$mode[ !grepl('plan_many',gpu_only$comment) ] <- 'interleaved'

data_to_plot <- cpu_only
data_to_plot <- rbind(data_to_plot, gpu_only)

data_to_plot$dev_name[data_to_plot$dev_name == '12xE5-26400'] <- '12xE5-2640'
data_to_plot$dev_name[data_to_plot$dev_name == 'GeForce_GTX_TITAN_Black'] <- 'Titan_Black'


select_sizes <- function(mode_tag, cpu_data, gpu_data) {
  sprintf("%s",mode_tag)
  gpu_rows <- filter(gpu_data, mode == mode_tag)
  gpu_data_sizes <- levels(as.factor(gpu_rows$data_in_mb))
  selected_cpu_rows <- filter(cpu_data, data_in_mb %in% c(gpu_data_sizes) )
  gpu_rows$speed_up <-  as.numeric(selected_cpu_rows$total_time_ms) / as.numeric(gpu_rows$total_time_ms)
  gpu_rows$cpu_total_time_ms <- as.numeric(selected_cpu_rows$total_time_ms)
  
  gpu_rows
  
}

k20_data <- filter(gpu_only, grepl("K20", dev_name))

modes_to_loop <- c(levels(as.factor(k20_data$mode)))
new_gpu_data <- do.call("rbind", lapply(modes_to_loop, FUN = select_sizes, cpu_only, k20_data))


other_data <- filter(gpu_only, !grepl("K20", dev_name))
modes_to_loop <- c(levels(as.factor(other_data$mode)))
new_gpu_data <- rbind(new_gpu_data,do.call("rbind", lapply(modes_to_loop, FUN = select_sizes, cpu_only, other_data)))

new_gpu_data

cpu_name <- c(levels(as.factor(cpu_only$dev_name)))

runtime_gpu <- ggplot(new_gpu_data, aes(x=data_in_mb, y=as.numeric(speed_up), color=as.factor(dev_name),linetype=as.factor(mode))) 
runtime_gpu <- runtime_gpu + geom_line(size=1.5) + my_theme + scale_x_log10()
runtime_gpu <- runtime_gpu + geom_hline(yintercept = 1, colour = 'blue', linetype = "dotted") + ylim(0,1.2*max(new_gpu_data$speed_up))
runtime_gpu <- runtime_gpu + ylab(sprintf("speed-up over %s",cpu_name)) + xlab("input data / MB")#+ guides(color=guide_legend(nrow=2,byrow=TRUE)) #

ggsave("batched_folds_gpu_speed_up.png",runtime_gpu)
ggsave("batched_folds_gpu_speed_up.svg",runtime_gpu)
ggsave("batched_folds_gpu_speed_up.pdf",runtime_gpu)

