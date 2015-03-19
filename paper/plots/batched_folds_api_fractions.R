#!/usr/bin/env Rscript

argv = commandArgs(TRUE)

if(length(argv) < 1){
    stop("Usage: batched_folds_api_fractions.R <data_file> ")
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
gpu_nd_data$data_in_mb <- 8*2*as.numeric(gpu_nd_data$stack_dims_x)*as.numeric(gpu_nd_data$stack_dims_z)*as.numeric(gpu_nd_data$stack_dims_y)*4/(1024.*1024.)
  
data_api_asides_selected <- select(gpu_nd_data, dev_name, comment, data_in_mb, cuda_cpy_plus_mem,cuda_mem_perc)

head(data_api_asides_selected)

data_api_asides <- filter(data_api_asides_selected, !grepl("NVS",dev_name))


library(reshape2)
api_melted <- melt(data_api_asides,
                   id.vars=c("data_in_mb","dev_name","comment"),
                   measure.vars=c("cuda_cpy_plus_mem","cuda_mem_perc"))

levels(api_melted$variable)[levels(api_melted$variable)=="cuda_cpy_plus_mem"] <- "Memcpy+Malloc+Free"
levels(api_melted$variable)[levels(api_melted$variable)=="cuda_mem_perc"] <- "Malloc+Free"

p_api_fractions <- ggplot(api_melted, aes(x=data_in_mb, y=value, colour=variable, linetype=dev_name)) 
p_api_fractions <- p_api_fractions + geom_line(size=1.5) + my_theme #+ scale_y_log10()
p_api_fractions <- p_api_fractions + ylab("api runtime fraction / %") + xlab("input data / MB")
p_api_fractions <- p_api_fractions 
p_api_fractions <- p_api_fractions + scale_y_continuous(limits = c(0, 100))
ggsave("batched_folds_api_fractions.png",p_api_fractions)
