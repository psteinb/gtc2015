#!/usr/bin/env Rscript

argv = commandArgs(TRUE)

if(length(argv) < 1){
    stop("Usage: single_fft_runtime.R <data_file> ")
}

write(argv)
inputfiles <- argv

file_reader <- function(fn) {
  read.table(fn, header = T, sep = ' ') 
}

all_data <- do.call("rbind", lapply(inputfiles, file_reader))

head(all_data)
tail(all_data)
nrow(all_data)
