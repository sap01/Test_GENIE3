#!/usr/bin/env Rscript
#' Test R package 'GENIE3'
#' 
#' Test R package 'GENIE3' on various datasets
#' 
#' @section Coding style:
#' \url{https://github.com/sap01/coding_practices/blob/master/R_coding_practices.md}
#' 
#' @section Usage for Unix-alike OSes:
#' Let us assume that this script is inside directory
#' '/home/username/R/R-x.y.z/projects/Test_GENIE3' and
#' the Rscript file is inside directory 
#' '/home/username/R/R-x.y.z/bin'.
#' Then, execute this script using the following commands 
#' (the '$' symbol represents the Bash command prompt):
#' $ cd /home/username/R/R-x.y.z/projects/Test_GENIE3/
#' $ nohup time -v /home/username/R/R-x.y.z/bin/Rscript 
#' /home/username/R/R-x.y.z/projects/Test_GENIE3/GENIE3.R 
#' input.json &
#' 
#' where '/Test_GENIE3/asset/input.json' contains the user-defined 
#' parameters. A file named 'nohup.out' will be generated inside
#' '/home/username/R/R-x.y.z/projects/Test_GENIE3/'.
#'
#' @section Usage for Windows OSes:
#' Let us assume that this script is inside directory 
#' 'D:\R\R-x.y.z\projects\Test_GENIE3' and
#' the 'Rscript.exe' file is inside directory 
#' 'C:\Program Files\R\R-x.y.z\bin'.
#' Then, execute this script using the following commands 
#' (the '>' symbol represents the DOS command prompt):
#' >cd "D:\R\R-x.y.z\projects\Test_GENIE3"
#' >"C:\Program Files\R\R-x.y.z\bin\Rscript.exe" 
#' GENIE3.R input.json
#' 
#' where '/Test_GENIE3/asset/input.json' contains the user-defined 
#' parameters.
#'
#' @param input_args Name of a JSON file that contains the 
#' parameter values for \code{GENIE3::GENIE3()}. The file
#' must be stored inside the 'asset' sub-directory.
#' 
#' @return R object 'di_net_adj_matrix_wt'. This is the
#' adjacency matrix of the directed network reconstructed
#' with GENIE3 from the input data. If an input data has
#' multiple time series, then GENIE3 reconstructs one 
#' adjacency matrix from each time series; consequently, 
#' all matrices are added to reconstruct the final matrix.
#'
## Remove all objects in the current workspace
rm(list = ls())

##------------------------------------------------------------
## Begin: Load the Required Packages
##------------------------------------------------------------
## For reading from and writing to '.json' files
library(rjson)

library(GENIE3)
##------------------------------------------------------------
## End: Load the Required Packages
##------------------------------------------------------------

##------------------------------------------------------------
## Begin: Read User-defined input Params
##------------------------------------------------------------
init_path <- base::getwd()

input_args <- base::commandArgs(trailingOnly = TRUE)

if (base::length(args) != 1)
{
  base::stop("Exactly one input file must be supplied.", call. = FALSE)
}

input_params <-
  rjson::fromJSON(file = paste(init_path, 
                               'asset', 
                               input_args, 
                               sep = '/'))
base::rm(input_args)

## Input file for time-series gene expression data
input_data_filename <- input_params$input_data_filename
input_data_filename <- paste(init_path, 
                             'asset', 
                             input_data_filename, 
                             sep = '/')

## Number of time points (T)
num_timepts <- input_params$num_timepts

## Number of time series (S)
num_time_series <- input_params$num_time_series

## Please see argument 'regulators' of
## GENIE3::GENIE3().
regulators <- input_params$regulators

## Please see argument 'targets' of
## GENIE3::GENIE3().
targets <- input_params$targets

## Please see argument 'treeMethod' of
## GENIE3::GENIE3().
treeMethod <- input_params$treeMethod

## Please see argument 'K' of
## GENIE3::GENIE3().
K <- input_params$K

## Please see argument 'nTrees' of
## GENIE3::GENIE3().
nTrees <- input_params$nTrees

## Please see argument 'nCores' of
## GENIE3::GENIE3().
nCores <- input_params$nCores

## Please see argument 'verbose' of
## GENIE3::GENIE3().
verbose <- input_params$verbose

base::rm(input_params)
##------------------------------------------------------------
## End: Read User-defined input Params
##------------------------------------------------------------

##------------------------------------------------------------
## Begin: Create the output directory
##------------------------------------------------------------

## Output directory name
output_dirname <- base::paste('output', 
                              format(base::Sys.time(), 
                                     "%Y%m%d%H%M%S"), 
                              sep = '')

if (base::.Platform$OS.type == 'windows') {
  if (!output_dirname %in% base::shell("ls asset" , intern = TRUE)) {
    ## Output directory name for Windows OSes
    output_dirname <- base::paste('asset', output_dirname, sep = '/')
    output_dirname <- base::paste(init_path, output_dirname, sep = '/')
    
    ## Convert directory path to canonical form for the Windows OS.
    ## It raises the warning if the directory does not exist, which
    ## is expected. Therefore, please ignore the warning.
    output_dirname <- base::normalizePath(output_dirname, 
                                          winslash = '\\', 
                                          mustWork = NA)
    
    base::shell(paste('mkdir ', output_dirname, sep = ''),
                intern = TRUE,
                mustWork = TRUE)
  }
} else {
  if (base::.Platform$OS.type == 'unix') {
    if (!output_dirname %in% base::system("ls asset" , intern = TRUE)) {
      output_dirname <- base::paste('asset', output_dirname, sep = '/')
      output_dirname <- base::paste(init_path, output_dirname, sep = '/')
      
      base::system(paste('mkdir ', output_dirname, sep = ''))
    }
  }
}
##------------------------------------------------------------
## End: Create the output directory
##------------------------------------------------------------

##------------------------------------------------------------
## Begin: Load the Required External Functions
##------------------------------------------------------------
## NA
##------------------------------------------------------------
## End: Load the Required External Functions
##------------------------------------------------------------

##------------------------------------------------------------
## Begin: Main program
##------------------------------------------------------------

## Print the output dir name in 'nohup.out'
base::print('The output directory name is:')
base::print(output_dirname)
base::print('') ## to append a blank line

## Save console output in a file named 'output.txt' inside the output directory.
output_filename <- base::paste(output_dirname, 'output.txt', sep = '/')
output_file_conn <- base::file(output_filename, open = "wt")
base::sink(output_file_conn)

##------------------------------------------------------------
## Begin: Read input data file
##------------------------------------------------------------

## Begin: Find file extension of the input data file. Only '.tsv' and '.RData'
## are allowed.
## Split the string at every '.' and consider the last substring as the
## file extension.
input_data_filename_ext <-
  base::unlist(strsplit(input_data_filename, '[.]'))
## End: Find file extension of the input data file. Only '.tsv' and '.RData'
## are allowed.

input_data <- NULL

if (input_data_filename_ext[length(input_data_filename_ext)] == 'tsv') {
  input_data <-
    utils::read.table(input_data_filename, header = TRUE, sep = "\t")
  
  ## Remove first col i.e. the time point names
  input_data <- input_data[,-1]
  
} else if (input_data_filename_ext[length(input_data_filename_ext)] == 'RData') {
  ## Loads an object named 'input_data'
  base::load(input_data_filename)
}

num_nodes <- base::ncol(input_data)
##------------------------------------------------------------
## End: Read input data
##------------------------------------------------------------

##------------------------------------------------------------
## Begin: Learn the GENIE3 model
##------------------------------------------------------------

## start the timer
start_time <- base::proc.time()

node_names <- base::colnames(input_data)

## Input data is required to be a matrix where rows = nodes and
## cols = samples.
## Please see input param 'exprMatrix' of GENIE3::GENIE3().
input_data <- base::t(input_data)

## Initialize weighted adjacency matrix of the
## to-be-reconstructed directed net
di_net_adj_matrix_wt <- NULL

## Apply GENIE3 on one time series at a time
for (time_series_idx in 1:num_time_series) {
  ## Input data of the current time series
  input_data_curr_series <- input_data[, 1:num_timepts]
  
  ## Remaining input data
  input_data <- input_data[,-(1:num_timepts)]
  
  ## Make results reproducibile
  base::set.seed(seed = 123)
  
  ## Run GENIE3 on the current time series to
  ## reconstruct weighted adjacency matrix of 
  ## the directed net
  di_net_adj_matrix_wt_curr_series <- 
    GENIE3::GENIE3(exprMatrix = input_data_curr_series, 
                   regulators = regulators, 
                   targets = targets, 
                   treeMethod = treeMethod, 
                   K = K, 
                   nTrees = nTrees, 
                   nCores = nCores, 
                   verbose = verbose)
  
  ## Sum time-series-spedific adjacency matrices
  if (time_series_idx == 1) {
    di_net_adj_matrix_wt <- 
      di_net_adj_matrix_wt_curr_series
  } else {
    di_net_adj_matrix_wt <- 
      (di_net_adj_matrix_wt +
         di_net_adj_matrix_wt_curr_series)
  }
  
  base::rm(input_data_curr_series, 
           di_net_adj_matrix_wt_curr_series)
  
  
  base::rm(net_adj_mx_wt)
  
  base::print(paste('Series', 
                    time_series_idx, 
                    'is completed.', 
                    sep = ' '))
  
}
base::rm(time_series_idx)

base::save(
  di_net_adj_matrix_wt,
  file = paste(output_dirname,
               '/di_net_adj_matrix_wt',
               '.RData',
               sep = ''))

##------------------------------------------------------------
## End: Learn the GENIE3 model
##------------------------------------------------------------

## Stop the timer
elapsed_time <- (base::proc.time() - start_time)
base::writeLines('elapsed_time = \n')
base::print(elapsed_time)

## Close output to 'output.txt'
base::sink()
base::close(output_file_conn)

## Save R session info in a file named 'sessionInfo.txt' inside the output directory.
output_filename <-
  base::paste(output_dirname, 'sessionInfo.txt', sep = '/')
output_file_conn <- base::file(output_filename, open = "wt")
base::rm(output_filename)
base::sink(output_file_conn)
utils::sessionInfo()
base::sink()
base::close(output_file_conn)
##------------------------------------------------------------
## End: Main Program
##------------------------------------------------------------
