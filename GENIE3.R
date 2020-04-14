#!/usr/bin/env Rscript
## Goal of this program: Apply the TVDBN methods available in package 'EDISON' [1, 2].
## on the given datasets.
##
##
## The coding practices followed are documented at:
## https://github.com/sap01/coding_practices/blob/master/R_coding_practices.md
##
## Begin: References
## 1. Dondelinger, Frank, Sophie L?bre, and Dirk Husmeier. "Non-homogeneous
## dynamic Bayesian networks with Bayesian regularization for inferring
## gene regulatory networks with gradually time-varying structure."
## Machine Learning 90.2 (2013): 191-230.
##
## 2. The 'EDISON' package in R: https://CRAN.R-project.org/package=EDISON
## End: References
##
## Usage:
## For Unix-alike OSes:
## Let us assume that this script is inside directory
## '/home/saptarshi/R/R-3.3.2/projects/repoedisonr' and
## the Rscript file is inside directory '/home/saptarshi/R/R-3.3.2/bin'.
## Then, execute this script using the following commands (the '$' symbol
## represents the Bash command prompt):
## $ cd /home/saptarshi/R/R-3.3.2/projects/repoedisonr/
## $ nohup time /home/saptarshi/R/R-3.3.2/bin/Rscript /home/saptarshi/R/R-3.3.2/projects/repoedisonr/EDISON.R input.json &
## where '/repoedisonr/asset/input.json' contains the user-defined parameters. A file
## named 'nohup.out' will be generated inside
## '/home/saptarshi/R/R-3.3.2/projects/repoedisonr/'.
##
## For Windows OSes:
## Let us assume that this script is inside directory 'D:\R\R-3.3.2\projects\repoedisonr' and
## the 'Rscript.exe' file is inside directory 'C:\Program Files\R\R-3.3.1\bin'.
## Then, execute this script using the following commands (the '>' symbol
## represents the DOS command prompt):
## >cd "D:\R\R-3.3.2\projects\repoedisonr"
## >"C:\Program Files\R\R-3.3.1\bin\Rscript.exe" EDISON.R input.json
## where '/repoedisonr/asset/input.json' contains the user-defined parameters.
##
## Input: A time series gene expression dataset with multiple time series.
## TODO (sap)
##
## Output: Time-varying Gene Regulatory Networks and a corresponding rolled up network.
##
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
init.path <- base::getwd()

input.args <- base::commandArgs(trailingOnly = TRUE)

if (base::length(args) != 1)
{
  base::stop("Exactly one input file must be supplied.", call. = FALSE)
}

input.params <-
  rjson::fromJSON(file = paste(init.path, 
                               'asset', 
                               input.args, 
                               sep = '/'))
base::rm(input.args)

## Input file for time-series gene expression data
input.data.filename <- input.params$input.data.filename
input.data.filename <- paste(init.path, 
                             'asset', 
                             input.data.filename, 
                             sep = '/')

## Number of time points (T)
num.timepts <- input.params$num.timepts

## Number of time series (S)
num.time.series <- input.params$num.time.series

## Please see argument 'regulators' of
## GENIE3::GENIE3().
regulators <- input.params$regulators

## Please see argument 'targets' of
## GENIE3::GENIE3().
targets <- input.params$targets

## Please see argument 'treeMethod' of
## GENIE3::GENIE3().
treeMethod <- input.params$treeMethod

## Please see argument 'K' of
## GENIE3::GENIE3().
K <- input.params$K

## Please see argument 'nTrees' of
## GENIE3::GENIE3().
nTrees <- input.params$nTrees

## Please see argument 'nCores' of
## GENIE3::GENIE3().
nCores <- input.params$nCores

## Please see argument 'verbose' of
## GENIE3::GENIE3().
verbose <- input.params$verbose

base::rm(input.params)
##------------------------------------------------------------
## End: Read User-defined input Params
##------------------------------------------------------------

##------------------------------------------------------------
## Begin: Create the output directory
##------------------------------------------------------------

## Output directory name
output.dirname <- base::paste('output', 
                              format(Sys.time(), 
                                     "%Y%m%d%H%M%S"), 
                              sep = '')

if (base::.Platform$OS.type == 'windows') {
  if (!output.dirname %in% base::shell("ls asset" , intern = TRUE)) {
    ## Output directory name for Windows OSes
    output.dirname <- base::paste('asset', output.dirname, sep = '/')
    output.dirname <- base::paste(init.path, output.dirname, sep = '/')
    
    ## Convert directory path to canonical form for the Windows OS.
    ## It raises the warning if the directory does not exist, which
    ## is expected. Therefore, please ignore the warning.
    output.dirname <- base::normalizePath(output.dirname, 
                                          winslash = '\\', 
                                          mustWork = NA)
    
    base::shell(paste('mkdir ', output.dirname, sep = ''),
                intern = TRUE,
                mustWork = TRUE)
  }
} else {
  if (base::.Platform$OS.type == 'unix') {
    if (!output.dirname %in% base::system("ls asset" , intern = TRUE)) {
      output.dirname <- base::paste('asset', output.dirname, sep = '/')
      output.dirname <- base::paste(init.path, output.dirname, sep = '/')
      
      base::system(paste('mkdir ', output.dirname, sep = ''))
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
base::print(output.dirname)
base::print('') ## to append a blank line

## Save console output in a file named 'output.txt' inside the output directory.
output.filename <- base::paste(output.dirname, 'output.txt', sep = '/')
output.file.conn <- base::file(output.filename, open = "wt")
base::sink(output.file.conn)

##------------------------------------------------------------
## Begin: Read input data file
##------------------------------------------------------------

## Begin: Find file extension of the input data file. Only '.tsv' and '.RData'
## are allowed.
## Split the string at every '.' and consider the last substring as the
## file extension.
input.data.filename.ext <-
  base::unlist(strsplit(input.data.filename, '[.]'))
## End: Find file extension of the input data file. Only '.tsv' and '.RData'
## are allowed.

input.data <- NULL

if (input.data.filename.ext[length(input.data.filename.ext)] == 'tsv') {
  input.data <-
    utils::read.table(input.data.filename, header = TRUE, sep = "\t")
  
  ## Remove first col i.e. the time point names
  input.data <- input.data[,-1]
  
} else if (input.data.filename.ext[length(input.data.filename.ext)] == 'RData') {
  ## Loads an object named 'input.data'
  base::load(input.data.filename)
}

num.nodes <- base::ncol(input.data)
##------------------------------------------------------------
## End: Read input data
##------------------------------------------------------------

##------------------------------------------------------------
## Begin: Learn the EDISON model
##------------------------------------------------------------

## start the timer
start.time <- base::proc.time()

node.names <- base::colnames(input.data)

## Input data is required to be a matrix where rows = nodes and
## cols = samples.
## Please see input param 'exprMatrix' of GENIE3::GENIE3().
input.data <- base::t(input.data)

## Initialize weighted adjacency matrix of the
## to-be-reconstructed directed net
di.net.adj.matrix.wt <- NULL

## Apply GENIE3 on one time series at a time
for (time.series.idx in 1:num.time.series) {
  ## Input data of the current time series
  input.data.curr.series <- input.data[, 1:num.timepts]
  
  ## Remaining input data
  input.data <- input.data[,-(1:num.timepts)]
  
  ## Make results reproducibile
  base::set.seed(seed = 123)
  
  ## Run GENIE3 on the current time series to
  ## reconstruct weighted adjacency matrix of 
  ## the directed net.
  di.net.adj.matrix.wt.curr.series <- 
    GENIE3::GENIE3(exprMatrix = input.data.curr.series, 
                   regulators = regulators, 
                   targets = targets, 
                   treeMethod = treeMethod, 
                   K = K, 
                   nTrees = nTrees, 
                   nCores = nCores, 
                   verbose = verbose)
  
  base::save(
    net.adj.mx.wt,
    file = paste(output.dirname,
                 '/GENIE3.result',
                 time.series.idx,
                 '.RData',
                 sep = ''))
  
  base::rm(input.data.curr.series)
  
  
  base::rm(net.adj.mx.wt)
  
  base::print(paste('Series', 
                    time.series.idx, 
                    'is completed.', 
                    sep = ' '))
  
}
base::rm(time.series.idx)
##------------------------------------------------------------
## End: Learn the EDISON model
##------------------------------------------------------------

## Stop the timer
elapsed.time <- (base::proc.time() - start.time)
base::writeLines('elapsed.time = \n')
base::print(elapsed.time)

## Close output to the 'console_output.txt'
base::sink()
base::close(output.file.conn)

## Save R session info in a file named 'sessionInfo.txt' inside the output directory.
output.filename <-
  base::paste(output.dirname, 'sessionInfo.txt', sep = '/')
output.file.conn <- base::file(output.filename, open = "wt")
base::rm(output.filename)
base::sink(output.file.conn)
utils::sessionInfo()
base::sink()
base::close(output.file.conn)
##------------------------------------------------------------
## End: Main Program
##------------------------------------------------------------
