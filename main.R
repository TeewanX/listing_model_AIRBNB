rm(list = ls())

#Setting the root directory to this project's
ROOT.DIRECTORY <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(ROOT.DIRECTORY)

#Loading functions
source.files <- list.files(path = 'rcode', recursive = T)
invisible(lapply(paste0('rcode/', source.files), source))

#Starting calibration
runAll()