loadConfiguration <- function(location) {
  #reading the configuration file
  require(yaml)
  configuration <- read_yaml(file = location)
  
  #loading all the prescribed packages and installing them if not available
  for (pkg in configuration$packages) {
    print(pkg)
    if (!require(pkg, character.only = T)) {
      install.packages(pkg, dependencies = T)
    }
    library(pkg, character.only = T)
  }
  
  #loading in calibration specific parameters
  configuration$parameters <- loadParameters(configuration)
  
  return(configuration)
}

loadParameters <- function(configuration) {
  
  #reading the parameters
  parameters <- read_yaml(configuration$parameters$location)
  
  return(parameters)
}