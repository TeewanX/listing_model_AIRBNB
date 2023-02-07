loadConfiguration <- function(location) {
  require(yaml)
  configuration <- read_yaml(file = location)
  
  for (pkg in configuration$packages) {
    print(pkg)
    if (!require(pkg, character.only = T)) {
      install.packages(pkg, dependencies = T)
    }
    library(pkg, character.only = T)
  }
  
  configuration$parameters <- loadParameters(configuration)
  
  return(configuration)
}

loadParameters <- function(configuration) {
  
  parameters <- read_yaml(configuration$parameters$location)
  
  return(parameters)
}