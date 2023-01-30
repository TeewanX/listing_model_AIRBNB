loadParameters <- function(location) {
  require(yaml)
  parameters <- read_yaml(file = location)
  
  for (pkg in parameters$packages) {
    print(pkg)
    if (!require(pkg, character.only = T)) {
      install.packages(pkg, dependencies = T)
    }
    library(pkg, character.only = T)
  }
  
  parameters$configuration <- loadConfiguration(parameters)
  
  return(parameters)
}

loadConfiguration <- function(parameters) {
  
  configuration <- read_yaml(parameters$configuration$location)
  
  return(configuration)
}