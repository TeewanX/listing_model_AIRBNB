loadParameters <- function(location) {
  require(yaml)
  parameters <- read_yaml(file = location)
  
  for (p in parameters$packages) {
    if (!require(p, character.only = T)) {
      install.packages(p, dependencies = T)
    }
    require(p)
  }
  return(parameters)
}