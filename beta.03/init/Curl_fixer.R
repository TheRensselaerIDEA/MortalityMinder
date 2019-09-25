# This fixes a new problem with the curl package
require(devtools)

if (!require("curl", character.only = T)) {
  install_version("curl", version = "4.0", repos = "http://cran.us.r-project.org")
  require("curl", character.only = T)
}
