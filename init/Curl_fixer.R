# This fixes a new problem with the curl package
if (!require("devtools", character.only = T)) {
  install.packages("devtools")
  require("devtools", character.only = T)
}

if (!require("curl", character.only = T)) {
  install_version("curl", version = "4.0", repos = "http://cran.us.r-project.org")
  require("curl", character.only = T)
}
