https://r-spatial.github.io/rgee/
  
  
  # rgee?

reticulate::install_miniconda()





library(reticulate)


library(remotes)
library(rgee)

pip install earthengine-api --upgrade

rgee::ee_install_set_pyenv(
  py_path = "/home/csaybar/.virtualenvs/rgee/bin/python", # Change it for your own Python PATH
  py_env = "rgee" # Change it for your own Python ENV
)





library(rgee)
ee_Initialize()
db <- 'CGIAR/SRTM90_V4'
image <- ee$Image(db)
image$bandNames()$getInfo()
#> [1] "elevation"

