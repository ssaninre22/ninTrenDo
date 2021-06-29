# ********************************************************** #
# ********** Google Trends Database Building Tool ********** #
# ********************************************************** #

library(devtools)
library(tidyverse)
library(fs)

#create_package("C:/Users/sebastian/Dropbox/Proyectos/Rpacks/ninTrenDo")

use_git()

dir_info(all = TRUE, regexp = "^[.]git$") %>% 
  select(path, type)

# Create function files with use_r
use_r("long_gt")
use_r("long_gt_ltc")

# Load functions ()
load_all()

# Check function
check()
# Licence
use_gpl_license(3)

# Documentation
devtools::document()
?long_gt
?ninTrenDo

usethis::use_readme_rmd()

# Install

install()


keyword = "Ethereum"
geo=""
input.sdate = as.Date("2014-09-01")
input.edate = as.Date("2021-06-26")
input.frequency = "d"
td.method="chow-lin"
input.delta = 6
input.ol.win = 1
input.type="web"
input.categ=0
