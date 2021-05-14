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

