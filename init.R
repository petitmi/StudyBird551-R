# R script to run author supplied code, typically used to install additional R packages
# contains placeholders which are inserted by the compile script
# NOTE: this script is executed in the chroot context; check paths!

r <- getOption('repos')
r['CRAN'] <- 'http://cloud.r-project.org'
options(repos=r)

# ======================================================================

# packages go here
install.packages(c('readr', 'here', 'ggthemes', 'remotes','dashCoreComponents','dashHtmlComponents','dplyr','forcats','ggplot2','plotly','readxl','tidyr','tidyverse','wordcloud2','webshot','htmlwidgets','base64enc'))
remotes::install_github("plotly/dashR", upgrade = "always")
remotes::install_github('facultyai/dash-bootstrap-components@r-release')
