#----------------------------------------------------------#
# Load packages -----
#----------------------------------------------------------#
if(!isTRUE(require(tidyverse, quietly = TRUE))) {
  install.packages("tidyverse", dependencies = TRUE); library(tidyverse)
} else {
  require(tidyverse)
}

if(!isTRUE(require(pdftools, quietly = TRUE))) {
  install.packages("pdftools", dependencies = TRUE); library(pdftools)
} else {
  require(pdftools)
}


