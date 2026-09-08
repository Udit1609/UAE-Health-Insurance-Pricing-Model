install.packages("tidyverse")
library(tidyverse)

install.packages("broom")
library(broom)

install.packages("MASS")
library(MASS)


set.seed(12345)
options(scipen = 999)
output_folder <- "UAE_Health_Pricing_model"
dir.create(output_folder, showWarnings = FALSE)
options(tibble.width = Inf, tibble.print_max = 10)
