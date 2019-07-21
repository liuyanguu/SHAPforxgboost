# install.packages("usethis")
library("usethis")
library("here")
path = here()
create_package(path)

proj_activate(path)
use_mit_license("Yang Liu liuyanguu")

pkgs <- c("data.table","ggplot2","ggforce","ggExtra","gridExtra","grid", "RColorBrewer")
invisible(lapply(pkgs, usethis::use_package, min_version = T))

usethis::use_package("here", type = "suggests")
# use_package()

dataXY_df <- fread("D:/liuyanguu/Blogdown/hugo-xmag/Intermediate/terradataXY.csv")
rferesults_terra <- readRDS("D:/liuyanguu/Blogdown/hugo-xmag/Intermediate/cwv_10by10_terra_new_f9")
shap_score <- rferesults_terra$shap_score
var_list_a <- rferesults_terra$features_rank_full_model
use_data(shap_score)
use_data(dataXY_df)
use_data(var_list_a)

use_roxygen_md()
use_readme_md()
