install.packages("usethis")
library("usethis")
library("here")
path = here()
create_package(path)

proj_activate(path)
use_mit_license("Yang Liu liuyanguu")
use_readme_md()
