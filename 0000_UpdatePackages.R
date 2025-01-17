#  "0000_UpdatePackages"

run = FALSE
# run = TRUE
if(run){
  library("pacman")

## Undata packages
mypks <- pacman::p_lib()
saveRDS(mypks, "~/mypks.rds")
mypks <- readRDS("~/mypks.rds")
install.packages(mypks)
}
run = FALSE
