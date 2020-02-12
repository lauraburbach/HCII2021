# This file helps to download the data from the osf.
# it requires the osfr package from github
#
#
if (!require(remotes)) {
  install.packages("remotes")
} 
if (!require(osfr)) {
  remotes::install_github("ropensci/osfr")  
}

decompress_file <- function(directory, file, .file_cache = FALSE) {
  
  if (.file_cache == TRUE) {
    print("decompression skipped")
  } else {
    
    # Set working directory for decompression
    # simplifies unzip directory location behavior
    wd <- getwd()
    setwd(directory)
    
    # Run decompression
    decompression <-
      system2("unzip",
              args = c("-o", # include override flag
                       file),
              stdout = TRUE)
    
    # uncomment to delete archive once decompressed
    # file.remove(file) 
    
    # Reset working directory
    setwd(wd); rm(wd)
    
    # Test for success criteria
    # change the search depending on 
    # your implementation
    if (grepl("Warning message", tail(decompression, 1))) {
      print(decompression)
    }
  }
}    

# set the OSF_PAT to your personal access token if auth is required
# install usethis before running this code.
# usethis::edit_r_environ() 
# add line OSF_PAT="yourpathere"

library(dplyr)

osf_auth(Sys.getenv("OSF_PAT"))

# redownload all the data 
osf_retrieve_node("bvqnt") %>%   # retrieve the project
  osf_ls_nodes() %>%                       # get all subnodes
  dplyr::filter(name == "Simulation Data") %>%    # pick the data node
  pull(id) %>%                             # get their id
  osf_retrieve_node() %>%                  # retrieve all files nodes
  osf_ls_files() %>%                       # put them in a osf_tbl
  osf_download("data", conflicts = "overwrite")  # download and overwrite

cat("Done!")

# unzip the data?
decompress_file("data", "netlogo.zip")
decompress_file("data", "julia.zip") # file is > 5Gb

