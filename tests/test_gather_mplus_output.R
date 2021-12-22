main_dir2 <- "C:/Users/shh6304/Desktop/test"
load(paste0(main_dir2, "/folders.Rdata"))
cell_folders <- folders
est_folders <- c("ULSMV_delta")
cell_prefix <- "U"

ulsmv_est <- gather_mplus_output(nfolders = length(cell_folders),
                                 nreps = 5,
                                 cell_prefix = "U")
