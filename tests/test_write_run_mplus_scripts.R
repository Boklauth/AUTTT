# Create mplus scripts
library(AUTTT)
?mplus_montecarlo_analysis_grm2

## Write Mplus Scripts ####
# Note that we have the option to run the files as well while creating the script.
# This may be helpful when you don't have many replications.

main_dir <- "G:/My Drive/CLASSES/0000_Dissertation/2_data_gen_and_analysis"
data_dir <- "C:/Users/BK/Desktop/test9"
# Load folders, aka data cells
load(paste0(data_dir, "/folders.Rdata"))
# In each cell, there is a model object


my_est <-c('MLR', 'ULSMV_delta', 'WLSMV_delta')
selected_f <- c(1, 2, 3, 4)
for (cell in selected_f){
  load(paste0(data_dir, "/", folders[cell], "/", "study_cell.Rdata"))
  for (reps in 1 : 5){
    mplus_montecarlo_analysis_grm2(model_object = study_cell,
                                   use_new_dir = NULL,
                                   estimators = my_est,
                                   rep = reps,
                                   type_montecarlo = FALSE,
                                   run_files = TRUE
    )
  }
}



# End of writing Mplus scripts ####







