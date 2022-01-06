
# 3 Write Mplus Scripts and Analyze data using ML Probit ####

#' main_dir2 <- "C:/Users/shh6304/Desktop/t8"

#' execute_ML_probit(main_dir2 = "C:/Users/shh6304/Desktop/t8",
#'                    nReps = 500)

execute_ML_probit <- function(main_dir2,
                              nReps){

# declare variable
new_dir <- main_dir2

if(dir.exists(main_dir2)){
  setwd(main_dir2)
}else {
  dir.create(main_dir2)
  setwd(main_dir2)
}
# load folders
load(paste0(getwd(), "/folders.Rdata"))
# check folders
folders[1:24]

nReps <- 5
for(i in 12:12){ # specify a number range or numbers to select a condition
  message(paste0("Loading data model from: ", folders[i]))
  load(paste0(main_dir2, "/", folders[i], "/study_cell.Rdata"))
  message("Creating Mplus scripts in: ")
  # supply a new directory here
  my_new_dir <- paste0(main_dir2, "/", folders[i])
  print(my_new_dir)
  # message
  start_time <- Sys.time()
  message("Start time: ", start_time)

  # write and/or execute Mplus scripts


  for (r in 1:nReps){
    time1 <- Sys.time() # time stamp
    mplus_montecarlo_analysis_grm2(model_object = study_cell,
                                   use_new_dir = new_dir,
                                   estimators = c("ML_probit"),
                                   rep = r,
                                   type_montecarlo = FALSE,
                                   run_files = TRUE)
    # message per replication
    time2 <- Sys.time()
    message("duration: ")
    time_diff <- time2 - time1
    time_diff
    message(".....................")
  }
  # end of script writing and execution
  # message
  end_time <- Sys.time()
  message("End time: ", end_time)
  message("Total duration:")
  end_time - start_time
}
} # end function

