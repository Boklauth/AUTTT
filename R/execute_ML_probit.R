#'Execute Mplus Input Files Using Estimator Maximum Likelihood with the Probit Model
#'
#' The function executes Mplus scripts (.inp) in the background, and the estimator
#' is maximum likelihood with the probit model. The other data required are "folders.Rdata",
#' which contains the study conditions and is created during the data generation
#' process; "study_cell.Rdata", which contains the data sets of all replications
#' for each condition.
#'
#' @param main_dir2 This is the main directory in which folders representing
#' study conditions and data sets for the conditions are stored.
#' @param folder_index This takes one or more numeric values and contain the index
#' numbers of the study conditions.
#' @param nReps This is the total number of replications. The function will
#' develop and execute the scripts one by one.
#'
#' @export
#' @examples
#' library(AUTTT)
#'
#'	main_dir2 <- "C:/Users/shh6304/Desktop/test6"
#'	# load folders
#'	load(paste0(main_dir2, "/folders.Rdata"))
#'	# study_cell.Rdata will be searched in each study condition folder
#'	# once it finds the condition folders.
#'	est_folder <- c("ULSMV_delta")
#' execute_ML_probit(main_dir2 = main_dir2,
#'                  nReps = 500)
#'
# 3 Write Mplus Scripts and Analyze data using ML Probit ####

#' main_dir2 <- "C:/Users/shh6304/Desktop/t8"

#' execute_ML_probit(main_dir2 = "C:/Users/shh6304/Desktop/t8",
#'                   folder_index = C(1,2),
#'                    nReps = 500)

execute_ML_probit <- function(main_dir2,
                              folder_index,
                              nReps){

# declare variable


if(dir.exists(main_dir2)){
  setwd(main_dir2)
  # load folders
  load(paste0(getwd(), "/folders.Rdata"))
}else {
  message(paste0(main_dir2, " does not exist. Please check the directory."))
}



for(i in folder_index){ # specify a number range or numbers to select a condition
  message(paste0("Loading data model from: ", folders[i]))
  load(paste0(main_dir2, "/", folders[i], "/study_cell.Rdata"))
  message("Creating Mplus scripts in: ")
  # supply a new directory here
  my_new_dir <- paste0(main_dir2, "/", folders[i])
  # message
  message("Selected condition:")
  message(my_new_dir)
  start_time <- Sys.time()
  message("Start time: ", start_time)

  # write and/or execute Mplus scripts


  for (r in 1:nReps){
    time1 <- Sys.time() # time stamp
    mplus_montecarlo_analysis_grm2(model_object = study_cell,
                                   use_new_dir = my_new_dir,
                                   estimators = c("ML_probit"),
                                   rep = r,
                                   type_montecarlo = FALSE,
                                   run_files = TRUE)
    # message per replication
    time2 <- Sys.time()
    message("duration: ")
    time_diff <- time2 - time1
    message(time_diff)
    message(".....................")
  }
  # end of script writing and execution
  # message
  end_time <- Sys.time()
  message("End time: ", end_time)
  message("Total duration:")
  message(end_time - start_time)
}
} # end function

