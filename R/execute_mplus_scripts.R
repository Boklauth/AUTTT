
#'Execute Mplus Input Files
#'
#' The function executes Mplus scripts (.inp) in the background. .
#'
#' @param main_dir This is the main directory in which folders representing
#' study conditions and data sets for the conditions are stored.
#' @param cell_folders This object contains the names of the the folders representing
#' the experimental conditions/cells.
#' @param est_folders It is the folder name(s), where Mplus scripts are created and
#' stored using the function mplus_montecarlo_analysis_grm2().
#' @param start_condition It is a numeric starting value representing an index
#' of the cell_folders. It can range from 1 to length(cell_folders).
#' @param end_condition It is a numeric ending value representing an index
#' of the cell_folders. It can range from 1 to length(cell_folders).
#'
#' @export
#' @examples
#' library(AUTTT)
#'
#'	main_dir2 <- "C:/Users/shh6304/Desktop/test6"
#'	# load folders
#'	load(paste0(main_dir2, "/folders.Rdata"))
#'	cell_folders <- folders
#'	est_folder <- c("ULSMV_delta")
#' execute_mplus_scripts(main_dir = main_dir2,
#'                      cell_folders = cell_folders,
#'                      est_folders = ULSMV_delta,
#'                      start_condition = 1,
#'                      end_condition = length(cell_folders))
#'
execute_mplus_scripts <- function(main_dir,
                                  cell_folders,
                                  est_folders,
                                  start_condition,
                                  end_condition){
  # Run models in the folder (/ML_probit or /ULSMV_delta or /WLSMV_delta)
  start_time <- Sys.time()
  print(paste0("Start time: ", start_time))
  for(i in start_condition:end_condition){
    # message
    message(paste0("Analyzing condition: ", folders[i]))

    time1 <- Sys.time()
    print(paste0("time1: ", time1))

    # run mplus files
    runModels(target=paste0(main_dir2, "/", folders[i], "/ULSMV_delta"))

    # message and meta data
    message(paste0("Finished analzying condition: ", folders[i]))
    time2 <- Sys.time()
    print(paste0("time2: ", time2))
    message("Total duration for the condition: ")
    print(time2-time1)
  }
  end_time <- time2
  message("Total duration: ")
  print(end_time - start_time)
  }


