#'Gather Mplus Output in One Table
#'
#' The function gather standardized parameter estimates produced by Mplus.
#' It reads the file with the extention ".out".
#' @param main_dir This is the main directory in which folders representing
#' study conditions and data sets for the conditions are stored.
#' @param cell_folders This object contains the names of the the folders representing
#' the experimental conditions/cells.
#' @param est_folders It is the folder name(s), where Mplus scripts are created and
#' stored using the function mplus_montecarlo_analysis_grm2().
#' @param nreps It is the total of replications for a conditions/cell.
#' @param cell_prefix It is prefix given to an R object. The function will add
#' a number after it to make an object name for a study condition (e.g., U1).
#' @param methods There are three options: "read", "gather", and "all". If it
#' is "read", the function only reads the Mplus output files. If "gather", the
#' function will gather standardized parameter estimates by assuming that "read"
#' method has been executed previously and the R objects are in the Global
#' Environment. If "all", the function will first read the results from Mplus
#' output files and gather the standardized parameter estimates.
#' @return It will return a table with all standardized parameter estimates
#' obtained by a particular estimator for all replications and conditions.
#'
#' @export
#' @examples
#' library(AUTTT)
#'
#'	main_dir2 <- "C:/Users/shh6304/Desktop/test"
#'	load(paste0(main_dir2, "/folders.Rdata"))
#'	cell_folders <- folders
#'	est_folder <- c("ULSMV_delta")
#'	cell_prefix <- "U"
#'
#'	ulsmv_est <- gather_mplus_output(main_dir = main_dir2,
#'	                                 cell_folders = cell_folders,
#'	                                 est_folders = "ULSMV_delta",
#'	                                 nreps = 5,
#'	                                 cell_prefix = "U",
#'	                                 methods = "gather")

gather_mplus_output <- function(main_dir,
                                cell_folders,
                                est_folder,
                                cell_prefix,
                                nreps,
                                methods){
  # Declare variables

parms_allreps <- NULL # for storing all parameters for all estimators
NFOLDERS <- length(cell_folders)
MAXR <- nreps
if(methods == "read"){
  # 1-Read Mplus output for all cells for ONE estimator to R ####
  for(est_index in 1:length(est_folder)){
    for(cfolder_index in 1:length(cell_folders)){
      cell_name <- paste0(cell_prefix, cfolder_index)
      message("Reading output in cell name: ", cell_name)
      message("Start Time: ", Sys.time())
      assign(cell_name,
             readModels(paste0(main_dir2, "/", cell_folders[cfolder_index], "/", est_folder[est_index]),
                        recursive=FALSE, what = "all", quiet = FALSE))

      message("End Time: ", Sys.time())
    } # end iterations for cell folders
  } # end iterations for estimators
}

if(methods == "gather"){
# 2-read and organize output ####
for(cfolder_index in 1:NFOLDERS){
  for(R in 1:MAXR){
    # declare variables
    cell_id <- paste0("cell", cfolder_index)
    file_prefix <- tolower(est_folder[est_index])
    file_name <- paste0(file_prefix, "_rep", R, ".out")
    cell_name <- paste0(cell_prefix, cfolder_index)
    parms <- get(cell_name)[[file_name]][["parameters"]][["stdyx.standardized"]]
    folder = cell_folders[cfolder_index]
    cell = cfolder_index
    rep = R
    estimator = est_folder[est_index]
    parms_onerep <- cbind(folder, cell, rep, estimator, parms)
    parms_allreps <- rbind(parms_allreps, parms_onerep)

  } # end replication iteration
} # end folder iteration
}

if(methods=="all"){
  # 1-Read Mplus output for all cells for ONE estimator to R ####
  for(est_index in 1:length(est_folder)){
    for(cfolder_index in 1:length(cell_folders)){
      cell_name <- paste0(cell_prefix, cfolder_index)
      message("Reading output in cell name: ", cell_name)
      message("Start Time: ", Sys.time())
      assign(cell_name,
             readModels(paste0(main_dir2, "/", cell_folders[cfolder_index], "/", est_folder[est_index]),
                        recursive=FALSE, what = "all", quiet = FALSE))

      message("End Time: ", Sys.time())
    } # end iterations for cell folders
  } # end iterations for estimators

  # 2-read and organize output ####
  for(cfolder_index in 1:NFOLDERS){
    for(R in 1:MAXR){
      # declare variables
      cell_id <- paste0("cell", cfolder_index)
      file_prefix <- tolower(est_folder[est_index])
      file_name <- paste0(file_prefix, "_rep", R, ".out")
      cell_name <- paste0(cell_prefix, cfolder_index)
      parms <- get(cell_name)[[file_name]][["parameters"]][["stdyx.standardized"]]
      folder = cell_folders[cfolder_index]
      cell = cfolder_index
      rep = R
      estimator = est_folder[est_index]
      parms_onerep <- cbind(folder, cell, rep, estimator, parms)
      parms_allreps <- rbind(parms_allreps, parms_onerep)

    } # end replication iteration
  } # end folder iteration


}

if(nrow(parms_allreps) == nrow(parms_onerep)*MAXR*NFOLDERS){
  message("The number of observations is correct.")
} else {
  print("The number of observations is not equal for all cells.")
}
return(parms_allreps)
}


