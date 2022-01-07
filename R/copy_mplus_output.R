#' A Function to Copy the Data Folder
#'
#' In a data folder, there are sub folders, which represent study conditions.
#' In the sub folders, there may be folders for estimators. This function allows
#' one to copy just the output files with the extensions "out" and "dat"
#' and paste another location but preserving the folder structure.
#' It uses the "folder.Rdata" (study condition folders), which are created in
#' a data generation stage.
#'
#' @param source_main_dir It is a main directory, where the study condition folders,
#' including data files, are located. This main directory is to be copied.
#' @param dest_main_dir It is a main destination directory, where the study
#' condition folders, including data files, will be located.
#' @param output_folder This is the folder where the output files
#' (".out", ".dat") are stored. The folder represents an estimator.
#'
#' @export
#' @examples
#'	library(AUTTT)
#'
#' source_main_dir <- "C:/Users/shh6304/Desktop/test3"
#' # dest_main_dir does not exist and will be created.
#' # Folders and files in "test3" will be cloned.
#' dest_main_dir <- "C:/Users/shh6304/OneDrive - Western Michigan University/test3"
#'
#' copy_mplus_output(source_main_dir = "C:/Users/shh6304/Desktop/test3",
#'                  dest_main_dir = "C:/Users/shh6304/OneDrive - Western Michigan University/test3",
#'                  output_folder = "ML_probit")


copy_mplus_output <- function(source_main_dir,
                              dest_main_dir,
                              output_folder){
  # load folders
  load(paste0(source_main_dir, "/folders.Rdata"))
# create destination main folder if not exist
if(!dir.exists(dest_main_dir)){
  dir.create(dest_main_dir)
}

  # create dest. condition folders conditional on existence of source condition folders
    for (i in 1:length(folders)){
      if(dir.exists(paste0(source_main_dir, "/", folders[i])) & !dir.exists(paste0(dest_main_dir, "/", folders[i]))){
        dir.create(paste0(dest_main_dir, "/", folders[i]))
      }
    }



est_folder <- output_folder

for(i in 1:length(folders)){
  source_subdir <- paste0(source_main_dir, "/", folders[i], "/", est_folder)
  message(paste0("Source directory: ", source_subdir))

  dest_subdir <- paste0(dest_main_dir, "/", folders[i], "/", est_folder)
  message(paste0("Destination directory: ", dest_subdir))

  # conditional copying
  if(dir.exists(source_subdir) & !dir.exists(dest_subdir)){
    message("Creating destination directory")
    dir.create(dest_subdir)
  }

  dataFiles_out <- dir(source_subdir, "*.out", ignore.case = TRUE, all.files = TRUE)
  dataFiles_dat <- dir(source_subdir, "*.dat", ignore.case = TRUE, all.files = TRUE)
  # copy files
  message("Copying files")
  file.copy(file.path(source_subdir, dataFiles_out), dest_subdir, overwrite = TRUE)
  file.copy(file.path(source_subdir, dataFiles_dat), dest_subdir, overwrite = TRUE)
  # Done status for the folder
  message(paste0("Done for ", folders[i]), "/", est_folder)
  message(".....................")
}
} # end function
