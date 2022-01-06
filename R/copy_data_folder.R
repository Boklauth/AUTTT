#' A Function to Copy the Data Folder
#'
#' In a data folder, there are sub folders, which represent study conditions.
#' In the sub folders, there may be folders for estimators. This function allows
#' one to copy just the data sets and associated files with the extentions "inp",
#' "Rdata", and "csv". This function is useful when you want to copy the data sets.
#' It uses the "folder.Rdata" (study condition folders), which are created in
#' a data generation stage.
#'
#' @param source_main_dir It is a main directory, where the study condition folders,
#' including data files, are located. This main directory is to be copied.
#' @param dest_main_dir It is a main destination directory, where the study
#' condition folders, including data files, will be located.
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
#' copy_data_folder(source_main_dir = "C:/Users/shh6304/Desktop/test3",
#'                  dest_main_dir = "C:/Users/shh6304/OneDrive - Western Michigan University/test3")

copy_data_folder <- function(source_main_dir,
                             dest_main_dir){

  load(paste0(source_main_dir, "/folders.Rdata"))

  # create destination main folder if not exist
  if(!dir.exists(dest_main_dir)){
    dir.create(dest_main_dir)
  }

  i <- 1
  for(i in 1:length(folders)){
    # start time
    start_time <- Sys.time()


    source_subdir <- paste0(source_main_dir, "/", folders[i])
    message(paste0("Source directory: ", source_subdir))

    dest_subdir <- paste0(dest_main_dir, "/", folders[i])
    message(paste0("Destination directory: ", source_subdir))

    if(!dir.exists(dest_subdir)){
      message("Creating destination directory")
      dir.create(dest_subdir)
    }
    # in the main folder
    folders_Rdata <- dir(source_main_dir, "*.Rdata", ignore.case = TRUE, all.files = TRUE)
    # in the sub folders
    dataFiles_Rdata <- dir(source_subdir, "*.Rdata", ignore.case = TRUE, all.files = TRUE)
    dataFiles_dat <- dir(source_subdir, "*.dat", ignore.case = TRUE, all.files = TRUE)
    # copy files
    message("Copying files...")
    file.copy(file.path(source_main_dir, folders_Rdata), dest_main_dir, overwrite = TRUE)
    file.copy(file.path(source_subdir, dataFiles_Rdata), dest_subdir, overwrite = TRUE)
    file.copy(file.path(source_subdir, dataFiles_dat), dest_subdir, overwrite = TRUE)

    # Done status for the folder
    message(paste0("Done for ", folders[i]))
    # times
    # end time
    end_time <- Sys.time()
    message(paste0("Start time: ", start_time))
    message(paste0("End time: ", end_time))
    message("......................")
  }

  } # end function



