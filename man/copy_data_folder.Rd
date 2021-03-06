% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy_data_folder.R
\name{copy_data_folder}
\alias{copy_data_folder}
\title{A Function to Copy the Data Folder}
\usage{
copy_data_folder(source_main_dir, dest_main_dir)
}
\arguments{
\item{source_main_dir}{It is a main directory, where the study condition folders,
including data files, are located. This main directory is to be copied.}

\item{dest_main_dir}{It is a main destination directory, where the study
condition folders, including data files, will be located.}
}
\description{
In a data folder, there are sub folders, which represent study conditions.
In the sub folders, there may be folders for estimators. This function allows
one to copy just the data sets and associated files with the extentions "inp",
"Rdata", and "csv". This function is useful when you want to copy the data sets.
It uses the "folder.Rdata" (study condition folders), which are created in
a data generation stage.
}
\examples{
library(AUTTT)

source_main_dir <- "C:/Users/shh6304/Desktop/test3"
# dest_main_dir does not exist and will be created.
# Folders and files in "test3" will be cloned.
dest_main_dir <- "C:/Users/shh6304/OneDrive - Western Michigan University/test3"

copy_data_folder(source_main_dir = "C:/Users/shh6304/Desktop/test3",
                 dest_main_dir = "C:/Users/shh6304/OneDrive - Western Michigan University/test3")
}
