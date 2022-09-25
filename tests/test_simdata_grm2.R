# Author: Bo Klauth

library(AUTTT)

# Loading theta values
# Set main directory for theta
main_dir <- "G:/My Drive/CLASSES/0000_Dissertation/2_data_gen_and_analysis"
setwd(main_dir)
getwd()
# MVN 300
load(paste0(getwd(), "/output/thetas", "/thetas_mvn_values300.RData"))
theta1 <- theta_mvn1
# MVN 600
load(paste0(getwd(), "/output/thetas", "/thetas_mvn_values600.RData"))
theta2 <- theta_mvn2
# MVN 1200
load(paste0(getwd(), "/output/thetas", "/thetas_mvn_values1200.RData"))
theta3 <- theta_mvn3
# LC negatively skewed 300
load(paste0(getwd(), "/output/thetas", "/thetas_neg_skewLC300.RData"))
theta4 <- scaled_output10

# LC negatively skewed 600
load(paste0(getwd(), "/output/thetas", "/thetas_neg_skewLC600.RData"))
theta5 <- scaled_output11

# LC negatively skewed 1200
load(paste0(getwd(), "/output/thetas", "/thetas_neg_skewLC1200.RData"))
theta6 <- scaled_output12

# MNS was previously named NS.
theta_label <- c('MVN', 'MVN', 'MVN', 'MNS', 'MNS', 'MNS')

# model ####
model1 = list(seq(1,7, by=1),
              seq(8,14, by=1),
              seq(15,21, by=1))

# total number of items
nvar <- length(unlist(model1))
# Load factor loadings ####

load(paste0(main_dir, "/output/itempara/loadings1.Rdata"))# low
load(paste0(main_dir, "/output/itempara/loadings2.Rdata"))# high
loading_label <- c('LO', "HI")

# Loading thresholds  ####
load(paste0(main_dir, "/output/itempara/thresholds_mat1.Rdata")) # normal
load(paste0(main_dir, "/output/itempara/thresholds_mat2.Rdata")) # moderately skewed
threshold_label <- c('NORM', 'MSKE')



# create a new director to store raw data and Mplu script ####


# unlink(x="C:/Users/shh6304/Desktop/t7", recursive = TRUE)

# The best location is 'Desktop'.
# Mplus allows only upto 90 characters for directory.
# Create a director
main_dir2 <- "C:/Users/BK/Desktop/test9"
if(dir.exists(main_dir2)){
  setwd(main_dir2)
}else {
  dir.create(main_dir2)
  setwd(main_dir2)
}

for (code_chunk in 1:1){
nReps <- 5 # replications
theta_index <- 1
M_index <- 1
L_index <- 1
T_index <- 1
P_index <- 0

folder_list <- NULL
for (theta_index in 1:6){ # theta distributions
  for (M_index in 1:1){ # model index
    for (L_index in 1:2){ # loading index
      for (T_index in 1:2){ # threshold index
        # start content

        # 1-set up variables ####
        P_index <- P_index + 1 # numbering cell/condition
        model <- get(paste0("model", M_index)) # model
        ncat <- ncol(get(paste0("thresholds_mat", T_index)))+1 # n. of categories

        factor_loadings <- get(paste0("loadings", L_index)) # was M_index
        message("Factor loading level: ")
        message(paste0("loadings", L_index))
        print(factor_loadings)

        theta_val <- get(paste0("theta", theta_index))
        sample_size <- nrow(theta_val)
        thresholds_mat <- get(paste0("thresholds_mat", T_index))
        # Convert to IRT parameters
        ## A vector of item discrimination ####
        a_vec <- factor_loadings/sqrt(1-factor_loadings^2)

        ## intercept parameters (DeMars, 2012) ####
        d <- thresholds_mat/sqrt(1-factor_loadings^2)


        ## Naming a folder ####
        data_folder <- paste0("C", P_index, "_",
                              "N", sample_size, "_", # sample size
                              theta_label[theta_index], "_", # theta distribution
                              # "F", length(model),"_", # dimensionality
                              # "Length", length(unlist(model)), "_", # n of items
                              "L", loading_label[L_index],"_",
                              # "C", ncat,"_",
                              "I", threshold_label[T_index]
        )

        folder_list <- rbind(folder_list, data_folder)


        # message: creating folder
        setwd(main_dir2)
        message(paste0("creating a folder: ",data_folder))
        # creating folder if not existed
        if(isFALSE(dir.exists(paste0(getwd(), "/", data_folder)))){
          dir.create(paste0(getwd(), "/", data_folder))
          # message: created: data_folder
          message(paste0("created: ", data_folder))
          # message: set directory for storing data
          message(paste0("set working directory to: ", getwd(), "/", data_folder))
          # set working directory for replicated data sets
          setwd(paste0(getwd(), "/", data_folder))

        } else {
          # set working directory for replicated data sets
          setwd(paste0(getwd(), "/", data_folder))
        }


        # 2-Simulate raw data and scripts####
        ## 2.1 Generate raw data: using the function ####
        message(paste0("Creating ", nReps, " data sets for the condition."))

      study_cell <- simdata_grm(model = model,
                                 theta_matrix = theta_val,
                                 a = a_vec,
                                 d = d,
                                 N = NULL,
                                 R = nReps,
                                 method = "U",
                                 file_dir = getwd(),
                                 file_prefix = "bk",
                                 seed_num = 12345)


        # 2.2 Save model results
        message(paste0("Saving data model at: ", getwd()))
        save(study_cell, file = paste0(getwd(), "/study_cell.Rdata"))

        ## 2.3 Output data validation ####
        # study_cell$res_prob
        print(study_cell$avg_res_prob)
        write.csv(t(study_cell$avg_res_prob),
                  file= paste0(getwd(),"/avg_response_probabilities.csv"),
                  row.names = FALSE)

        write.csv(study_cell$res_prob,
                  file= paste0(getwd(),"/response_probabilities.csv"),
                  row.names = FALSE)
        ## 2.4 message: done with R replications with the condition
        message(paste0("Done with C", P_index, ": replication ", nReps))


      }
    }
  }
}

expected_cells <- theta_index*M_index*L_index*T_index
if(P_index == expected_cells){
  message("Finished data simulation without error.")
  message("Finish Mplus script writing without error.")
} else {
  message("Failed to simulate data for all conditions.")
  message("Failed to write Mplus scripts.")
}
# folders
folders <- folder_list

main_dir2
save(folders, file = paste0(main_dir2, "/folders.Rdata"))
} # End code chuck for generating data



