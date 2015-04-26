

#' Load a csv dataset with all columns designated as character vectors
#'
#' @param path Path to the csv data to be loaded
#' @return data.frame of loaded csv file
loaddata <- function(path){
    print(paste('loading data from', destfile, sep=' '))
    df <- read.csv(path, colClasses=rep("character", 160))
}


#' Scrub the data.frame to make it usable for machine learning
#'
#' * Change #DIV/0! no NAs
#' * Change empty strings no NA
#' * Convert features to appropriate numeric, integer, or factor types
#' * Drop fields with no data
#' * convert NAs to 0
#'
#' @param df dirty data.frame from the read.csv
#' @return clean pretty data.frame
scrub <- function(df){

    # Change "" to NA
    df[df == "#DIV/0!"] = NA
    df[df == ""] = NA
    df[is.na(df)] <- 0

    numerics <- c(
    "kurtosis_roll_belt",
    "kurtosis_picth_belt",
    # "kurtosis_yaw_belt",  # empty field
    "skewness_roll_belt",
    "skewness_roll_belt.1",
    # "skewness_yaw_belt", # empty field
    "max_yaw_belt",
    "min_yaw_belt",
    # "amplitude_yaw_belt",  # empty or zeros
    "kurtosis_roll_arm",
    "kurtosis_picth_arm",
    "kurtosis_yaw_arm",
    "skewness_roll_arm",
    "skewness_pitch_arm",
    "skewness_yaw_arm",
    "kurtosis_roll_dumbbell",
    "kurtosis_picth_dumbbell",
    # "kurtosis_yaw_dumbbell", # empty field
    "skewness_roll_dumbbell",
    "skewness_pitch_dumbbell",
    # "skewness_yaw_dumbbell", # empty field
    "max_yaw_dumbbell",
    "min_yaw_dumbbell",
    # "amplitude_yaw_dumbbell", # empty and zeros
    "kurtosis_roll_forearm",
    "kurtosis_picth_forearm",
    # "kurtosis_yaw_forearm", # empty field
    "skewness_roll_forearm",
    "skewness_pitch_forearm",
    # "skewness_yaw_forearm", # empty field
    "max_yaw_forearm",
    "min_yaw_forearm",
    # "amplitude_yaw_forearm", # empty and zeros
    "roll_belt",
    "pitch_belt",
    "yaw_belt",
    "max_roll_belt",
    "min_roll_belt",
    "amplitude_roll_belt",
    "var_total_accel_belt",
    "avg_roll_belt",
    "stddev_roll_belt",
    "var_roll_belt",
    "avg_pitch_belt",
    "stddev_pitch_belt",
    "var_pitch_belt",
    "avg_yaw_belt",
    "stddev_yaw_belt",
    "var_yaw_belt",
    "gyros_belt_x",
    "gyros_belt_y",
    "gyros_belt_z",
    "roll_arm",
    "pitch_arm",
    "yaw_arm",
    "var_accel_arm",
    "avg_roll_arm",
    "stddev_roll_arm",
    "var_roll_arm",
    "avg_pitch_arm",
    "stddev_pitch_arm",
    "var_pitch_arm",
    "avg_yaw_arm",
    "stddev_yaw_arm",
    "var_yaw_arm",
    "gyros_arm_x",
    "gyros_arm_y",
    "gyros_arm_z",
    "max_roll_arm",
    "max_picth_arm",
    "min_roll_arm",
    "min_pitch_arm",
    "amplitude_roll_arm",
    "amplitude_pitch_arm",
    "roll_dumbbell",
    "pitch_dumbbell",
    "yaw_dumbbell",
    "max_roll_dumbbell",
    "max_picth_dumbbell",
    "min_roll_dumbbell",
    "min_pitch_dumbbell",
    "amplitude_roll_dumbbell",
    "amplitude_pitch_dumbbell",
    "var_accel_dumbbell",
    "avg_roll_dumbbell",
    "stddev_roll_dumbbell",
    "var_roll_dumbbell",
    "avg_pitch_dumbbell",
    "stddev_pitch_dumbbell",
    "var_pitch_dumbbell",
    "avg_yaw_dumbbell",
    "stddev_yaw_dumbbell",
    "var_yaw_dumbbell",
    "gyros_dumbbell_x",
    "gyros_dumbbell_y",
    "gyros_dumbbell_z",
    "magnet_dumbbell_z",
    "roll_forearm",
    "pitch_forearm",
    "yaw_forearm",
    "max_roll_forearm",
    "max_picth_forearm",
    "min_roll_forearm",
    "min_pitch_forearm",
    "amplitude_roll_forearm",
    "amplitude_pitch_forearm",
    "var_accel_forearm",
    "avg_roll_forearm",
    "stddev_roll_forearm",
    "var_roll_forearm",
    "avg_pitch_forearm",
    "stddev_pitch_forearm",
    "var_pitch_forearm",
    "avg_yaw_forearm",
    "stddev_yaw_forearm",
    "var_yaw_forearm",
    "gyros_forearm_x",
    "gyros_forearm_y",
    "gyros_forearm_z",
    "magnet_forearm_y",
    "magnet_forearm_z"
    )

    if ("classe" %in% names(df)) {
        processed <- data.frame(classe=df$classe)
    } else {
        processed <- NULL
    }
    for (idx in 1:length(numerics)) {
        if (is.null(processed)) {
            processed <- data.frame(x=as.numeric(df[, numerics[idx]]))
            names(processed) <- numerics[idx]
        } else {
            processed[, numerics[idx]] <- as.numeric(df[, numerics[idx]])
        }
    }

    integers <- c(
    # "raw_timestamp_part_1", # timestamp
    # "raw_timestamp_part_2", # timestamp
    "num_window",
    "total_accel_belt",
    "max_picth_belt",
    "min_pitch_belt",
    "amplitude_pitch_belt",
    "accel_belt_x",
    "accel_belt_y",
    "accel_belt_z",
    "magnet_belt_x",
    "magnet_belt_y",
    "magnet_belt_z",
    "total_accel_arm",
    "accel_arm_x",
    "accel_arm_y",
    "accel_arm_z",
    "magnet_arm_x",
    "magnet_arm_y",
    "magnet_arm_z",
    "max_yaw_arm",
    "min_yaw_arm",
    "amplitude_yaw_arm",
    "total_accel_dumbbell",
    "accel_dumbbell_x",
    "accel_dumbbell_y",
    "accel_dumbbell_z",
    "magnet_dumbbell_x",
    "magnet_dumbbell_y",
    "total_accel_forearm",
    "accel_forearm_x",
    "accel_forearm_y",
    "accel_forearm_z",
    "magnet_forearm_x"
    )

    for (idx in 1:length(integers)) {
      processed[, integers[idx]] <- as.integer(df[, integers[idx]])
    }

    # No factor variables
    # factors <- c(
    # # "new_window"
    # )
    # for (idx in 1:length(factors)) {
    #   processed[, factors[idx]] <- as.factor(df[, factors[idx]])
    # }

    processed[is.na(processed)] <- 0
    processed
}

#' Split a data.frame into training, test, and validation sets by adding
#' a "set" feature.
#'
#' @param df data.frame to split into train/test/validate
#' @return original data.frame with additional "set" feature
splitdataset <- function(df){
    cuts <- floor(runif(nrow(df)) * 10)
    splits <- rep(0, length(cuts))
    splits[cuts < 4] <- 1
    splits[cuts < 2] <- 2
    split_table <- rbind(
        table(splits),
        round(table(splits) / sum(table(splits)) * 100))

    colnames(split_table) <- c("training", "testing", "validation")
    rownames(split_table) <- c("count", "percent")

    list(x=splits, table=split_table)
}




# scols <- sort(names(processed))
# normed <- data.frame(classe=processed$classe)
# for (idx in 1:ncol(processed)) {
#   cname <- scols[idx]
#   print(cname)
#   if (class(processed[, cname]) == "factor") {
#     normed[, cname] <- processed[, cname]
#   } else {
#     tmp <- processed[, cname]
#     normed[, cname] <- (tmp - mean(tmp, na.rm=TRUE)) / sd(tmp, na.rm=TRUE)
#   }
#   # print(c(names(processed)[idx], class(processed[, idx])))
# }

# # normed <- data.frame(classe=processed$classe)
# # for (idx in 1:length(numerics)) {
# #     tmp <- processed[, numerics[idx]]
# #     normed[, numerics[idx]] <- (tmp - mean(tmp, na.rm=TRUE)) / sd(tmp, na.rm=TRUE)
# # }
# # for (idx in 1:length(integers)) {
# #     tmp <- processed[, integers[idx]]
# #     normed[, integers[idx]] <- (tmp - mean(tmp, na.rm=TRUE)) / sd(tmp, na.rm=TRUE)
# # }


# dropped <- c()
# for (idx in 1:ncol(df)) {
#   cname <- names(df)[idx]
#   if (!(cname %in% names(processed))) {
#     dropped <- c(dropped, cname)
#   }
# }
# print("Fields dropped")
# print(paste(dropped, collapse=", "))