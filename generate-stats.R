#!/usr/bin/env Rscript
# Requires installation of R, at a minimum for apt use:
#     $sudo apt install r-base r-base-core r-recommended
# Version 0.1
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, data.table, dplyr, tidyverse, anytime, 
  stringr, loggit, gt, webshot
)
# webshot only installs if it's missing or old
suppressMessages(
 webshot::install_phantomjs(force=FALSE)
)

###################
# Local Functions #
###################

# Display stuff #
#################
.get_pretty_timestamp_diff <- function(
  start_timestamp,
  end_timestamp,
  seconds_decimal=2,
  round_simple=TRUE
){
  # Set defaults
  .days_decimal <- 0
  .days <- 0
  .hours_decimal <- 0
  .hours <- 0
  .minutes_decimal <- 0
  .minutes <- 0
  .seconds_display <- 0
  .seconds <- (end_timestamp-start_timestamp) 
  if (round_simple) {
    .years <- as.integer(.seconds / (365.24*24*60*60))
    if (.years > 0) {
      .years <-round(.seconds / (365.24*24*60*60),1)
      .seconds <- 0
    } else {
      .days <- as.integer((.seconds / (365.24*24*60*60)-.years)*365.24)
      if (.days > 0 ) {
        .days <-round((.seconds / (365.24*24*60*60)-.years)*365.24,1)
        .seconds <- 0
      } else {
        .days_decimal <-(.seconds / (365.24*24*60*60)-.years)*365.24-.days
        .hours <- as.integer(.days_decimal*24)
        if (.hours > 0) {
          .hours <- round(.days_decimal*24,1)
        }else{
          .hours_decimal <- .days_decimal*24 - .hours
          .minutes <- as.integer(.hours_decimal*60)
          .minutes_decimal <- .hours_decimal*60 - .minutes
          if (.minutes > 0) {
            .minutes <- round(.hours_decimal*60,1)
            .hours_decimal <- 0
            .seconds_display <- 0    
          } else {
            .seconds_display <-1 # round(.seconds,seconds_decimal)
          }
        }
      }
    }
  } else {
    .years <- as.integer(.seconds / (365.24*24*60*60))
    .days <- as.integer((.seconds / (365.24*24*60*60)-.years)*365.24)
    .days_decimal <-(.seconds / (365.24*24*60*60)-.years)*365.24-.days
    .hours <- as.integer(.days_decimal*24)
    .hours_decimal <- .days_decimal*24 - .hours
    .minutes <- as.integer(.hours_decimal*60)
    .minutes_decimal <- .hours_decimal*60 - .minutes
    .seconds_display <- round(.minutes_decimal*60,seconds_decimal)
  }
  .time_statement_list <- c(
    ifelse(as.integer(.years),
           ifelse((.years == 1)," year ",paste0(.years," years ")),
           NA
    ),
    ifelse(as.integer(.days),
           ifelse((.days == 1)," day ",paste0(.days," days ")),
           NA
    ),
    ifelse(as.integer(.hours),
           ifelse((.hours == 1)," hour ",paste0(.hours," hours ")),
           NA
    ),
    ifelse(as.integer(.minutes),
           ifelse((.minutes == 1)," minute ",paste0(.minutes," minutes ")),
           NA
    ),
    ifelse(as.integer(.seconds_display),
           ifelse(
             (.seconds_display == 1),
             " second ",
             paste0(.seconds_display," seconds ")
           ),
           NA
    )
  )
  .time_statement_list <- na.omit(.time_statement_list)
  ifelse(
    (length(.time_statement_list) <= 1),
    .time_statement_list[1],
    paste0(
      paste0(
        .time_statement_list[1:(length(.time_statement_list)-1)],
        collapse=""
      ),
      "and ",
      .time_statement_list[length(.time_statement_list)]
    )
  )
}