#' update_feeder_data
#' @export

update_feeder_data <- function(year){
  feeder_updates <- readRDS("data/feeder_updates.rds")

  PABU_sites <- rdrop2::drop_dir("RFID_data/PABU")$name
  LAZB_sites <- rdrop2::drop_dir("RFID_data/LAZB")$name
  BCCH_sites <-rdrop2::drop_dir("RFID_data/BCCH")$name

  PABU_update <- 0
  LAZB_update <- 0

  ## For PABU
  for(i in 1:length(PABU_sites)){
    feeders <- rdrop2::drop_dir(paste0("RFID_Data/PABU/", PABU_sites[i]))$name

    for(f in 1:length(feeders)){
      last_update <- as.POSIXct(substr(gsub("T", " ", rdrop2::drop_get_metadata(paste0("RFID_data/PABU/", PABU_sites[i], "/", feeders[f], "/", year, "_DATALOG.TXT"))$client_modified),
                               1, 19), tz = "GMT")

      if(last_update == feeder_updates$last_update[feeder_updates$Feeder == feeders[f]]){
        print(paste(feeders[f], "up to date", sep = " "))
        PABU_update <- max(0, PABU_update)
      }else{
        print(paste("Updating data for", feeders[f], sep = " "))
        PABU_update <- max(1, PABU_update)
        feeder_updates$last_update[feeder_updates$Feeder == feeders[f]] <- last_update

        rdrop2::drop_download(paste0("RFID_data/PABU/", PABU_sites[i], "/", feeders[f], "/", year, "_DATALOG.txt"), local_path = paste0("data-raw/", PABU_sites[i], "/", feeders[f]), overwrite = TRUE)

        rfid <- readLines(paste0("data-raw/", PABU_sites[i], "/", feeders[f], "/", year, "_DATALOG.txt"))
        if(length(which(rfid == ""|rfid == " ")) != 0){
          rfid <- rfid[-which(rfid == ""|rfid == " ")]
        }else{
          rfid[length(rfid)] <- paste0(rfid[length(rfid)], rfid[1])
          rfid <- rfid[-1]
        }

        ## Read in as dataframe, add info
        rfid_df <- read.table(textConnection(rfid),sep = ",")
        names(rfid_df) <- c("RFID", "DateTime", "Feeder", "Antenna")
        rfid_df <- dplyr::mutate(rfid_df, Site = PABU_sites[i])
        rfid_df <- tidyr::separate(data = rfid_df,col = DateTime,into=c("Date", "Time"), sep = " ")

        ## write converted data as .csv

        write.csv(rfid_df, file = paste0("data-raw/", PABU_sites[i], "/", feeders[f], "/", year, "_DATALOG.csv"), row.names = FALSE)
      }
    }
  }

  if(PABU_update == 1){
    for(i in 1:length(PABU_sites)){
      feeders <- rdrop2::drop_dir(paste0("RFID_Data/PABU/", PABU_sites[i]))$name

      for(f in 1:length(feeders)){
        rfid <- read.csv(paste0("data-raw/", PABU_sites[i], "/", feeders[f], "/", year, "_DATALOG.csv"))

        if(i == 1 & f == 1){
          rfid_df <- rfid
        }else{
          rfid_df <- dplyr::bind_rows(rfid_df, rfid)
        }
      }
    }

    #### Convert Date and time into correct format
    rfid_df <- dplyr::mutate(rfid_df, Date = lubridate::mdy(rfid_df$Date), Time = lubridate::hms(rfid_df$Time))

    #### Add week and month of each "visit"
    rfid_df <- dplyr::mutate(rfid_df, Week = lubridate::week(rfid_df$Date),
                      Month = lubridate::month(rfid_df$Date))

    #### Remove observer checks
    obs_rfid <- read.csv("data-raw/RFID_observers.csv")

    obs_checks <- which(rfid_df$RFID %in% obs_rfid$ID)

    pabu_checks <- rfid_df[obs_checks,]
    saveRDS(pabu_checks, paste0("data/PABU/", year, "_obs_checks.rds"))

    rfid_df <- rfid_df[-obs_checks,]

    #### Save PABU data ----

    saveRDS(rfid_df, paste0("data/PABU/", year, "_RFID.rds"))
    write.csv(rfid_df, paste0("data/PABU/", year, "_RFID.csv"), row.names = FALSE)
  }

  ## For LAZB
  for(i in 1:length(LAZB_sites)){
    feeders <- rdrop2::drop_dir(paste0("RFID_Data/LAZB/", LAZB_sites[i]))$name

    for(f in 1:length(feeders)){
      last_update <- as.POSIXct(substr(gsub("T", " ", rdrop2::drop_get_metadata(paste0("RFID_data/LAZB/", LAZB_sites[i], "/", feeders[f], "/", year, "_DATALOG.TXT"))$client_modified),
                                       1, 19), tz = "GMT")

      if(last_update == feeder_updates$last_update[feeder_updates$Feeder == feeders[f]]){
        print(paste(feeders[f], "up to date", sep = " "))
        LAZB_update <- max(0, LAZB_update)
      }else{
        print(paste("Updating data for", feeders[f], sep = " "))
        LAZB_update <- max(1, LAZB_update)

        feeder_updates$last_update[feeder_updates$Feeder == feeders[f]] <- last_update

        rdrop2::drop_download(paste0("RFID_data/LAZB/", LAZB_sites[i], "/", feeders[f], "/", year, "_DATALOG.txt"), local_path = paste0("data-raw/", LAZB_sites[i], "/", feeders[f]), overwrite = TRUE)

        rfid <- readLines(paste0("data-raw/", LAZB_sites[i], "/", feeders[f], "/", year, "_DATALOG.txt"))
        if(length(which(rfid == ""|rfid == " ")) != 0){
          rfid <- rfid[-which(rfid == ""|rfid == " ")]
        }else{
          rfid[length(rfid)] <- paste0(rfid[length(rfid)], rfid[1])
          rfid <- rfid[-1]
        }

        ## Read in as dataframe, add info
        rfid_df <- read.table(textConnection(rfid),sep = ",")
        names(rfid_df) <- c("RFID", "DateTime", "Feeder", "Antenna")
        rfid_df <- dplyr::mutate(rfid_df, Site = LAZB_sites[i])
        rfid_df <- tidyr::separate(data = rfid_df,col = DateTime,into=c("Date", "Time"), sep = " ")

        ## write converted data as .csv

        write.csv(rfid_df, file = paste0("data-raw/", LAZB_sites[i], "/", feeders[f], "/", year, "_DATALOG.csv"), row.names = FALSE)

      }
    }
  }

  if(LAZB_update == 1){
    for(i in 1:length(LAZB_sites)){
      feeders <- rdrop2::drop_dir(paste0("RFID_Data/LAZB/", LAZB_sites[i]))$name

      for(f in 1:length(feeders)){
        rfid <- read.csv(paste0("data-raw/", LAZB_sites[i], "/", feeders[f], "/", year, "_DATALOG.csv"))

        if(i == 1 & f == 1){
          rfid_df <- rfid
        }else{
          rfid_df <- dplyr::bind_rows(rfid_df, rfid)
        }
      }
    }

    #### Convert Date and time into correct format
    rfid_df <- dplyr::mutate(rfid_df, Date = lubridate::mdy(rfid_df$Date), Time = lubridate::hms(rfid_df$Time))

    #### Add week and month of each "visit"
    rfid_df <- dplyr::mutate(rfid_df, Week = lubridate::week(rfid_df$Date),
                      Month = lubridate::month(rfid_df$Date))

    #### Remove observer checks
    obs_rfid <- read.csv("data-raw/RFID_observers.csv")

    obs_checks <- which(rfid_df$RFID %in% obs_rfid$ID)

    LAZB_checks <- rfid_df[obs_checks,]
    saveRDS(lazb_checks, paste0("data/LAZB/", year, "_obs_checks.rds"))

    rfid_df <- rfid_df[-obs_checks,]

    #### Save LAZB data ----

    saveRDS(rfid_df, paste0("data/LAZB/", year, "_RFID.rds"))
    write.csv(rfid_df, paste0("data/LAZB/", year, "_RFID.csv"), row.names = FALSE)
  }

  saveRDS(feeder_updates, 'data/feeder_updates.rds')
}
