# Thomas Boehrer
# 25/02/2023
##########
# DESCRIPTION OF THE FUNCTION:
# This function aims to downsample acoustic data in two dimension: depth and time

downsampling_acoustics <- function(
    data, start_time, end_time, temp_resolution,
    depth_resolution, local_time_zone) {

    # the following statements return a tibble with 5 columns
    return(data |>
        dplyr::select(
          depth, dateTime, biomassScore,dataAboveGround
        ) |>
        # transform and round dateTime information
        dplyr::mutate(
          dateTime = as.POSIXct(dateTime)
        ) |>
        dplyr::mutate(
          dateTime = round.POSIXt(dateTime, units = 'min')
        ) |>
        # create the time bin and downsampling of the data
        dplyr::mutate(
            timeBin = cut(dateTime, breaks = temp_resolution, labels = F)
        ) |>
        dplyr::group_by(
            depth, timeBin
        ) |>
        dplyr::summarise(
            biomassScore = mean(biomassScore)
        ) |>
        dplyr::ungroup() |>
        # make depth negative and recreate dateTime 
        dplyr::mutate(
            depth = -depth, 
            dateTime = as.POSIXct(
                ((as.numeric(timeBin) - 1) / max(as.numeric(timeBin)) *
                as.numeric(difftime(end_time, start_time, units = 'secs')) + (-local_time_zone * 3600)),
                origin = start_time
            )
        ) |>
        # depthBin 
        dplyr::mutate(
            depthBin = plyr::round_any(depth, depth_resolution)
        ) |> 
        dplyr::group_by(
            dateTime, depthBin, timeBin
        ) |> 
        # downsample the data based on the selected depthBins
        dplyr::summarise(
            biomassScore = mean(biomassScore, na.rm = T)
        ) |>
        dplyr::mutate(
            depth = as.numeric(as.character(depthBin))
        ) |>
        dplyr::ungroup() |>
        dplyr::select(-depthBin)
    )
}