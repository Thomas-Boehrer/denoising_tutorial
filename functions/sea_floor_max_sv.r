# Thomas Boehrer
# line-picking algorithm for seafloor detection
# maxSv - replicate from Echoview
# https://support.echoview.com/WebHelp/Reference/Algorithms/Line_picking_algorithm.htm#Maximum_Sv

sea_floor_max_sv <- function(
    data, seaFloor_art_min_good_sv = 0.95,
    seaFloor_discrim_level = 0.6,
    seaFloor_end_depth = abs(as.numeric(depthLookupTable[dim(depthLookupTable)[1], 1])),
    seaFloor_start_depth = -40){

    data <- data |> 
      dplyr::select(
        c(timeBin, dateTime, depth, biomassScore)
      ) |>
      dplyr::ungroup()
  # convert xyz into wide matrix to allow for faster processing
  acousticsMatrix <- data |> 
    dplyr::select(timeBin, depth, biomassScore) |> 
    tidyr::pivot_wider(names_from = timeBin, values_from = biomassScore, values_fill = 0) |> 
    dplyr::select(-depth) |> 
    as.matrix()
  #removes NAs (otherwise index cannot be calculated)
  acousticsMatrix[is.na(acousticsMatrix)] <- 0
  #acoustics[is.na(acoustics)] <- 0
  
  # set up tibbles where results are stored (bottomLine) and helper table for matching depth-index with
  # actual depth
  bottomLine <- tibble::tibble(
    dplyr::distinct(
        data, timeBin),
    seaBottom = NA,
    detectionQuality = NA)
  depthLookupTable <- tibble::tibble(
    dplyr::distinct(data, depth)) |>
    dplyr::arrange(desc(depth)) |>
    dplyr::mutate(depthIndex = dplyr::row_number()) |>
    na.omit(depthLookupTable)
  # calculates the smallest Index of the searched ping
  indMin <- depthLookupTable$depthIndex[
    which(
        abs(depthLookupTable$depth - (seaFloor_start_depth)) ==
        min(abs(depthLookupTable$depth - (seaFloor_start_depth)))
    )]


  # start algorithm. The initial conditional ("if") allows for flexible good fits for sea floor detection.
  # This is because some data sets include different intensities in biomass scores  
  for (i in 1 : nrow(bottomLine)){ # nolint

    # Index of the max ping value 
    indMax <- which.max(acousticsMatrix[,i])
    # if index is within a defined depth the conditional statement takes the highest value from below 
    if(indMax <= indMin){
      helperFrame <- tibble::tibble(
        ind = 1 : dim(acousticsMatrix)[1], # nolint
        score = acousticsMatrix[,i]) |>
        dplyr::arrange(desc(score))

      for (n in 1 : dim(helperFrame)[1]){ # nolint
        if(helperFrame$ind[n] > indMin)
          break
      }
      indMax <- helperFrame$ind[n]
    }

    if (acousticsMatrix[indMax,i] - acousticsMatrix[indMax,i] * 0.001 >=
            seaFloor_art_min_good_sv){
      minimumGoodSv <- 
        as.numeric(
            acousticsMatrix[indMax, i] - acousticsMatrix[indMax, i] * 0.001)
    } else {
      minimumGoodSv <- seaFloor_art_min_good_sv
    }
    
    
    # define search interval from highest value to the surface/minimum seraching depth (indMin)
    searchInterval <- acousticsMatrix[indMax:indMin, i]
    
    if (length(which(searchInterval < seaFloor_discrim_level)) == 0){
      seaBottomInd <- indMax} else {
        seaBottomInd <- indMax - min(which(searchInterval < seaFloor_discrim_level)) + 1
      }
        
    # store the information in the predefined tibble
    bottomLine$seaBottom[i] <- depthLookupTable$depth[depthLookupTable$depthIndex == seaBottomInd]
    # definitions for a good fit 
    if(acousticsMatrix[indMax, i] > minimumGoodSv){
      bottomLine$detectionQuality[i] <- 'good'
    } else {
      bottomLine$detectionQuality[i] <- 'bad'
    }
    if(bottomLine$detectionQuality[i] == 'bad'){
      bottomLine$seaBottom[i] <- 
        as.numeric(
            depthLookupTable[dim(depthLookupTable)[1], 1]
        )
    }
  }
  

  return(
    dplyr::left_join(
        data, bottomLine, by = "timeBin"
    )
    )
}