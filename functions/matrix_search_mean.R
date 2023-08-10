# test for the matrix search 

# This Script iterates through the acoustic backscattering signal of a dataset

matrix_search <- function(
  data, 
  noiseMax = noiseMax) {


  rangeNum <- length(unique(data$depth))
  pingNum <- length(unique(data$timeBin))

  # Set Up
  acousticsMatrix <- data |> 
  dplyr::select(c(depth, timeBin, powerCal)) |>
  tidyr::pivot_wider(names_from = timeBin, values_from = powerCal, values_fill = 0) |>
  dplyr::select(-depth) |>
  as.matrix()
  # remove attributes from the matrix
  attr(acousticsMatrix, "dimnames") <- NULL
  #
  noiseEst <- matrix(data = NA, nrow = nrow(acousticsMatrix), ncol = ncol(acousticsMatrix))
  attr(noiseEst, "dimnames") <- NULL

  ##########
  # Matrix Search
  # search through the matrix 
  for (i in 1 : ncol(acousticsMatrix)){
    if(i <= pingNum/2){
      for (j in seq(from = 1, by = rangeNum, to = nrow(acousticsMatrix))){
        if(j <= nrow(acousticsMatrix) - (rangeNum-1)){
          powerCalAvTemp <-
            10 * log10(mean(acousticsMatrix[j:(j+rangeNum-1), 1:pingNum]))
          noiseEst[j:(j+rangeNum-1), 1:(pingNum/2)] <-
            min(powerCalAvTemp)
        }else{
          powerCalAvTemp <-
            10 * log10(mean(acousticsMatrix[(nrow(acousticsMatrix)-rangeNum+1) : nrow(acousticsMatrix), 1:pingNum]))
          noiseEst[(seq(from = 1, by = rangeNum,
                        to = nrow(acousticsMatrix))[length(seq(from = 1, by = rangeNum,
                                                               to = nrow(acousticsMatrix)))]):(nrow(acousticsMatrix)),
                   1:(pingNum/2)] <-
            min(powerCalAvTemp)
        }
      }
    }
    if(i > ncol(acousticsMatrix)-pingNum/2){
      for (j in seq(from = 1, by = rangeNum, to = nrow(acousticsMatrix))){
        if(j <= nrow(acousticsMatrix) - (rangeNum-1)){
          powerCalAvTemp <-
            10 * log10(mean(acousticsMatrix[j:(j+rangeNum-1),
                                                (ncol(acousticsMatrix)-pingNum/2+1) : ncol(acousticsMatrix)]))
          noiseEst[j:(j+rangeNum-1), (ncol(acousticsMatrix)-pingNum/2+1) :ncol(acousticsMatrix)] <-
            min(powerCalAvTemp)
        }else{
          powerCalAvTemp <-
            10 * log10(mean(acousticsMatrix[seq(from = 1, by = rangeNum,
                                                    to = nrow(acousticsMatrix))[length(seq(from = 1, by = rangeNum,
                                                                                           to = nrow(acousticsMatrix)))]:nrow(acousticsMatrix),
                                                (ncol(acousticsMatrix)-pingNum/2+1):ncol(acousticsMatrix)]))
          noiseEst[seq(from = 1, by = rangeNum,
                       to = nrow(acousticsMatrix))[length(seq(from = 1, by = rangeNum,
                                                              to = nrow(acousticsMatrix)))]:nrow(acousticsMatrix),
                   (ncol(acousticsMatrix)-pingNum/2+1):ncol(acousticsMatrix)] <- min(powerCalAvTemp)
        }
      }
    }
    if(i %in% seq(from = (pingNum/2+1), to = ncol(acousticsMatrix)-pingNum/2)){
      for (j in seq(from = 1, by = rangeNum, to = nrow(acousticsMatrix))){
        if(j <= nrow(acousticsMatrix) - (rangeNum-1)){
          powerCalAvTemp <-
            10 * log10(mean(acousticsMatrix[j:(j+rangeNum-1),
                                                (i-pingNum/2):(i+rangeNum-1)]))
          noiseEst[j:(j+rangeNum-1), i] <- min(powerCalAvTemp) # (i-pingNum/2):(i+rangeNum-1) instead of i
        }else{
          powerCalAvTemp <-
            10 * log10(mean(acousticsMatrix[(nrow(acousticsMatrix)-rangeNum+1) : nrow(acousticsMatrix),
                                                (i-pingNum/2):(i+rangeNum-1)]))
          noiseEst[(seq(from = 1, by = rangeNum,
                        to = nrow(acousticsMatrix))[length(seq(from = 1, by = rangeNum,
                                                               to = nrow(acousticsMatrix)))]):nrow(acousticsMatrix), i] <-
            min(powerCalAvTemp) #(i-pingNum/2):(i+rangeNum-1)
        }
      }
    }
  }
  # remove high values
  if(noiseMax != 0){
    noiseEst <- ifelse(noiseEst >= noiseMax, noiseMax, noiseEst)
  }
  ##########
  # bind the noise estimate to the acoustics
  frameExp <- data |>
    tidyr::expand(dateTime, depth) |>
    dplyr::arrange(dateTime, desc(depth))
  # merges both tibbles to create a complete dataset
  noiseEst <- as.vector(noiseEst)
  data <- frameExp |>
    dplyr::left_join(data) |>
    dplyr::mutate(noiseEst = as.vector(noiseEst))
  #
  return(data)
}