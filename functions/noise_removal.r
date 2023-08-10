# START OF THE FUNCTION 
# -------------------------------------------------------------------------
# DESCRIPTION...


noise_removal <- function(data, scaling = TRUE) {
  
  # -------------------------------------------------------------------------
  # calculate the noiseSv
  noise_Sv <- function(noiseEst = data$noiseEst, depth = data$depth,
                       absorptionCoef = data$absorptionCoef){
    depth <- abs(depth)
    noiseSv <- noiseEst + (20 * log10(depth) + 2 * absorptionCoef * depth)
    return(noiseSv)
  }
  data$noiseSv <- noise_Sv(
    data$noiseEst, data$depth, data$absorptionCoef
  )
  # -------------------------------------------------------------------------
  # noise Correction
  correct_Sv <- function(meanSv = data$meanSv, noiseSv = data$noiseSv){
    correctSv <- 10 * log10((10^(meanSv/10)) - 10^(noiseSv/10))
    return(correctSv)
  }
  data$correctSv <- correct_Sv(
    data$meanSv, data$noiseSv
  )
  print(paste(sum(is.na(data$correctSv)), 'values are not available'))

  # -------------------------------------------------------------------------
  # if noise is extremely high and signal low --> NaNs are created
  # tidyverse
  # data <- data |>
  #   mutate_all(~replace(., is.nan(.), sort(data$correctSv)[2]))
  if(scaling == TRUE){
    data$biomassScoreDenoised <- scales::rescale(data$correctSv, to = c(0,1))
  }
# -------------------------------------------------------------------------
  #set up the dataset
  data <- data |>
    dplyr::select(dateTime, timeBin, depth, biomassScore,
    biomassScoreDenoised, correctSv, seaBottom)
# return the output of the function
  return(data)
}
