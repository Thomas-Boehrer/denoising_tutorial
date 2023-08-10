# Thomas Boehrer
# 26.02.2023
# processing of measured backscattering values for noise removal
# functions are collected from various papers to calculate the portion of noise  
# -------------------------------------------------------------------------
# START
# -------------------------------------------------------------------------
power_cal_trans <- function(
    acoustics, temperature, salinity, pH,
    scaling, decibelMin, decibelMax,
    frequency, pulseDuration){
  # -------------------------------------------------------------------------
  acoustics <- acoustics |> 
  dplyr::select(
    c(timeBin, dateTime, depth, biomassScore, seaBottom)
  ) |>
  dplyr::ungroup()


  decibelRange <- c(decibelMin, decibelMax)
  # rescaling of the Biomass Values 
  # if scaling is requiered 
  if(!'scales' %in% rownames(installed.packages())){install.packages('scales')}
  if(scaling == TRUE){ # scaling needs to be implmented in the main script
    acoustics$meanSv <- 
        scales::rescale(acoustics$biomassScore, to = decibelRange) # Input needs to be defined dynamically 
  }
  # -------------------------------------------------------------------------
  # speed of sound by MacKenzie 1981
  # Mackenzie KV (1981) Nineâterm equation for sound speed in the oceans.
  # The Journal of the Acoustical Society of America, 70(3), 807â812.
  # https://doi.org/10.1121/1.386920
  # The ranges of validity encompass the following:
  #   temperature â2 to 30 Â°C, salinity 30 to 40 ppt, and depth 0 to 8000 m.
  # works 
  speed_of_sound <- function(temperature = 2, salinity = 34, depth) {
    depth <- abs(depth)
    speedOfSound <- 1448.96 + 4.591 * temperature - 5.304 * 10^-2 * (temperature^2) + 
      2.374 * 10^-4 *(temperature^3) + 1.340 * (salinity-35) + 1.630 * 10^-2 * (depth) + 
      1.675 * 10^-7 * (depth^2) - 1.025 * 10^-2 * temperature^(salinity - 35) - 
      7.139 * 10^-13 * temperature * depth^3
    return(speedOfSound)
  }
  acoustics$speedOfSound <- speed_of_sound(temperature, salinity, acoustics$depth) # test
  # -------------------------------------------------------------------------
  # TVG function - time varied gain 
  # a bit shaky since I donÂ´t know which echosounder has been used 
  # HERE FOR SIMRAD EK60
  # incorporates the range from the vessel to detection point and
  # accounts for amplification of the noise signal 
  # function calculates the amplification 
  # TO REMOVE THE RANGE DEPENDEND AMPLIFICATION FROM THE MEASURED BACKSCATTERING VALUE
  # works 
  range_amplification <- function(range, pulseDuration, speedOfSound){ # range corresponds to depth 
    range <- abs(range)
    rangeAmplification <- range - (pulseDuration * (speedOfSound/4))
    return(rangeAmplification)
  }
  acoustics$rangeAmplification <- range_amplification(
    range = acoustics$depth,
    pulseDuration, acoustics$speedOfSound) 
  # -------------------------------------------------------------------------
  # calculate absorption loss 
  # Ainslie MA, McColm JG. (1998). A simplified formula for viscous
  # and chemical absorption in sea water.
  # The Journal of the Acoustical Society of America, 103(3), 1671â1672.
  # https://doi.org/10.1121/1.421258
  # works 
  absorption_coef_AinslieMcColm <- function(frequency, depth, temperature = 2, salinity = 34, pH = 8.1){
    freq <- frequency/1000
    depthKm <- abs(depth/1000)
    f1 <- 0.78 * sqrt(salinity/35) * exp(temperature/26) # boric acid relaxation relaxation frequency
    f2 <- 42 * exp(temperature/17) # magnesium relaxation frequency
    alpha1 <- (0.106 * (f1 * (freq^2)) / ((f1^2) + (freq^2)) * exp((pH - 8) / 0.56))
    alpha2 <- (0.52 * (1 + temperature / 43) * (salinity / 35) * (f2 * (freq^2)) /
                 ((f2^2) + (freq^2)) * exp(-depthKm / 6))
    alpha3 <- 0.00049 * freq^2 * exp(-(temperature / 27 + depthKm))
    absorptionCoef <- (alpha1 + alpha2 + alpha3) / 1000 # convert the output in db/m 
    return(absorptionCoef)
  }
  acoustics$absorptionCoef <- absorption_coef_AinslieMcColm(frequency, acoustics$depth)
  # -------------------------------------------------------------------------
  # equivalent to mean_SV but with the range dependent amplification removed 
  # calculate backscattered power per sample
  # depth in absolute values 
  # meanSv = acoustics$meanSv 
  # works 
  power_cal_samp <- function(meanSv, depth, absorptionCoef, rangeAmplification){
    depth <- abs(depth)
    depth <- ifelse(depth >= 1, depth <- depth, depth <- 1)
    powerCalSamp <- meanSv - (20 * log10(depth) + 2 * absorptionCoef * rangeAmplification * depth)
    return(powerCalSamp)
  }
  acoustics$powerCalSamp <- power_cal_samp(acoustics$meanSv, acoustics$depth, acoustics$absorptionCoef, acoustics$rangeAmplification)
  # -------------------------------------------------------------------------
  # transform the calculated power per sample to the required format to fit it into the power function for every ping 
  power_cal_trans <- function(powerCalSamp){
    powerCalTrans <- 10^(powerCalSamp/10)
    return(powerCalTrans)
  }
  acoustics$powerCal <- power_cal_trans(acoustics$powerCalSamp)
  # from here on the data can be converted into a matrix to iterate through the data set
  # -------------------------------------------------------------------------
  # dataset output 
  # with the output powerCal one can calculate the noise estimate for ever sample (defined by timeBin and depth) 
  # acoustics <- tibble(timeBin = acoustics$timeBin, depth = acoustics$depth,
  #                     powerCal = acoustics$powerCal, absorptionCoef = acoustics$absorptionCoef,
  #                     meanSv <- acoustics$meanSv)
  # -------------------------------------------------------------------------
  return(acoustics)
}