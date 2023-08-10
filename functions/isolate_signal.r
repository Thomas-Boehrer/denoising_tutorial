# Thomas Boehrer
# 23/03/2023
##########
# Removal of Remnants
# This function aims to remove the uppermoste and
# lowermost parts of the echograms, when necessary
##########
isolate_signal <- function(data, upper_limit = -10, lower_limit) {

  data$upperLimit <- rep(-upper_limit, length = nrow(data))
  data$lower_limit <- rep(-lower_limit, length = nrow(data))
    # remove parts below the sea floor detection line 
    # the lower limit, and above the upper limit
    return(
      data |>
            dplyr::mutate(
              biomassScoreDenoised = dplyr::case_when(
                  depth >= upperLimit | 
                  depth <= seaBottom |
                  depth <= lower_limit ~ NaN,
                  TRUE ~ biomassScoreDenoised
              )
            )
    )
}
