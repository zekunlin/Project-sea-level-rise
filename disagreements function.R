
library(raster)
library(landscapemetrics)

quantity_allocation_disagreement <- function(reference, comparison){
  ## test that the comparison raster is the same extent, resolution, and crs as the reference to ensure that they can be compared accurately
  raster::compareRaster(reference, comparison)
  # compare <- reference - comparison
  # compare3 <- reference + comparison
  # extent <- raster::extent(reference)
  # num_cols <- raster::ncol(reference)
  # num_rows <- raster::nrow(reference)
  # resolution <- raster::xres(reference)
  # num_of_cells <- max(raster::cellsFromExtent(reference, raster::extent(reference)))
  # max_num_patches <- ceiling(num_rows/2) * ceiling(num_cols/2)
  # distance_min <- 2*resolution
  # distance_max <- sqrt((num_rows - 1)^2 + (num_cols - 1)^2)*resolution
  # max_para <- (resolution*4)/(resolution*resolution)
  # min_para <- ((resolution * num_rows * 2) + (resolution * num_cols * 2))/((resolution * num_rows) * (resolution * num_cols))
  
  ## calculate number of infected patches
  NP_ref <- landscapemetrics::lsm_c_np(reference, directions = 8)$value[2]
  if (sum(comparison[comparison > 0]) == 0) {
    NP_comp <- 0
    ENN_MN_comp <- 0
    LPI_comp <- 0
    PARA_MN_comp <- 0
  } else {
    NP_comp <- landscapemetrics::lsm_c_np(comparison, directions = 8)$value[2]
  }
  
  change_NP <- abs((NP_comp - NP_ref)/(NP_comp + NP_ref))
  # change_NP <- abs((NP_comp - NP_ref)/max(abs(1 - NP_ref), abs(NP_ref - max_num_patches)))
  if (change_NP >= 1) {change_NP <- 1}
  
  # calculate the mean euclidean distance between patches
  if (NP_ref > 1) {
    ENN_MN_ref <- landscapemetrics::lsm_c_enn_mn(reference, directions = 8, verbose = TRUE)$value[2]
  } else  if (NP_ref == 1) {
    ENN_MN_ref <- 0
  }
  
  if (sum(comparison[comparison > 0]) != 0 && NP_comp > 1) {
    ENN_MN_comp <- landscapemetrics::lsm_c_enn_mn(comparison, directions = 8, verbose = TRUE)$value[2]
  } else  if (sum(comparison[comparison > 0]) != 0 && NP_comp <= 1) {
    ENN_MN_comp <- 0
  }
  
  if (ENN_MN_ref != 0) {
    change_ENN_MN <- abs((ENN_MN_comp - ENN_MN_ref)/(ENN_MN_comp + ENN_MN_ref))
    # change_ENN_MN <- abs((ENN_MN_comp - ENN_MN_ref)/max(abs(distance_min - ENN_MN_ref), abs(distance_max - ENN_MN_ref)))
  } else if (ENN_MN_comp == 0 && ENN_MN_ref == 0) {
    change_ENN_MN <- 0
  } else {
    change_ENN_MN <- 1
  }
  
  # calculate the mean perimeter-area ratio of patches and the difference
  PARA_MN_ref <- landscapemetrics::lsm_c_para_mn(reference, directions = 8)$value[2]
  if (sum(comparison[comparison >0]) == 0) {
    PARA_MN_comp <- 0
  } else if (sum(comparison[comparison >0]) != 0) {
    PARA_MN_comp <- landscapemetrics::lsm_c_para_mn(comparison, directions = 8)$value[2]
  }
  
  change_PARA_MN <- abs((PARA_MN_comp - PARA_MN_ref)/(PARA_MN_comp + PARA_MN_ref)) 
  # change_PARA_MN <- abs((PARA_MN_comp - PARA_MN_ref)/max(abs(min_para - PARA_MN_ref), abs(max_para - PARA_MN_ref)))
  
  # calculate the largest patch index and difference
  LPI_ref <- landscapemetrics::lsm_c_lpi(reference, directions = 8)$value[2]
  if (sum(comparison[comparison >0]) == 0) {
    LPI_comp <- 0
  } else if (sum(comparison[comparison >0]) != 0) {
    LPI_comp <- landscapemetrics::lsm_c_lpi(comparison, directions = 8)$value[2]
  }
  
  change_LPI <- abs((LPI_comp - LPI_ref)/(LPI_comp + LPI_ref))
  # change_LPI <- abs((LPI_comp - LPI_ref)/max(abs(0 - LPI_ref),abs(100 - LPI_ref)))
  # calculate landscape similarity index between reference and comparison
  LSI <- 1 - ((change_NP + change_ENN_MN + change_PARA_MN + change_LPI) / 4)
  if (LSI < 0) { LSI <- 0 }
  configuration_disagreement <- 1 - LSI
  
  ## calculate reference and comparison totals to use for creation of probabilities 
  positives_in_reference <- sum(reference[] == 1)
  negatives_in_reference <- sum(reference[] == 0)
  total_in_reference <- sum(reference[] >= 0)
  positives_in_comparison <- sum(comparison[] == 1)
  negatives_in_comparison <- sum(comparison[] == 0)
  ## calculate confusion matrix for accurracy assessment
  true_positive <- sum(comparison[reference == 1] == 1)
  false_positive <- sum(comparison[reference == 0] == 1)
  false_negative <- sum(comparison[reference == 1] == 0)
  true_negative <- sum(comparison[reference == 0] == 0)
  ## calculate probabilities of each class based on Death to Kappa (Pontius et al. 2011)
  if (negatives_in_comparison == 0 && total_in_reference == 0) {
    probability_00 <- (true_negative / 1) * (negatives_in_reference / 1)
    probability_01 <- (false_negative / 1) * (negatives_in_reference / 1)
  } else if (total_in_reference == 0) {
    probability_00 <- (true_negative / negatives_in_comparison) * (negatives_in_reference / 1)
    probability_01 <- (false_negative / negatives_in_comparison) * (negatives_in_reference / 1)
  } else if (negatives_in_comparison == 0) {
    probability_00 <- (true_negative / 1) * (negatives_in_reference / total_in_reference)
    probability_01 <- (false_negative / 1) * (negatives_in_reference / total_in_reference)
  } else {
    probability_00 <- (true_negative / negatives_in_comparison) * (negatives_in_reference / total_in_reference)
    probability_01 <- (false_negative / negatives_in_comparison) * (negatives_in_reference / total_in_reference)
  }
  
  if (positives_in_comparison == 0 && total_in_reference == 0) {
    probability_10 <- (false_positive / 1) * (positives_in_reference / 1)
    probability_11 <- (true_positive / 1) * (positives_in_reference / 1)
  } else if (total_in_reference == 0) {
    probability_10 <- (false_positive / positives_in_comparison) * (positives_in_reference / 1)
    probability_11 <- (true_positive / positives_in_comparison) * (positives_in_reference / 1)
  } else if (positives_in_comparison == 0) {
    probability_10 <- (false_positive / 1) * (positives_in_reference / total_in_reference)
    probability_11 <- (true_positive / 1) * (positives_in_reference / total_in_reference)
  } else {
    probability_10 <- (false_positive / positives_in_comparison) * (positives_in_reference / total_in_reference)
    probability_11 <- (true_positive / positives_in_comparison) * (positives_in_reference / total_in_reference)
  }
  
  ## calculate quantity and allocation disagreements for infected/infested from probabilities based on Death to Kappa (Pontius et al. 2011)
  quantity_disagreement <- abs((probability_11 + probability_10) - (probability_11 +probability_01))
  allocation_disagreement <- 2 * min((probability_11 + probability_10) - probability_11, (probability_11 + probability_01) - probability_11)
  total_disagreement <- quantity_disagreement + allocation_disagreement
  ## calculate odds ratio with adjustments so can never be NA or INF
  if(false_negative == 0 && false_positive == 0) {
    odds_ratio = (true_positive * true_negative) / 1
  } else if (false_negative == 0) {
    odds_ratio = (true_positive * true_negative) / false_positive
  } else if (false_positive == 0) {
    odds_ratio = (true_positive * true_negative) / false_negative
  } else {
    odds_ratio = (true_positive * true_negative) / (false_negative * false_positive)
  }
  ## create data frame for outputs and add calculated values to it
  output <- data.frame(quantity_disagreement = 0, allocation_disagreement = 0, total_disagreement = 0, configuration_disagreement = 0, omission = 0, commission = 0,  true_positives = 0, true_negatives = 0, odds_ratio = 0)
  output$omission <- false_negative
  output$commission <- false_positive
  output$true_positives <- true_positive
  output$true_negatives <- true_negative
  output$quantity_disagreement <- quantity_disagreement
  output$allocation_disagreement <- allocation_disagreement
  output$total_disagreement <- total_disagreement
  output$configuration_disagreement <- configuration_disagreement
  output$odds_ratio <- odds_ratio
  output$np_ref <- NP_ref
  output$np_comp <- NP_comp
  output$change_np <- change_NP
  output$enn_mn_ref <- ENN_MN_ref
  output$enn_mn_comp <- ENN_MN_comp
  output$change_enn_mn <- change_ENN_MN
  output$lpi_ref <- LPI_ref
  output$lpi_comp <- LPI_comp
  output$change_lpi <- change_LPI
  output$para_mn_ref <- PARA_MN_ref
  output$para_mn_comp <- PARA_MN_comp
  output$change_para_mn <- change_PARA_MN
  
  return(output)
}
