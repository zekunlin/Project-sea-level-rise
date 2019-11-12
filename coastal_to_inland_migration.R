library(data.table)
library(stringr)
library(knitr)
library(rgdal)


migration_dp <- "E:/slr_project_data/us_revenue_migration_data/countyoutflow"
#year_series <- c("08", "09", seq(10, 16, 1))                                     # create a year series
year_series <- seq(11, 16, 1)


for (k in 1:(length(year_series) - 1)) {
  # Load data ---------------------------
  
  # data path to migration data
  
  input_csv_name <- paste(migration_dp, year_series[k], year_series[k+1], 
                          ".csv", sep = "")
  migration_out <- fread(input_csv_name)
  
  # load a list of coastal counties
  coastal_counties <- read.table("E:/Zekun/Population_Migration/NC_Coastal_Counties.csv", 
                                 sep=",", header = T)
  
  coastal_three_digits_fips <- coastal_counties$FIPS %% 1000                      # get the last three digits of fips code
  
  # Data Filter and Selection ---------------------------
  
  in_state_migraion <- migration_out[y1_statefips==37 & y2_statefips == 37,       # filter migrations from NC coastal counties to NC inland counties
                                     ,]                                          
  
  #out_state_migration <- migration_out[y1_statefips == 37 & y2_statefips != 37,,]
  
  return_and_exemption <- in_state_migraion$n1 + in_state_migraion$n2             # the number of immigrant or non-immigrant should be the sum of -
                                                                                  # the number fo tax return (n1) and exemptions (n2)
  
  my.table <- data.frame(y1_countyfips=in_state_migraion$y1_countyfips, 
                         y2_countyfips=in_state_migraion$y2_countyfips, 
                         return_and_exemption)                                    # create a table with origins, destinations, and the number of migrants
  
  # Make Transition Table ---------------------------
  
  non_repeated_counties <- unique(my.table$y1_countyfips)
  
  migration_matrix <- matrix(data = NA, 100, 100)                                 # 100 counties make to 100 * 100 transition matrix
  
  for (i in 1:100) {
    migration_vector <- rep(NA, 100)
    
    specified_county <- non_repeated_counties[i]
    
    dest_list <- my.table[my.table$y1_countyfips == specified_county, ]$y2_countyfips
    
    
    for (j in 1:100) {
      in_loop_dest <- non_repeated_counties[j]
      
      if (in_loop_dest %in% dest_list){
        migration_vector[j] <- my.table[my.table$y1_countyfips == specified_county &
                                          my.table$y2_countyfips == in_loop_dest, 3]
      }else{
        migration_vector[j] <- 0
      }
    }
    
    migration_matrix[i, ] <- migration_vector
  }
  
  colnames(migration_matrix) <- non_repeated_counties
  rownames(migration_matrix) <- non_repeated_counties
  
  write.csv(migration_matrix, file = paste(
    write_path, "CountyOutFlow_Amount_Table", 
    year_series[k], year_series[k+1], ".csv", sep = ""))                          # Rows are origins, columns are destinations
  
  
  # Make Transition Probability Table ---------------------------
  P <- migration_matrix / (rowSums(migration_matrix) - diag(migration_matrix))    # Calculate Migration Probability: 
                                                                                  # P = Number of Population (moving or stay) / Total Moved Population
  diag(P) <- NA
  
  P_stay <- diag(migration_matrix) / rowSums(migration_matrix)
  
  write.csv(P, file = paste(
    write_path, "CountyOutFlow_Probability_Table", 
    year_series[k], year_series[k+1], ".csv", sep = ""))
  

  
}

