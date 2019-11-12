library(data.table)
library(stringr)
library(knitr)
library(rgdal)

read_path <- "E:/Zekun/Population_Migration/"

IRS_migration_data <- read.table(paste(read_path, 
                      "IRS-County-to-County-Migration-Data--1990-2013-copy.txt",
                      sep = ""), header = T)
head(IRS_migration_data)                                                        # there only data up to 2013, we need to add 2014, 2015 to it
irow <- dim(IRS_migration_data)[1]
origin_list <- IRS_migration_data$origin
desti_list <- IRS_migration_data$destination

# add 2014-2016 irs data to Hauer's data ---------------------------------------

irs_raw_data_path <- "E:/slr_project_data/us_revenue_migration_data"

raw_data_list <- list.files(irs_raw_data_path, pattern = "countyoutflow*", 
                            full.names = T)
irs_data_to_add <- tail(raw_data_list, n=2)

for (d in irs_data_to_add) {
  migration_of_year <- 1:dim(IRS_migration_data)[1]
  t1 <- fread(d, check.names = F, stringsAsFactors = F)
  for (i in 1:irow) {
    pick_row <- which(t1$y1_statefips == origin_list[i] %/% 1000 & 
             t1$y1_countyfips == origin_list[i] %% 1000 &
              t1$y2_statefips == desti_list[i] %/%1000 &
                t1$y2_countyfips == desti_list[i] %% 1000)
    if (length(pick_row) == 0) {
      matched_migration <- 0
    }else{
      matched_migration <- t1[pick_row,]$n1
    }
  migration_of_year[i] <- matched_migration  
    
  }
   
  IRS_migration_data <- cbind(IRS_migration_data, migration_of_year)
  
}

name_vector <- names(IRS_migration_data)
name_vector[27:28] <- c("X2014","X2015")
colnames(IRS_migration_data) <- name_vector

migration_of_year_2012 <- migration_of_year
diff_2012 <- setdiff(migration_of_year_2012, IRS_migration_data$X2012)          # shouldnt be different? but why?
diff_2012 <-  migration_of_year_2012 - IRS_migration_data$X2012
diff_index <- which(diff_2012 != 0)
diff_element_2012 <- cbind(IRS_migration_data$X2012[diff_index], 
                       migration_of_year_2012[diff_index])

migration_of_year_2013 <- migration_of_year
diff_2013 <- migration_of_year_2013 - IRS_migration_data$X2013
diff_index_2013 <- which(diff_2013 != 0)
diff_element_2013 <- cbind(IRS_migration_data$X2013[diff_index_2013],
                            migration_of_year_2013[diff_index_2013])

migration_of_year_2011 <- migration_of_year
diff_2011 <- migration_of_year_2011 - IRS_migration_data$X2011
diff_index_2011 <- which(diff_2011 != 0)
diff_element_2011 <- cbind(IRS_migration_data$X2011[diff_index_2011],
                           migration_of_year_2011[diff_index_2011])


IRS_migration_updated_data <- IRS_migration_data
IRS_migration_updated_data$X2012 <- migration_of_year_2012
IRS_migration_updated_data$X2013 <- migration_of_year_2013


IRS_migration_data[diff_element_2012,]


# BUILD MIGRATION MATRIX -------------------------------------------------------

odo <- fread("E:/Zekun/Population_Migration/origin-destination-output.txt",     # read origin-destination data from Hauer
                  check.names = F, stringsAsFactors = F)
coastal_counties <- read.table(
  "E:/Zekun/Population_Migration/NC_Coastal_Counties.csv", sep=",", header = T) # read coastal county list
coastal_three_digits_fips <- coastal_counties$FIPS %% 1000 


IRS_dim <- dim(IRS_migration_updated_data)                                      # get dimension of IRS data
county_cumulative_migration <- rowSums(IRS_migration_updated_data[, 3:IRS_dim[2]])
IRS_cumulative_matrix <- cbind(IRS_migration_updated_data[,1:2], 
                               county_cumulative_migration)

## select out NC counties from IRS migration data        ----------------------
IRS_NC <- IRS_cumulative_matrix[IRS_cumulative_matrix$origin %/% 1000 == 37,]

destination_list_ofNC <- sort(unique(IRS_NC$destination))
origin_list_ofNC <- unique(IRS_NC$origin)

migration_matrix <- matrix(data=0, nrow = length(origin_list_ofNC), 
                           ncol = length(destination_list_ofNC))
row.names(migration_matrix) <- origin_list_ofNC
colnames(migration_matrix) <- destination_list_ofNC


for (x in origin_list_ofNC) {
  origin_index <- which(IRS_NC$origin == x)
  one_tomany_list <- IRS_NC[origin_index,]
  i <- which(rownames(migration_matrix) == x)
  j <- which(colnames(migration_matrix) == one_tomany_list$destination)
  migration_matrix[i, j] <- one_tomany_list$county_cumulative_migration
}



coastal_counties_index <- which(
  migration_matrix[,1] %in% coastal_three_digits_fips)                          # get row indexes for coastal counties

coastal_moving_out_amount <- migration_matrix[coastal_counties_index, ]







