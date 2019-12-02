library(data.table)
library(noncensus)
data(counties)

# indx <- sapply(counties, is.factor)
# counties[indx] <- lapply(counties[indx], function(x) as.numeric(as.character(x)))

data_path <- "E:/Zekun/Project_Sea_Level_Rise/data/us_revenue_migration_data/"
hauer_irs_migration_data <- fread("E:/Zekun/Project_Sea_Level_Rise/data/Population_Migration/IRS-County-to-County-Migration-Data--1990-2013-copy.txt",
                            stringsAsFactors = F, check.names = F)

##### this part will generate all possible combination of counties-------------
county_list <- data.frame(county_name=as.character(counties$county_name),
                          state = as.character(counties$state),
                          state_fips = as.numeric(as.character(counties$state_fips)),
                          county_fips = as.numeric(as.character(counties$county_fips)))

full_states_list <- sort(unique((county_list$state_fips)))
full_states_list <- full_state_list[full_state_list <= 56]                         # irs data doesnt include state fips greater than 56
county_list <- county_list[county_list$state_fips <= 56, ]

##irs_state_list <- sort(unique(c(data_p$y2_statefips, data_p$y1_statefips)))
##irs_state_intersect <- irs_state_list[irs_state_list %in% full_state_list]     # find what states are in both irs data and full county list

##county_list_to_irs <- county_list[as.numeric(county_list$state_fips) %in% 
##                                     irs_state_intersect,]



# a function to make one digit state fips to two digits
fill_two_digits_state <- function(x){                                           
  if (nchar(x) <2){
    x <- paste(0,x, sep = "")
  }else{
    x <- x
  }
}
# end function

# a function to make 2 digit county fips to three digits
fill_three_digits_county <- function(countyfips){                               
  if(nchar(countyfips)<3){
    if(nchar(countyfips)<2){
      countyfips <- paste(0, 0,countyfips, sep = "")
    }else{
      countyfips <- paste(0, countyfips, sep = "")
    }
  }else{
    countyfips <- countyfips
  }
}
# end function
  
y1 <- sapply(X = county_list$state_fips, FUN = fill_two_digits_state, simplify=T)
y2 <- sapply(X = county_list$county_fips, FUN = fill_three_digits_county, 
             simplify = T)

y3 <- paste(y1, y2, sep = "")
# generate all possible origin-destination combinations
all_ori_desti_combination <- expand.grid(y3, y3, stringsAsFactors = F)

### Read irs data into a list -------------------------------------------------

data_list <- list.files(data_path, pattern = "countyoutflow", full.names = T)
data_list_short <- list.files(data_path, pattern = "countyoutflow", 
                              full.names = F)
irs_storage_list <- vector(mode = "list", length = length(data_list_short))

names(irs_storage_list) <- data_list_short
# create a list to store all csv data for later search
for (f in 1:length(data_list)) {                                                
  data_f <- fread(data_list[f], stringsAsFactors = F, 
                              check.names = F)
  irs_storage_list[[f]] <- data_f
  
}



# let's loop based on state and export the migration records -------------------

ori_state <- as.numeric(all_ori_desti_combination$Var2) %/% 1000
ori_county <- as.numeric(all_ori_desti_combination$Var2) %% 1000
dest_state <- as.numeric(all_ori_desti_combination$Var1) %/% 1000
dest_county <- as.numeric(all_ori_desti_combination$Var1) %% 1000

all_ori_desti_comb_matrix <- cbind(ori_state, ori_county, dest_state, dest_county)

for (state in full_states_list) {
  # get combination of selected looping state
  ori_from_this_state <- subset(all_ori_desti_comb_matrix, ori_state == state)
  n_combinations <- nrow(ori_from_this_state)
  
  # create an empty matrix to store migration records
  looping_state_migration_table <- as.data.frame(matrix(data = NA, 
                                                        nrow = n_combinations, 
                                                        ncol = 12))
  no_migration_combinations <- as.data.frame(matrix(data = NA,
                                                    nrow = n_combinations,
                                                    ncol = 4))
  # naming columns
  colnames(looping_state_migration_table) <- c("ori_state", "ori_county", "dest_state",  
                                        "dest_county", as.character(2008:2015))
  colnames(no_migration_combinations) <- c("ori_state", "ori_county", "dest_state",  
                                           "dest_county")
  valid_return_ind = 1
  no_return_ind = 1
  
  for(irow in 1:n_combinations){

    matched_record_vector <- rep(NA, 8)                                         # why repeated 8 times?  2008 to 2015
    searching_criteria <- ori_from_this_state[irow,]
    
    for(yth in 1: 8){
      matrix_of_y <- as.data.frame(irs_storage_list[[yth]])
      
      matched_record_ind <- which(matrix_of_y[,1] == searching_criteria[1] &        # get the row index of matched record
                                  matrix_of_y[,2] == searching_criteria[2] &
                                  matrix_of_y[,3] == searching_criteria[3] &
                                  matrix_of_y[,4] == searching_criteria[4])
      
      if(length(matched_record_ind)>0){                                         # consider 3 situation here: 1)duplicated matched record, 2)no returns, 3) valid returns
        matched_returns <- matrix_of_y[matched_record_ind, 7]                   # 7th column is the number of returns
        
        if( length(unique(matched_returns)) > 1){
          matched_record <- mean(matched_returns)
        }else if(is.na(matched_returns)){
          matched_record <- 0
        }else{
          matched_record <- matched_returns
        }
      }else{
        matched_record <- 0
      }
      
      matched_record_vector[yth] <- matched_record                              # save to yth element represent yth year
    }
    
    if(sum(matched_record_vector) == 0){
      no_migration_combinations[no_return_ind, ] <- searching_criteria
      no_return_ind <- no_return_ind + 1
    }else{
      looping_state_migration_table[valid_return_ind, ] <- c(searching_criteria, matched_record_vector)
      valid_return_ind <- valid_return_ind + 1
    }
  }
  
  valid_return_table <- looping_state_migration_table[1:valid_return_ind - 1,]
  no_migration_comb_table <- no_migration_combinations[1:no_return_ind - 1, ]
  
  write.table(valid_return_table, "E:/Zekun/Project_Sea_Level_Rise/data/Population_Migration/nc_valid_return.csv",
            sep = ",", col.names = TRUE)
  write.table(no_migration_comb_table, "E:/Zekun/Project_Sea_Level_Rise/data/Population_Migration/nc_no_migration_comb.csv",
              sep = ",", col.names = TRUE)
}








# unsolved part : parallel ####################################################
return_matched_migration <- function(i, ori_dest_df, matrix_storage){
  
  ori_desti_dim <- dim(ori_dest_df)                                             # get the number of rows and columns in origins and destination frame
  storage_length <- length(matrix_storage)                                      # get how many irs migration matrices are in the matrix storage list
  
  matched_record_vector <- rep(NA, 8)
  
  for (y in 1:storage_length) {
    
    matrix_of_y <- as.data.frame(matrix_storage[[y]])
    
    matched_record_ind <- which(matrix_of_y[,1] == ori_dest_df[i, 1] &        # get the row index of matched record
                                matrix_of_y[,2] == ori_dest_df[i, 2] &
                                matrix_of_y[,3] == ori_dest_df[i, 3] &
                                matrix_of_y[,4] == ori_dest_df[i, 4])
    
    if(length(matched_record_ind)>0){                                          # consider 3 situation here: 1)duplicated matched record, 2)no returns, 3) valid returns
      
      matched_record <- matrix_of_y[matched_record_ind, 7]                    # 7th column is the number of returns
      
      if( length(unique(matched_record)) > 1){
        matched_record <- mean(matched_record)
      }else if(is.na(matched_record)){
        matched_record <- 0
      }else{
        matched_record <- matched_record
      }
    }else{
      matched_record <- 0
    }
    
    matched_record_vector[y] <- matched_record
  }
  #print(matched_record_vector)
  ori_dest_df[i, 5:12] <- matched_record_vector
  #return(ori_dest_df[i, ])
}



library(doParallel)
library(foreach)
n_cores <- detectCores(logical = F)
cl <- makeCluster(n_cores - 1)
registerDoParallel(cl)

results1 <- foreach (nr = 1:nrow(county_migration_table), .combine=rbind) %dopar%{
      return_matched_migration(i = nr, ori_dest_df = county_migration_table, 
                              matrix_storage = matrix_storage_list)

}


stopImplicitCluster()
write.table(x = results1, file = "E:/Zekun/total_migration_08_15_v2.csv", sep = ",", col.names = TRUE, row.names = FALSE)



##### Updates Hauer's table    -------------------------------------------------
# This part will update hauer's table with new 2013-2015 data
h_origin <- hauer_irs_migration_data$origin
h_dest <- hauer_irs_migration_data$destination

updated_hauer_matrix <- as.data.frame(hauer_irs_migration_data)
updated_hauer_matrix$`2014` <- rep(NA, 46203)
updated_hauer_matrix$`2015` <- rep(NA, 46203)
for (kth in 1:length(h_origin)){
  h_origin_state <- h_origin[kth] %/% 1000
  h_origin_county <- h_origin[kth] %% 1000
  h_dest_state <- h_dest[kth] %/% 1000
  h_dest_county <- h_dest[kth] %% 1000
  
  hauer_kth_return <- hauer_irs_migration_data[kth, 21:26]
  
  matched_vector <- vector(length = 8)
  
  for (eth in 1:length(irs_storage_list)) {
    matrix_of_eth <- as.data.frame(irs_storage_list[[eth]])
    matched_index <- which(matrix_of_eth$y1_statefips == h_origin_state &
                          matrix_of_eth$y1_countyfips == h_origin_county &
                          matrix_of_eth$y2_statefips == h_dest_state &
                          matrix_of_eth$y2_countyfips == h_dest_county)
    if(length(matched_index) >= 1){
      matched_returns <- mean(matrix_of_eth[matched_index, 7])
      matched_vector[eth] <- matched_returns
    }else {
      matched_returns <- 0
      matched_vector[eth] <- matched_returns
    }
    
    
  }
  
  if(all(matched_vector[1:6] == hauer_kth_return)){
    updated_hauer_matrix[kth, 27:28] <- matched_vector[7:8]
  }else{
    diff_vector <- matched_vector - hauer_kth_return
    diff_ind <- which(diff_vector != 0)
    updated_hauer_matrix[kth, diff_ind] <- matched_vector[diff_ind]
    updated_hauer_matrix[kth, 27:28] <- matched_vector[7:8]
  }
  
}
write.table(updated_hauer_matrix, "E:/Zekun/Project_Sea_Level_Rise/data/migration_records_v2.csv", 
            col.names = TRUE, sep = ", ")

## now detect if there are combinations left behind by hauer's data------------
irs_2014 <- irs_storage_list[[7]]
irs_2015 <- irs_storage_list[[8]]

returns_matrix <- irs_2014
for (nth in 1:nrow(returns_matrix)){
 nth_vector <- returns_matrix[nth,]
 ori_state_fips <- fill_two_digits_state(nth_vector$y1_statefips)
 ori_county_fips <- fill_three_digits_county(nth_vector$y1_countyfips)
 dest_state_fips <- fill_two_digits_state(nth_vector$y2_statefips)
 dest_county_fips <- fill_three_digits_county(nth_vector$y2_countyfips)
 ori_place <- paste(ori_state_fips, ori_county_fips, sep = "")
 dest_place <- paste(dest_state_fips, dest_county_fips, sep = "")
 
 which(updated_hauer_matrix$origin == as.numeric(ori_place))
}

  


