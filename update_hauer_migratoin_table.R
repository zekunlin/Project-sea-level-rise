library(data.table)
library(readxl)

hauers_table <- fread("E:/Zekun/Project_Sea_Level_Rise/data/Population_Migration/IRS-County-to-County-Migration-Data--1990-2013-copy.txt",
                   stringsAsFactors = F, check.names = F)
annual_table_dir <- list.files(path = "E:/Zekun/Project_Sea_Level_Rise/data/annual_migration_tables/",
                               full.names = TRUE)
initiate_table <- as.data.frame(read.csv(annual_table_dir[1], header = TRUE, sep = ","))

origins = paste(sapply(initiate_table$y1_state, FUN = fill_two_digits_state, simplify = TRUE), 
                 sapply(initiate_table$y1_county, FUN = fill_three_digits_county, simplify = TRUE), sep = "")
destinations = paste(sapply(initiate_table$y2_state, FUN = fill_two_digits_state, simplify = TRUE), 
                     sapply(initiate_table$y2_county, FUN = fill_three_digits_county, simplify = TRUE), sep = "")
keys <- paste(origins, destinations, sep = "")

# total_migration_table <- data.frame(origin = origins,
#                                     destination = destinations,
#                                     key = keys,
#                                     `1990` = initiate_table$returns, 
#                                     stringsAsFactors = FALSE)

total_migration_table <- data.frame(origin = 0,
                                    destination = 0,
                                    key = 0,
                                    stringsAsFactors = FALSE)
#total_migration_table <- matrix(0, ncol = 29, nrow = 2)
#colnames(total_migration_table) <- c("origin", "destination", "key", as.character(seq(1990, 2015)))
#total_migration_table <- data.frame(total_migration_table)



`%notin%` <- Negate(`%in%`)


# a function to make one digit state fips to two digits
fill_two_digits_state <- function(statefips){                                           
  x <- as.character(statefips)
  if (nchar(x) < 2){
    x <- paste(0,x, sep = "")
  }else{
    x <- x
  }
  return(x)
}
# end function

# a function to make 2 digit county fips to three digits
fill_three_digits_county <- function(x){                               
  countyfips <- as.character(x)
  if(nchar(countyfips) < 3){
    if(nchar(countyfips) < 2){
      countyfips <- paste(0, 0, countyfips, sep = "")
    }else{
      countyfips <- paste(0, countyfips, sep = "")
    }
  }else{
    countyfips <- countyfips
  }
  return(countyfips)
}

time_steps <- paste("x", c(seq(1990:2015)), sep = "")

for(f in annual_table_dir[1:26]){
  
  #total_migration_table_dim <- dim(total_migration_table)
  
  mig_of_f <- read.csv(f, header = TRUE, sep = ",")
  table_dim <- dim(mig_of_f)
  
  y1_state_ch <- sapply(mig_of_f$y1_state, fill_two_digits_state, simplify = TRUE)
  y1_county_ch <- sapply(mig_of_f$y1_county, fill_three_digits_county, simplify = TRUE)
  y2_state_ch <- sapply(mig_of_f$y2_state, fill_two_digits_state, simplify = TRUE)
  y2_county_ch <- sapply(mig_of_f$y2_county, fill_three_digits_county, simplify = TRUE )
  
  f_origins <- paste(y1_state_ch, y1_county_ch, sep = "")
  f_destinations <- paste(y2_state_ch, y2_county_ch, sep = "")
  f_table <- data.frame(origin = f_origins, destination = f_destinations, 
                       key = paste(f_origins, f_destinations, sep = ""), 
                       stringsAsFactors = FALSE)
  
  f_existed_ind <- which(f_table$key %in% total_migration_table$key)            # records from f but already existed in total migration table
  
  f_existed_comb <- f_table[f_existed_ind,]
  f_missing_ind <- which(f_table$key %notin% total_migration_table$key)         # records from f but not in total migration table
  f_missing_comb <- f_table[f_missing_ind,]                                     # 
  
  total_existed_ind <- which(total_migration_table$key %in% f_table$key)        # records from total migration table and also existed in f table
  total_missing_ind <- which(total_migration_table$key %notin% f_table$key)     # records from total migration table but not in f table
  
  total_existed_comb <-total_migration_table[total_existed_ind, ]
  total_missing_comb <- total_migration_table[total_missing_ind, ]
  
  total_migration_table <- rbind(total_migration_table, f_missing_comb)
  
}


total_mig_combinations <- total_migration_table[!duplicated(total_migration_table$key),]
write.table(total_mig_combinations, file = "E:/Zekun/Project_Sea_Level_Rise/data/all_migration_combinations.csv", 
          sep = ",", col.names = TRUE)
### 2nd part: get a table fill with migration records for each year -----------

total_mig_combinations <- read.table("E:/Zekun/Project_Sea_Level_Rise/data/all_migration_combinations.csv", 
                                     stringsAsFactors = FALSE, sep = ",")
head(total_mig_combinations)
total_mig_combinations <- total_mig_combinations[-1, ]

t1 <- cbind(total_mig_combinations,
            matrix(data=0, nrow = nrow(total_mig_combinations), ncol = 26))

for(k in 1:length(annual_table_dir)){
  
  mig_of_f <- read.csv(annual_table_dir[k], header = TRUE, sep = ",")
  table_dim <- dim(mig_of_f)
  
  y1_state_ch <- sapply(mig_of_f$y1_state, fill_two_digits_state, simplify = TRUE)
  y1_county_ch <- sapply(mig_of_f$y1_county, fill_three_digits_county, simplify = TRUE)
  y2_state_ch <- sapply(mig_of_f$y2_state, fill_two_digits_state, simplify = TRUE)
  y2_county_ch <- sapply(mig_of_f$y2_county, fill_three_digits_county, simplify = TRUE )
  
  f_origins <- paste(y1_state_ch, y1_county_ch, sep = "")
  f_destinations <- paste(y2_state_ch, y2_county_ch, sep = "")
  f_table <- data.frame(origin = f_origins, destination = f_destinations, 
                        key = paste(f_origins, f_destinations, sep = ""), 
                        returns = mig_of_f$returns, stringsAsFactors = FALSE)
  store_vector <- vector(mode = "numeric", length = nrow(t1))
  
  for(irow in 1:nrow(f_table)){
    
    ith_key <- f_table[irow, 3]
    store_vector[which(t1$key == ith_key)] <- as.numeric(as.character(unlist(f_table[irow, ]$returns)))
  }
   t1[, k + 3] <- store_vector
}

colnames(t1) <- c("origin", "destination", "key", as.character(seq(1990, 2015)))
write.table(t1, file = "E:/Zekun/Project_Sea_Level_Rise/data/migration_table.csv",
            col.names = TRUE, sep = ",")
  

