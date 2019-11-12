library(data.table)
library(noncensus)
data(counties)

# indx <- sapply(counties, is.factor)
# counties[indx] <- lapply(counties[indx], function(x) as.numeric(as.character(x)))

data_path <- "E:/slr_project_data/us_revenue_migration_data/"
hauer_irs_migration_data <- fread("E:/Zekun/Population_Migration/IRS-County-to-County-Migration-Data--1990-2013-copy.txt",
                                  stringsAsFactors = F, check.names = F)

county_list <- data.frame(county_name=as.character(counties$county_name),
                          state = as.character(counties$state),
                          state_fips = as.numeric(as.character(counties$state_fips)),
                          county_fips = as.numeric(as.character(counties$county_fips)))

full_state_list <- sort(unique((county_list$state_fips)))
full_state_list <- full_state_list[full_state_list<=56]                         # irs data doesnt include state fips greater than 56
#irs_state_list <- sort(unique(c(data_p$y2_statefips, data_p$y1_statefips)))
#irs_state_intersect <- irs_state_list[irs_state_list %in% full_state_list]     # find what states are in both irs data and full county list

# county_list_to_irs <- county_list[as.numeric(county_list$state_fips) %in% 
#                                     irs_state_intersect,]

county_list <- county_list[county_list$state_fips<=56,]


fill_two_digits_state <- function(x){                                           # a function to make one digit state fips to two digits
  if (nchar(x) <2){
    x <- paste(0,x, sep = "")
  }else{
    x <- x
  }
}

fill_three_digits_county <- function(countyfips){                               # a function to make 2 digit county fips to three digits
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

y1 <- sapply(X = county_list$state_fips, FUN = fill_two_digits_state, simplify=T)
y2 <- sapply(X = county_list$county_fips, FUN = fill_three_digits_county, 
             simplify = T)

y3 <- paste(y1, y2, sep = "")

all_ori_desti <- expand.grid(y3, y3, stringsAsFactors = F)



data_list <- list.files(data_path, pattern = "countyoutflow", full.names = T)
data_list_short <- list.files(data_path, pattern = "countyoutflow", 
                              full.names = F)
matrix_storage_list <- vector(mode = "list", length = length(data_list_short))
names(matrix_storage_list) <- data_list_short

for (f in 1:length(data_list)) {                                                # create a list to store all csv data for later search
  data_f <- fread(data_list[f], stringsAsFactors = F, 
                  check.names = F)
  matrix_storage_list[[f]] <- data_f
  
}


county_migration_table <- as.data.frame(matrix(data = NA, nrow = nrow(all_ori_desti), 
                                               ncol = 12))                                     # define a empty matrix to fill in
colnames(county_migration_table) <- c("ori_state", "ori_county", "dest_state",  # name columns 
                                      "dest_county", as.character(2008:2015))
ori_state <- as.numeric(all_ori_desti$Var2) %/% 1000
ori_county <- as.numeric(all_ori_desti$Var2) %% 1000
dest_state <- as.numeric(all_ori_desti$Var1) %/% 1000
dest_county <- as.numeric(all_ori_desti$Var1) %% 1000

county_migration_table$ori_state <- ori_state
county_migration_table$ori_county <- ori_county
county_migration_table$dest_state <- dest_state
county_migration_table$dest_county <- dest_county


# unsolved part : parallel ####################################################
return_matched_migration <- function(i, ori_dest_df, matrix_storage){
  
  ori_desti_dim <- dim(ori_dest_df)                                             # get the number of rows and columns in origins and destination frame
  storage_length <- length(matrix_storage)                                      # get how many irs migration matrices are in the matrix storage list

    for (y in 1:storage_length) {
      
      matrix_of_y <- as.data.frame(matrix_storage[[y]])
      
      matched_record_ind <- which(matrix_of_y[,1] == ori_dest_df[i, 1] &        # get the row index of matched record
                                  matrix_of_y[,2] == ori_dest_df[i, 2] &
                                  matrix_of_y[,3] == ori_dest_df[i, 3] &
                                  matrix_of_y[,4] == ori_dest_df[i, 4])
      
      if(length(matched_record_ind)>1){                                         # consider 3 situation here: 1)duplicated matched record, 2)no returns, 3) valid returns
        
        matched_record <- matrix_of_y[matched_record_ind, 7]                    # 7th column is the number of returns
        
        if( length(unique(matched_record)) > 1){
          matched_record <- mean(matched_record)
        }else if(is.na(matched_record)){
          matched_record <- 0
        }else{
          matched_record <- matched_record
        }
        
      }
      
      ori_dest_df[i, 4 + y] <- matched_record

  }
}




library(doParallel)
library(foreach)
n_cores <- detectCores(logical = F)
cl <- makeCluster(n_cores - 2)
registerDoParallel(cl)

system.time(
foreach (rows = 1:100, .combine=cbind) %dopar%{
  
  return_matched_migration(i = rows, ori_dest_df = county_migration_table, 
                           matrix_storage = matrix_storage_list)
  
}
)

stopImplicitCluster()




