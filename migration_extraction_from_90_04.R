folder_list <- list.files(path = "E:/Zekun/Project_Sea_Level_Rise/data/us_revenue_migration_data/",
           pattern = "*countymigration", full.names = TRUE)
folder_selecte <- folder_list[1]
folder_selecte
txt_path <- list.files(folder_selecte, recursive = T, 
           pattern = glob2rx("C*o.txt"), full.names = TRUE)


# 1. the first part will address migration from 90-91 and 91-92, these years has different data format than later files

# define a function to get returns with a given row
get_return_by_row <- function(input_row){
  
  if(all(!is.na(input_row))){
    coerced_input_row <- as.numeric(input_row)
    if(!is.na(coerced_input_row[5])){
      
    return(
      c(input_row[1], input_row[2], as.character(input_row[4]), input_row[5])
      )
      }else if(!is.na(coerced_input_row[6])){
          return(
            c(input_row[1], input_row[2], input_row[5], input_row[6])
            )
      }
  }
}




for(txt in txt_path){
  
  read_in_txt <- read.table(txt, header = FALSE, fill = TRUE)
  #                               colClasses = c("integer", "integer", "character", "character", "character", "numeric", "numeric", "numeric"))
  process_this_txt <- apply(read_in_txt, 2, as.character)
  
  #process_this_txt[process_this_txt[, 4] == "Total" & process_this_txt[, 5] == "Migration",]
  txt_dimension <- dim(process_this_txt) 
  origins_index <- which(process_this_txt[, 4] == "Total" | process_this_txt[, 5] == "Total")
  origins_index <- c(origins_index, txt_dimension[1])
  ending_index <- 0
  for(oth in 1:(length(origins_index) - 1)){
    
    # start searching for chunk ends between oth and oth + 1 in origins_index
    chunk_start <- origins_index[oth]
    chunk_end <- origins_index[oth + 1]
    subset_county_chunk <- process_this_txt[chunk_start:chunk_end, ]
    
    for (irow in 1:nrow(subset_county_chunk)) {
      if(subset_county_chunk[irow, 1] == "Same" & subset_county_chunk[irow, 2] == "State"){
        ending_index <- c(ending_index, irow + chunk_start - 1)
        break()
      }else if(subset_county_chunk[irow, 1] == "Region" & subset_county_chunk[irow, 2] == "1:"){
        ending_index <- c(ending_index, irow + chunk_start - 1)
        break()
      }else if(subset_county_chunk[irow, 1] == "All" & subset_county_chunk[irow, 2] == "Migration"){
        ending_index <- c(ending_index, irow + chunk_start - 1)
        break()
      }else if(subset_county_chunk[irow, 3] == "County" & subset_county_chunk[irow, 4] == "Non-Migrants"){
        ending_index <- c(ending_index, irow + chunk_start - 1)
      }else{
        next()
      }
    }
  }
  
  # same_state_index <- which(process_this_txt[, 1] == "Same" & process_this_txt[, 2] == "State")
  # region_index <- which(process_this_txt[,1] == "Region" & process_this_txt[,2] == "1:")
  # all_migration_flows_index <- which(process_this_txt[,1] =="All" & process_this_txt[,2] == "Migration")
  origins_index <- head(origins_index, -1)
  ending_index <- ending_index[-1]
  if(length(origins_index) == length(ending_index)){
    print("destinations fall between these intervals, you are good to continue")
    
    migration_table_for_this_state <- matrix(data = NA, ncol = 6, nrow = 1)
    
    for(a in 1:length(origins_index)){
      
      destination_interval <- process_this_txt[origins_index[a]:(ending_index[a] - 1), 1:8]     
      if((ending_index[a] - origins_index[a]) > 2) {
        state_county_fips <- destination_interval[, 1:2]
        state_county_fips_num <- apply(state_county_fips, 2, as.numeric)
        destination_interval_omitna <- destination_interval[complete.cases(state_county_fips_num),]
      
         # <- destination_interval_omitna <- na.omit(destination_interval)
        destination_domestic <- subset(destination_interval_omitna, destination_interval_omitna[,3]!="Foreign")
        origin_state <- as.numeric(as.character(destination_domestic[1,1]))
        origin_county <- as.numeric(as.character(destination_domestic[1,2]))
        num_destinations <- dim(destination_domestic)[1]
        desti_state <- as.numeric(as.character(destination_domestic[2:num_destinations, 1]))
        desti_county <- as.numeric(as.character(destination_domestic[2:num_destinations, 2]))
        num_returns <- do.call(rbind, apply(destination_domestic, 1, get_return_by_row))
        
        table_ori_dest_return <- cbind(rep(origin_state, dim(num_returns)[1]), rep(origin_county, dim(num_returns)[1]), num_returns)
        migration_table_for_this_state <- rbind(migration_table_for_this_state, table_ori_dest_return)
      }else{
        print("no enough migration")
        next()
      }
      
    }
    
    
  }else{
    print(paste("pls correct intervals for", txt, sep = ""))
    next()
  }
  
  migration_table_for_this_state <- migration_table_for_this_state[-1,]
  colnames(migration_table_for_this_state) <- c("y1_state", "y1_county", "y2_state", "y2_county", "y2_state_name", "returns")
  write.table(migration_table_for_this_state, 
              file = paste("E:/Zekun/Project_Sea_Level_Rise/data/processed_migration_table/county_out_flow_9192/",
                           substr(txt, 149, 160), sep = ""), row.names = FALSE, sep = ",")
  
}

# a tool to visually check intervals
for(a in 1:length(origins_index)){
  txt_interval <- process_this_txt[origins_index[a] : same_state_index[a],]
  ind1 <- which(txt_interval$V1 == "Same" & txt_interval$V2 == "State")
  if(length(ind1)>1){
    print(a)
  }
}



#2. the second part will adress migration from 93-04, data are stored as xls files.
require(readxl)
xls_path <- folder_list[3:13]

for(f in xls_path){
  county_in_f <- list.files(f, recursive = T, pattern = glob2rx("C*o.xls"), 
                            full.names = TRUE)
  for(c in county_in_f){
    migration_of_this_state <- read_excel(c, col_names = FALSE, skip = 8)
    colnames(migration_of_this_state) <- c("y1_state", "y1_county", "y2_state", 
                                           "y2_county", "y2_state_name", "dest_detail"
                                          ,"returns", "exemption", "total_income")
    
  }
}