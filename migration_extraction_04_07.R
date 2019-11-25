# 3. this part will process migrations between 04 to 07
require(readxl)

folder_list <- list.files("E:/Zekun/Project_Sea_Level_Rise/data/us_revenue_migration_data/", 
           pattern = glob2rx("county0*"), full.names = TRUE)

xls_path <- folder_list[4]

for (x in xls_path){
  which_year <- paste("20", substr(x, nchar(x) - 3, nchar(x) -2), "to20",
                      substr(x, nchar(x) - 1, nchar(x)), "countymigration", sep = "")
  dir_path <- paste("E:/Zekun/Project_Sea_Level_Rise/data/processed_migration_table/", 
                    which_year, sep = "")
  if(!dir.exists(dir_path)){
    dir.create(dir_path)
  }
  
  county_in_f <- list.files(x, recursive = T, pattern = glob2rx("co*o*.xls"), 
                            full.names = TRUE)
  #county_in_f <- county_in_f[-1]    this line only work on 07-08
  short_path_name <- list.files(x, recursive = T, pattern = glob2rx("co*o*.xls"),
                           full.names = FALSE)
  for(f in county_in_f){
    
    state_name <- substr(f, nchar(f) - 12, nchar(f) - 4)
    state_migration <- read_excel(f, col_names = FALSE, skip = 8)
    colnames(state_migration) <- c("y1_state", "y1_county", "y2_state", 
                                           "y2_county", "y2_state_name", "dest_detail"
                                           ,"returns", "exemption", "total_income")
    migration_within_56_states <- subset(state_migration, y2_state <= 56)
    
    write.csv(migration_within_56_states, file = paste(dir_path, "/", state_name, ".csv", sep = ""))
  }
}
