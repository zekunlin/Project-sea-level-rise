library(data.table)
library(readxl)

hauers_table <- fread("E:/Zekun/Project_Sea_Level_Rise/data/Population_Migration/IRS-County-to-County-Migration-Data--1990-2013-copy.txt",
                      stringsAsFactors = F, check.names = F)

migration_table <- read.csv("E:/Zekun/Project_Sea_Level_Rise/data/migration_table_v3.csv", 
                            sep = ",", stringsAsFactors = FALSE)
statefips <- 1:56
statefips <- statefips[-c(3, 7, 14, 43, 52)]
for(st in statefips){
  
  origin_vector <- as.numeric(migration_table$origin)
  migration_of_state <- migration_table[which(origin_vector %/% 1000 == st), ]
  non_migrants_index <- which(migration_of_state$origin == migration_of_state$destination)
  migration_of_state <- migration_of_state[-non_migrants_index, ]
  cumulative_migrations <- rowSums(migration_of_state[, 4:29])
  state_cumulative <- cbind(migration_of_state[, 1:3], cumulative_migrations)
  num_of_origins <- unique(migration_of_state$origin %% 1000)
  num_of_dests <- sort(unique(migration_of_state$destination))
  
  migration_matrix <- matrix(0, nrow = length(num_of_origins), 
                             ncol = length(num_of_dests), 
                             dimnames = list(num_of_origins, num_of_dests))
  
  for(x in num_of_origins){
    x_outgoing <- state_cumulative[which(state_cumulative$origin %% 1000 == x), ]
    i <- which(rownames(migration_matrix) == x)
    j <- 0
    for(b in 1:length(x_outgoing$destination)){
      y = x_outgoing$destination[b]
      j <- which(as.numeric(colnames(migration_matrix)) == y)
      migration_matrix[i, j] <- x_outgoing[x_outgoing$destination==y, ]$cumulative_migrations
    }
    
  }
  
  write.table(x = migration_matrix, 
              file = paste("E:/Zekun/Project_Sea_Level_Rise/data/state_migration_matrix/","state_fips_",
                           st, ".csv", sep = ""),
              sep = ",", row.names = TRUE, col.names = TRUE)
  
  migration_prob <- migration_matrix
  
  county_total_outgoing <- rowSums(migration_matrix, na.rm = TRUE)
  for(krow in 1:dim(migration_matrix)[1]){
    migration_prob[krow,] <- migration_matrix[krow,] / county_total_outgoing[krow]
  }
  write.table(x = migration_prob, 
              file = paste("E:/Zekun/Project_Sea_Level_Rise/data/state_migration_probability/", "state_fips_",
                           st, "_probability.csv", sep = ""), sep = ",", row.names = TRUE, col.names = TRUE)
}
