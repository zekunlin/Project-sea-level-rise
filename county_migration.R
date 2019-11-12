library(data.table)
library(stringr)
library(raster)
library(rgdal)

cntyAffected <- fread("E:/slr_project_data/counties_affected_6ftslr.txt", 
                      check.names = F, stringsAsFactors = F)
AffectedFIPS <- cntyAffected$COUNTYFP
wmtFIPS <- c(17, 19, 47, 129, 141)
migdatapath <- "E:/slr_project_data/us_revenue_migration_data/"     # this is the path to migra data
yr_serie <- c("08", "09", seq(10,16,1))

namein <- c("State_Code_Dest",  "County_Code_Dest", "State_Code_Origin", "County_Code_Origin",
           "State_Abbrv" ,   "County_Name",    "Return_Num",   "Exmpt_Num",  "Aggr_AGI") 
nameout <- c( "State_Code_Origin", "County_Code_Origin", "State_Code_Dest", "County_Code_Dest",
            "State_Abbrv" ,  "County_Name" ,  "Return_Num",   "Exmpt_Num" ,  "Aggr_AGI")
cnty_migin <- data.frame()
cnty_migout <- data.frame()

table129.47 <- data.frame()
table141.47 <- data.frame()
table19.47 <- data.frame()
table129.17 <- data.frame()
table141.17 <- data.frame()
table19.17 <- data.frame()

for (yr in 1:8) {
  # read in data of migraion flows in and out
  migin <- fread(paste(migdatapath, "countyinflow", yr_serie[yr], yr_serie[yr+1], ".csv",
                      sep = ""), check.names = F, stringsAsFactors = F)
  migout <- fread(paste(migdatapath, "countyoutflow", yr_serie[yr], yr_serie[yr+1], ".csv"
                        , sep = ""), check.names = F, stringsAsFactors = F)
  
  if(as.numeric(yr_serie[yr])<=10){
    #migWmt <-  migin[, .(State_Code_Dest==37 & County_Code_Dest %in% wmtFIPS),]
    migtoNc <- migin[migin$State_Code_Dest==37 & migin$County_Code_Dest %in% AffectedFIPS & 
                       migin$State_Code_Origin==97 & migin$County_Code_Origin==0,]
    migFromNc <- migout[migout$State_Code_Origin==37 & migout$County_Code_Origin %in% AffectedFIPS&
                          migout$State_Code_Dest==97 & migout$County_Code_Dest==0,]
    
    mig129.47 <- migin[migin$State_Code_Dest==37 & migin$County_Code_Dest==47 &
                         migin$State_Code_Origin == 37 & migin$County_Code_Origin == 129,]
    mig141.47 <- migin[migin$State_Code_Origin==37 & migin$State_Code_Dest==37 & 
                         migin$County_Code_Origin ==141 & migin$County_Code_Dest ==47,]
    mig19.47 <- migin[migin$State_Code_Origin==37 & migin$State_Code_Dest==37 & migin$County_Code_Origin ==19 & 
            migin$County_Code_Dest ==47,]
    
    mig129.17 <- migin[migin$State_Code_Origin==37 & migin$State_Code_Dest==37 & migin$County_Code_Origin ==129 & 
            migin$County_Code_Dest ==17,]
    mig141.17 <- migin[migin$State_Code_Origin==37 & migin$State_Code_Dest==37 & migin$County_Code_Origin ==141 & 
            migin$County_Code_Dest ==17,]
    mig19.17 <- migin[migin$State_Code_Origin==37 & migin$State_Code_Dest==37 & migin$County_Code_Origin ==19 & 
            migin$County_Code_Dest ==17,]
  }else{
    # migWmt <-  migin[y2_statefips==37 & y2_countyfips %in% wmtFIPS & y1_statefips==37 &
    #                    y1_countyfips %in% wmtFIPS,]
    
    migtoNc <- migin[migin$y2_statefips == 37 & migin$y2_countyfips %in% AffectedFIPS &
                       migin$y1_statefips==97 & migin$y1_countyfips==0,]
    migFromNc <- migout[migout$y1_statefips==37 & migout$y1_countyfips %in% AffectedFIPS &
                          migout$y2_statefips ==97 & migout$y2_countyfips==0,]
    
    mig129.47 <- migin[migin$y1_statefips==37 & migin$y2_statefips==37 & migin$y1_countyfips ==129 & 
            migin$y2_countyfips ==47,]
    colnames(mig129.47) <- namein
    
    mig141.47 <- migin[migin$y1_statefips==37 & migin$y2_statefips==37 & migin$y1_countyfips ==141 & 
            migin$y2_countyfips ==47,]
    colnames(mig141.47) <- namein
    
    mig19.47 <- migin[migin$y1_statefips==37 & migin$y2_statefips==37 & migin$y1_countyfips ==19 & 
            migin$y2_countyfips ==47,]
    colnames(mig19.47) <- namein
    
    mig129.17 <- migin[migin$y1_statefips==37 & migin$y2_statefips==37 & migin$y1_countyfips ==129 & 
            migin$y2_countyfips ==17,]
    colnames(mig129.17) <- namein
    mig141.17 <- migin[migin$y1_statefips==37 & migin$y2_statefips==37 & migin$y1_countyfips ==141 & 
            migin$y2_countyfips ==17,]
    colnames(mig141.17) <- namein
    
    mig19.17 <- migin[migin$y1_statefips==37 & migin$y2_statefips==37 & migin$y1_countyfips ==19 & 
            migin$y2_countyfips ==17,]
    colnames(mig19.17) <- namein
    
    colnames(migtoNc) <- namein
    colnames(migFromNc) <- nameout
  }
  
  migtoNc$year <- rep(yr_serie[yr], 25)
  migFromNc$year <- rep(yr_serie[yr], 25)
  
  cnty_migin <- rbind(cnty_migin, migtoNc)
  cnty_migout <- rbind(cnty_migout, migFromNc)
  
  
  table141.17 <- rbind( table141.17, mig141.17)
  table141.47 <- rbind( table141.47, mig141.47)
  
  table129.17 <- rbind( table129.17, mig129.17)
  table129.47 <- rbind( table129.47, mig129.47)
  
  table19.17 <- rbind(table19.17, mig19.17)
  table19.47 <- rbind(table19.47, mig19.47)
  
  
}


wmt_migin <- cnty_migin[cnty_migin$County_Code_Dest %in% wmtFIPS,]
wmt_migout <- cnty_migout[cnty_migout$County_Code_Origin %in% wmtFIPS,]
grossmig <- wmt_migin$Return_Num + wmt_migin$Exmpt_Num - wmt_migout$Return_Num - wmt_migout$Exmpt_Num
wmt_migin$grossmig <- grossmig
bladen <- wmt_migin[wmt_migin$County_Code_Dest == 17]
brunswick <- wmt_migin[wmt_migin$County_Code_Dest == 19]
columbus <- wmt_migin[wmt_migin$County_Code_Dest == 47]
newhanover <- wmt_migin[wmt_migin$County_Code_Dest == 129]
pender <- wmt_migin[wmt_migin$County_Code_Dest == 141]

bladenlm <- lm(bladen$grossmig~as.numeric(bladen$year))
colubuslm <- lm(columbus$grossmig~as.numeric(columbus$year))
brunswicklm <- lm(brunswick$grossmig~as.numeric(brunswick$year))
newhanoverlm <- lm(newhanover$grossmig~as.numeric(newhanover$year))
penderlm <- lm(pender$grossmig~as.numeric(pender$year))

plot(bladen$year, bladen$grossmig, type="point" ,pch=16, col="yellow2", ylab="# of gross migration", 
     xlab="year", ylim=c(-3000,6000), main="Gross Migration Trend of Five Counties", cex = 2)
abline(bladenlm, lwd=1, col="yellow2", lty=2)
points(columbus$year, columbus$grossmig, pch=16, col="green2")
abline(lm(columbus$grossmig~as.numeric(columbus$year)), lwd=1, col="green2",lty=2)
points(brunswick$year, brunswick$grossmig, pch=16, col="blue")
abline(lm(brunswick$grossmig~as.numeric(brunswick$year)), lwd=1, col="blue", lty=2)
points(newhanover$year, newhanover$grossmig, pch=16, col="red")
abline(lm(newhanover$grossmig~as.numeric(newhanover$year)), lwd=1, col="red", lty=2)
points(pender$year, pender$grossmig, pch=16, col="brown")
abline(lm(pender$grossmig~as.numeric(pender$year)), lwd=1, col="brown", lty=2)
legend("topleft", legend=c("Bladen", "Brunswick","Columbus", "New Hanover",
                           "Pender"),col=c("yellow2", "blue", "green2", "red","brown"),pch=16)
###########extrapolate migration with time
bladen.proj.mig <- bladenlm$coefficients[1] + bladenlm$coefficients[2]*seq(1,100,1)
colubus.proj.mig <- colubuslm$coefficients[1] + colubuslm$coefficients[2]*seq(1,100,1)
brunswick.proj.mig <- brunswicklm$coefficients[1] + brunswicklm$coefficients[2] * seq(1,100,1)
newhanov.proj.mig <- newhanoverlm$coefficients[1] + newhanoverlm$coefficients[2]*seq(1,100,1)
pender.proj.mig <- penderlm$coefficients[1] + penderlm$coefficients[2] * seq(1,100,1)
##########################################################################

#########calculate the migration ratio
out.141 <- sum(table141.17$Return_Num + table141.17$Exmpt_Num + 
   table141.47$Return_Num + table141.47$Exmpt_Num)
ratio.141.17 <- sum(table141.17$Return_Num + table141.17$Exmpt_Num)/out.141
ratio.141.47 <- 1 - ratio.141.17

out.129 <- sum(table129.17$Return_Num + table129.17$Exmpt_Num + 
   table129.47$Return_Num + table129.47$Exmpt_Num)
ratio.129.17 <- sum(table129.17$Return_Num + table129.17$Exmpt_Num)/out.129
ratio.129.47 <- 1 - ratio.129.17

out.19 <- sum(table19.17$Return_Num + table19.17$Exmpt_Num + 
   table19.47$Return_Num + table19.47$Exmpt_Num)
ratio.19.17 <- sum(table19.17$Return_Num + table19.17$Exmpt_Num)/out.19
ratio.19.47 <- 1 - ratio.19.17
#####################################################################

## calculate area
wmt.aff.bg <- readOGR(dsn = "E:/slr_project_data/cb_2017_37_bg_500k/wmt_affect_bg_proj.shp")
wmt.bg <- readOGR(dsn = "E:/slr_project_data/cb_2017_37_bg_500k/wmt_bg_2017_proj.shp")


affect.areas <- aggregate(wmt.aff.bg$Area, by=list(County=wmt.aff.bg$COUNTYFP), FUN=sum)
county.areas <- aggregate(wmt.bg$Area, by=list(County=wmt.bg$COUNTYFP), FUN=sum)

affct.ratio <- data.frame(county=affect.areas$County, affectRatio=affect.areas$x/county.areas$x[2:5])
##############################################################

### identify how many migrtions from coastal counties to inland

migin.dt <- as.data.table(migin)
wmt.regional.redist <- migin[migin$y1_statefips==37 & migin$y2_statefips==37 &
                               migin$y1_countyfips %in% wmtFIPS & migin$y2_countyfips %in% wmtFIPS 
                             & migin$y2_countyfips!=migin$y1_countyfips,]
wmt.regional.redist[order(wmt.regional.redist$y2_countyfips),]

pop.proj <- read.csv("E:/grassdata/futures_triangle_files/popproj_to2100_census.csv")

natureGrow <- function(x){
  x.len <- length(x)
  y <- x[2:x.len]
  growth <-  y-x[1:x.len-1]
  return(c(0 ,as.integer(growth)))
}

n.g.129 <- natureGrow(pop.proj$X37129)
n.g.141 <- natureGrow(pop.proj$X37141)
n.g.19 <- natureGrow(pop.proj$X37019)
n.g.17 <- natureGrow(pop.proj$X37017)
n.g.47 <- natureGrow(pop.proj$X37047)

moveout.129 <- n.g.129 * affct.ratio[affct.ratio$county==129, 2]
moveout.141 <- n.g.141 * affct.ratio[affct.ratio$county==141, 2]
moveout.19 <- n.g.19 * affct.ratio[affct.ratio$county=="019", 2]


movein.17 <- moveout.129 * ratio.129.17 + moveout.141 * ratio.141.17 + moveout.19 * ratio.19.17
movein.47 <- moveout.129 * ratio.129.47 + moveout.141 * ratio.141.47 + moveout.19 * ratio.19.47

netPop <- function(v,m,n,k){
  k=v[1]
  for (i in 2:length(v)) {
    
    pi <- k[i-1] + m[i] + n[i]
    k <- c(k, pi)
  }
  return(k)
}
net.pop.17 <- netPop(v = pop.proj$X37017, m=movein.17, n=n.g.17)
net.pop.47 <- netPop(v = pop.proj$X37047, m = movein.47, n=n.g.47)

coastalPop <- function(v, m, n, k){
  k=v[1]
  for (i in 2:length(v)) {
    pi <- k[i-1] - m[i] + n[i]
    k <- c(k, pi)
  }
  return(k)
}

net.pop.129 <- coastalPop(v=pop.proj$X37129, m = moveout.129, n = n.g.129)
net.pop.141 <- coastalPop(v = pop.proj$X37141, m = moveout.141, n = n.g.141)
net.pop.19 <- coastalPop(v = pop.proj$X37019, m =moveout.19, n = n.g.19)

pop.redis <- round(data.frame(year=seq(2001, 2100, 1),"37017" = net.pop.17, "37019" = net.pop.19,
                              "37047" = net.pop.47,"37129" = net.pop.129, "37141"=net.pop.141),0)
write.csv(x=pop.redis, file="E:/grassdata/futures_triangle_files/simulation_13(redistribution/popproj_redistribution.csv",
          sep = ",", row.names = F)
####################################################################################

# mig.019 <- pop.proj$X37019*affct.ratio$affectRatio[affct.ratio$county=="019"]
# mig.129 <- pop.proj$X37129* affct.ratio$affectRatio[affct.ratio$county=="129"]
# mig.141 <- pop.proj$X37141 * affct.ratio$affectRatio[affct.ratio$county=="141"]
# 
# res.017 <- pop.proj$X37017 + (mig.019 + mig.129 + mig.141)/2
# res.047 <- pop.proj$X37047 + (mig.019 + mig.129 + mig.141)/2
# res.019 <- pop.proj$X37019 - mig.019
# res.129 <- pop.proj$X37129 - mig.129
# res.141 <- pop.proj$X37141 - mig.141
# 
# pop.proj$X37017 <- res.017
# pop.proj$X37019 <- res.019
# pop.proj$X37047 <- res.047
# pop.proj$X37141 <- res.141
# pop.proj$X37129 <- res.129


# read data in
#migratCsvAddres <- "E:/slr_project_data/us_revenue_migration_data/countyinflow1516.csv"

nc_cnty <- data.table(y2_countyfips=cntyflowin$y1_countyfips, y2_countyname=cntyflowin$y1_countyname)

# consider the county level redistribution only (only happen inside the county)
nc_cnty_flowin <- cntyflowin[cntyflowin$y1_statefips==37 & cntyflowin$y2_statefips==37,] 
AffecMigrat <- nc_cnty_flowin[nc_cnty_flowin$y1_countyfips %in% cntyAffectedFips | 
                                nc_cnty_flowin$y2_countyfips %in% cntyAffectedFips,]
AffecSumary <- AffecMigrat[AffecMigrat$y1_countyfips == AffecMigrat$y2_countyfips,]
AffecMigratDetail <- AffecMigrat[, AffecMigrat[!AffecMigrat$y1_countyfips == 
                                                 AffecMigrat$y2_countyfips,],]
#id <- as.character(AffecMigratDetail$y1_countyname)
#AffecMigratDetail <- cbind(id,AffecMigratDetail)

outflowAddres <- "E:/slr_project_data/us_revenue_migration_data/countyoutflow1516.csv"
outflow <- fread(outflowAddres)
nc_outflow <- outflow[outflow$y1_statefips==37 & outflow$y2_statefips==37,] 
affectOutflow <- nc_outflow[nc_outflow$y1_countyfips %in% cntyAffectedFips | nc_outflow$y2_countyfips
                            %in% cntyAffectedFips,]
affectOutflowDetail <- affectOutflow[, affectOutflow[!affectOutflow$y1_countyfips == 
                                                       affectOutflow$y2_countyfips,],]

y2_names <- data.table(y2_countyname=affectOutflowDetail$y2_countyname, 
                       y2_countyfips=affectOutflowDetail$y2_countyfips)

y2_names <- y2_names[order(y2_names$y2_countyfips),]
AffecMigratDetail <- AffecMigratDetail[order(AffecMigratDetail$y2_countyfips),]
table2write <- data.frame(AffecMigratDetail, y2_countyname=as.character(y2_names$y2_countyname))
id=as.character(table2write$y2_countyname)
table2write <- cbind(id, table2write, group=links_table$group)
table2write$y2_countyname <- str_remove_all(table2write$y2_countyname, " County")
table2write$y1_countyname <- str_remove_all(table2write$y1_countyname, " County")
fwrite(table2write, file = "E:/slr_project_data/nc_revenue_migrat_cnty_6ftslr.csv",
      col.names = T, row.names = F, sep = ",")


############### plot projected population and migration 
plot(pop.proj$year,pop.proj$X37017, type="o", xlab="Year", ylab="Population", ylim=c(30000, 200000))
line(pop.proj$X37019)
