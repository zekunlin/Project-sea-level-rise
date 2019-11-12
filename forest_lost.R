library(raster)
library(data.table)
library(rgdal)
library(parallel)

#### compute how much forest lost caused by urbanization
nlcd11 <- raster("E:/slr_project_data/NLCD2011_LC_North_Carolina/nlcd_2011_wmt.tif")
simu.lst <- list.files("E:/grassdata/futures_triangle_files/simulation_12(inundate)/", 
                           pattern = ".tiff", full.names = T)
cnty.shp <- readOGR("E:/slr_project_data/wmt_counties/nc_counties2t.shp")

forest.mask.11 <- raster("E:/grassdata/futures_triangle_files/simulated_raster/forest_2011.tiff")
forest.perc.11 <- freq(forest.mask.11, value=1)/24435810

ag.mask.11 <- raster("E:/grassdata/futures_triangle_files/simulated_raster/agri_2011.tiff")
ag.perc.11 <- freq(ag.mask.11,value=1)/24435810

#compute real forest lost:
#nlcd.01 <- raster("E:/slr_project_data/NLCD2001_LC_N")
nlcd.06 <- raster("E:/slr_project_data/NLCD2006_LC_North_Carolina/nlcd_2006_wmt.tif")

forest.mask.06 <- raster("E:/grassdata/futures_triangle_files/simulated_raster/forest_2006.tiff")
forest.perc.06 <- freq(forest.mask.06, value=1)/24435810

ag.mask.06 <- raster("E:/grassdata/futures_triangle_files/simulated_raster/agri_2006.tiff")
ag.perc.06 <- freq(ag.mask.06,value=1)/24435810

# forest.mask.01 <- nlcd.01
# forest.mask.01[forest.mask.01 < 41 | forest.mask.01 > 43] <- NA
# forest.mask.01[!is.na(forest.mask.01)] <- 1
# 
# ag.mask.01 <- nlcd11
# ag.mask.01[ag.mask.01 < 81 | ag.mask.01 > 82] <- NA
# ag.mask.01[!is.na(ag.mask.01)] <- 1



###### how much forest is converted to urban?
chg.forest <- forest.mask.06
chg.forest[forest.mask.11==1] <- NA
urbnizd.forest <- chg.forest * ref.ra

###### how much agriculture isconverted to urban?
chg.agri <- ag.mask.06
chg.agri[ag.mask.11==1] <- NA
urbnizd.ag <- chg.agri * ref.ra

urbnizd.ag[urbnizd.ag==1] <- 2
cmbnd.chg <- urbnizd.ag + urbnizd.forest
plot(cmbnd.chg, col=c("grey","green", "red"))

forest = vector()
agriculture = vector()
#other = vector()

#n.cores <- detectCores() - 2
#cl <- makeCluster(n.cores, type = "PSOCK")   # go parallel
for (r in simu.lst) {
  simu_ra <- raster(r)
  simu_r1 <- simu_ra
  simu_r1[simu_r1<=50 & simu_r1 >0]=1   # development at YEAR 2050
  simu_r1[simu_r1!=1]=0
  
  # the num of pixels of changed forest due to dev
  fore <- freq(forest.mask * simu_r1, useNA="no", value=1) / freq(forest.mask, value=1)
  
  # the num of pixels of changed agriculture due to dev
  agr <- freq(ag.mask * simu_r1, useNA="no", value=1) / freq(ag.mask, value=1)
  
  forest=c(forest,fore)
  agriculture=c(agriculture,agr)
  
}
#stopCluster()

changed.land <- data.frame(forest=forest, agri=agriculture)
changed.land.inun <- data.frame(inundant.forest=forest, inundate.agri=agriculture)
changed.land
boxplot(changed.land, outline=F, main="Scenario 1, the amount of changed land (percentage)")
barplot(changed.land.inun, height = as.matrix(changed.land.inun))
write.csv(changed.land, 
          file="E:/grassdata/futures_triangle_files/scenario1_changedland.csv",
          sep = ",")


#########################################################################################
inundate.simu <- raster(
  "E:/grassdata/futures_triangle_files/simulation_12(inundate)/wmt_s12_inundate.tiff")
urb.06 <- raster("E:/grassdata/futures_triangle_files/simulated_raster/urban_2006.tiff")
cnty.shp <- readOGR("E:/slr_project_data/wmt_counties/nc_counties2t.shp")
######## plot inundate simu
inundate.simu[urb.06==1] <- 0
inundate.simu[inundate.simu>0] <- 1
plot(inundate.simu, col=c("white", "grey", "red3"), axes=F, legend=F)
plot(cnty.shp, col="transparent", border="black",add=T)
text(cnty.shp, cnty.shp$NAME, cex=0.8)
#############################################
######## plot redistribution simu
redistri.simu <- raster(
  "E:/grassdata/futures_triangle_files/simulation_13(redistribution/wmt_s13_redistribution.tiff"
)
redistri.simu[urb.06==1] <- 0
redistri.simu[redistri.simu >0] <- 1
plot(redistri.simu, col=c("white", "grey", "red3"), axes=F, legend=F)
plot(cnty.shp, col="transparent", border="black",add=T)
text(cnty.shp, cnty.shp$NAME, cex=0.8)
legend("bottomleft", fill=c("grey", "red3"), 
       legend = c("baseline development", "new development"), cex=0.8)
#############################################
### plot normal urbanization
roads <- readOGR(dsn = "E:/slr_project_data/main_roads/main_roads/main_roads/myroads.shp")
roads.c <- crop(x = roads,y = cnty.shp)
usual.simu <- raster(
  "E:/grassdata/futures_triangle_files/simulation_11/simulated_raster/wmt_s11_run14.tiff"
)

usual.simu[urb.06] <- 0
usual.simu[usual.simu==1] <- 0
usual.simu[usual.simu> 0 ] <- 1
plot(usual.simu, col=c("white", "grey", "red"), axes=F, legend=F)
plot(cnty.shp, col="transparent", border="black",add=T)
#plot(roads, col="darkblue", add=T, lwd=0.01, lty=3, )
text(cnty.shp, cnty.shp$NAME, cex=0.8)
legend("bottomleft", fill=c("grey", "red"), 
       legend = c("baseline development", "new development"), cex=0.8)

###calculate forest and agriculture lost, urban extent for each county
shp.17 <- cnty.shp[cnty.shp@data$COUNTYFP=="017",]
shp.47 <- cnty.shp[cnty.shp@data$COUNTYFP=="047",]
shp.19 <- cnty.shp[cnty.shp@data$COUNTYFP=="019",]
shp.129 <- cnty.shp[cnty.shp@data$COUNTYFP=="129",]
shp.141 <- cnty.shp[cnty.shp@data$COUNTYFP=="141",]

usual.simu.mask <- usual.simu
usual.simu.mask[usual.simu.mask!=1] <- NA
usual.forestloss <- usual.simu.mask*forest.mask.06
usual.agloss <- usual.simu.mask * ag.mask.06
forest.06.freq <- freq(forest.mask.06,value=1)
ag.06.freq <- freq(ag.mask.06,value=1)
usual.forestloss.freq <- freq(usual.forestloss, value=1)/forest.06.freq *100
usual.agloss.freq <- freq(usual.agloss, value=1)/ag.06.freq *100

inun.mask <- inundate.simu
inun.mask[inun.mask!=1] <- NA
inun.forestloss <- inun.mask*forest.mask.06
inun.forestloss.freq <- freq(inun.forestloss, value=1)/forest.06.freq*100
inun.agloss.freq <- freq(inun.mask * ag.mask.06, value=1)/ag.06.freq*100

redis.mask <- redistri.simu
redis.mask[redis.mask!=1] <- NA
redis.forestloss <- redis.mask * forest.mask.06
redis.forestloss.freq <- freq(redis.forestloss,value=1)/forest.06.freq*100
redis.agloss.freq <- freq(redis.mask * ag.mask.06, value=1)/ag.06.freq*100


#####calculate urban extension for each county
redis.17 <- mask(redistri.simu, shp.17)
usual.17 <- mask(usual.simu, shp.17)
inun.17 <- mask(inundate.simu, shp.17)

plot(redis.17, axes=F, legend=F, frame=F, col=c("grey", "yellow4", "red"), box=F)
plot(usual.17, axes=F, legend=F, frame=F, col=c("grey", "yellow4", "red"), box=F)
plot(inun.17, axes=F, legend=F, frame=F, col=c("grey", "yellow4", "red"), box=F)

redis.129 <- mask(redistri.simu, shp.129)
usual.129 <- mask(usual.simu, shp.129)
inun.129 <- mask(inundate.simu, shp.129)


redis.141 <- mask(redistri.simu, shp.141)
usual.141 <- mask(usual.simu, shp.141)
inun.141 <- mask(inundate.simu, shp.141)

redi.freq <- freq(redis.141, value=1)/sum(freq(redis.141)[,2])

# plot(redis.141, axes=F, legend=F, frame=F, col=c("grey", "yellow4", "red"), box=F)
# plot(usual.141, axes=F, legend=F, frame=F, col=c("grey", "yellow4", "red"), box=F)
# plot(inun.141, axes=F, legend=F, frame=F, col=c("grey", "yellow4", "red"), box=F)
# 
bartext <- c("Scenario1", "Scenario1", "Scnenario2", "Scenario2", "Scenario3", "Scenario3")
barplot(c(usual.forestloss.freq, usual.agloss.freq, inun.forestloss.freq, inun.agloss.freq, 
          redis.forestloss.freq, redis.agloss.freq), 
        col=c("green4", "yellow4","green4", "yellow4","green4", "yellow4"),
        names.arg=bartext, ylab="percentage", ylim=c(0,2))
legend("topleft", fill=c("green4", "yellow4"), legend=c("forest", "agricultural"))
