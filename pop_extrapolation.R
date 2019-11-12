# 1. process census data to get 2000 to 2017 estimates then compare that with NC OSBM data
pop.10.17 <- read.csv("E:/slr_project_data/nc_pop_data/PEP_2017_PEPANNRES_with_ann.csv", 
                      header = F, stringsAsFactors = F)
pop.00.10 <- read.csv("E:/slr_project_data/nc_pop_data/co-est00int-01-37.csv", header = F, 
                      stringsAsFactors = F)

head(pop.10.17)
class(pop.10.17)
pop.header1 <- pop.10.17[1,]
pop.header1.new <- c('Id1', 'Id2', 'Geography', '2010Apr4','2010base','2010', '2011', '2012', 
                     '2013', '2014', '2015', '2016', '2017')
colnames(pop.10.17) <- pop.header1.new # the 2010 to 2017 population are esitimated on 2010 
                                       #april base population, the hand book is here:
#https://www2.census.gov/programs-surveys/popest/technical-documentation/methodology/2010-2017/2017-natstcopr-meth.pdf?#
pop.10.17 <- pop.10.17[-1,]

class(pop.00.10)
head(pop.00.10)
pop.header2 <- c("Geography", "2000Apr1", "2000", "2001", '2002', '2003', '2004', '2005', '2006', 
                 '2007', '2008', '2009', '2010Apr1','2010Jul1')
pop.00.10 <- pop.00.10[-c(1:3),]
colnames(pop.00.10) <- pop.header2

# try to merge to get 00 to 17 population
library(stringr)
fips_wanted <- c(37141, 37129, 37019,	37047,	37017)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

get.all.pop <- function(fips, pop00, pop10){
  pop10.vec <- pop10[pop10$Id2 == fips,]
  cnty.name <- pop10.vec$Geography #name follow the pattern of 'countyname. North Carolina'
  cnty.name <- str_replace(cnty.name, pattern = ". North Carolina",replacement = "")
  pop00.vec <- pop00[pop00$Geography==paste(".",cnty.name, sep = ""),] 
  pop2 <- pop00.vec[which(colnames(pop00.vec) == "2000"): which(colnames(pop00.vec) == "2009")]
  pop.return <- c(pop10.vec[2], cnty.name, as.numeric(gsub(",", "", pop2)),
                           as.numeric(pop10.vec[6:13]))
  pop.return <- as.vector(unlist(pop.return))
  return(pop.return)
}

pop.selected.cnty <- rbind(get.all.pop(fips_wanted[1], pop.00.10, pop.10.17),
      get.all.pop(fips_wanted[2], pop.00.10, pop.10.17),
      get.all.pop(fips_wanted[3], pop.00.10, pop.10.17),
      get.all.pop(fips_wanted[4], pop.00.10, pop.10.17),
      get.all.pop(fips_wanted[5], pop.00.10, pop.10.17))

pop.trend <- data.frame(p1 = as.numeric(pop.selected.cnty[1, 3:20]),
                        p2 = as.numeric(pop.selected.cnty[2, 3:20]),
                        p3 = as.numeric(pop.selected.cnty[3, 3:20]),
                        p4 = as.numeric(pop.selected.cnty[4, 3:20]),
                        p5 = as.numeric(pop.selected.cnty[5, 3:20])
           )
colnames(pop.trend) <- fips_wanted           

# now we predict population from the pop.trend
pop.pred.mat <- NULL
yrs <- 2000:2100
time_period=2000:2017
for (i in c(1,2,3)) {
  pop_i <- data.frame(time=2000:2017, pop=pop.trend[,i])
  pop.ss <- nls(pop.trend[,i]~SSlogis(time_period, phi1, phi2, phi3), 
                data=pop_i)
  alpha <- coef(pop.ss)
  pred2 <- alpha[1]/(1 + exp(-(yrs - alpha[2])/alpha[3]))
  pop.pred.mat <- cbind(pop.pred.mat, pred2)
}


pop.trend[,4:5]
# here I used  the maximum population at these two counties untill now, becasue 
# they show decreasing trend after reached the maximum values, and devloped areas
# will not get back to undeveloped areas.
pop4 <- c(pop.trend$`37047`[1:10],rep(max(pop.trend[,4]), 91))
pop5 <- c(pop.trend$`32270`[1:11],rep(max(pop.trend[,5]), 90))

pop.pred.mat <- cbind(pop.pred.mat, pop4, pop5)
colnames(pop.pred.mat) <- fips_wanted
pop.pred.mat <- round(pop.pred.mat, 0)
write.csv(pop.pred.mat, 
          file = "E:/grassdata/futures_triangle_files/pop_proj_to2100_wilmington.csv")

# mod1 <- lm(pop.trend[,4]~poly(time_period,12))
# predicted.intervals <- predict(mod1, data.frame(x=time_period),
#                                interval = "confidence", level=0.99)
# 
# polymode1 <- coef(mod1)
# 
# pred3 <- polymode1[1] + polymode1[2]*yrs + polymode1[3]*yrs^2 + polymode1[4]*yrs^3+
#   polymode1[5]*yrs^4+polymode1[6]*yrs^5 + polymode1[7]*yrs^6 +polymode1[8]*yrs^7+
#   polymode1[9]*yrs^8+polymode1[10]*yrs^9 + polymode1[11]*yrs^10 + polymode1[12]*yrs^11+
#   polymode1[13]*yrs^12
