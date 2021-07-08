#ADAPTING SOPHIE'S CODE

#installing packages and setting up libraries
install.packages("dlnm")
install.packages("Epi")
install.packages("gnm")
install.packages("googledrive")

yeslibrary(parallel)
library(dplyr)
library(sf)
library(raster)

#read in case data (clean_cocci_data.R)
str(cases)

#establish hazard length (another option is 60 - from sophie's study)
hazard_length <- 42

#create function to get control periods (sophies for now)
getControlPeriods <- function(i) {
  cs <- cases[i,]
  dates <- cs$est_date - (1:4 * 365)
  ndates <- length(dates)
  dates <- dates[dates != cs$est_date]
  return(data.frame("CID" = rep(cs$CID, ndates), 
                    "date" = dates, 
                    "case" = 0))
}

#run creation of control periods parallel
cl <- makeCluster(3, setup_strategy = "sequential")
clusterExport(cl, varlist = c("getControlPeriods", "cases"))
tmp <- parLapply(cl, 1:nrow(cases), getControlPeriods) # 30s to run
stopCluster(cl)

#compile look up what do.call means
begin <- Sys.time()
controls <- do.call(rbind, tmp)
end <- Sys.time()
print(end - begin) # 1.5 mins to run (double check this)

#write function for ambidirectional control periods (ideally want to do time stratified)
cp_test <- function(i){
  cs <- cases[i,]
  dates <- c(cs$date - (1:2 * 365), cs$date + (1:2 * 365)) #this line does something a little weird - double check
  ndates <- length(dates)
  dates <- dates[dates != cs$date]
  return(data.frame("CID" = rep(cs$CID, ndates), 
                    "date" = dates, 
                    "case" = 0))
}

#test with your new function (works)
cl <- makeCluster(3, setup_strategy = "sequential")
clusterExport(cl, varlist = c("cp_test", "cases"))
tmp <- parLapply(cl, 1:nrow(cases), cp_test) # 30s to run
stopCluster(cl)

#compile look up what do.call means
begin <- Sys.time()
controls <- do.call(rbind, tmp)
end <- Sys.time()
print(end - begin) # 1.49 min to run

#bind the cases and control periods together
casemat <- rbind(cases %>%
                   dplyr::select(CID, date) %>%
                   mutate(case = 1), 
                 controls)

casemat$CID <- as.numeric(casemat$CID)

case_control <- casemat %>%
  arrange(CID)

#get exposure data - what size hexagon do you want to use? need dummy dataset and other datasets for confounders
drive_find(n_max = 30)

drive_download("test_R", type = "csv")
