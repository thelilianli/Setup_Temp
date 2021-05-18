#time period
mylst_acct <- c(substr(mmmddyyyy,2,7),
                prev_ym(substr(mmmddyyyy,2,7),1),
                prev_ym(substr(mmmddyyyy,2,7),2),
                prev_ym(substr(mmmddyyyy,2,7),3),
                prev_ym(substr(mmmddyyyy,2,7),4))

##########################################################################################
#loop through and consolidate datasets
##########################################################################################
new_cols <- NULL
datalist = list()
i <- 1

for (j in mylst_acct){
  print(paste0("starting",j))
  dat <-readRDS(paste0(dd,j,"/Raw Data/","data_consolidated_",j,".RDS"))
  dat <- as.data.frame(dat)
  #dat <-dat[!duplicated(dat),]
  
  dat[sapply(dat, is.factor)] <- lapply(dat[sapply(dat, is.factor)], 
                                        as.character)
  
  #insert any transformations or filter conditions here
  
  datalist[[i]] <- dat # add it to your list
  i<- i+1
  
  #get list of MoM new field updates
  if (!exists("new_cols")){
    new_cols <- colnames(dat)
  } else {
    new_cols <- unique(c(new_cols,colnames(dat)))                                        
  }
  
  print(nrow(dat))
}

#plug in unavailable values for new field in historic months
for (i in 1:length(datalist)){
  dat <- datalist[[i]]
  add_cols <- setdiff(new_cols,colnames(dat))
  for(j in add_cols){
    dat[,j] <- NA
  }
  datalist[[i]] <- dat
}


fulldata <-  do.call(rbind, datalist)
fulldata <- fulldata[order(fulldata$order,decreasing = TRUE),]#sorting records for dedup
fulldata <- fulldata[!duplicated(fulldata$id),] #dedupping by latest order