#run job

#clear environment
rm(list=ls()) ; options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx4g")); 

##########################################################################################
#initializing
##########################################################################################

#determining the latest scripts to call
main_path <- paste0("//path/Channel/ALL/")

latest_ym <- list.files(path = WFMT, pattern = "ALL_EMP.RDS", all.files = FALSE,
                             full.names = FALSE, recursive = TRUE,
                             ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
latest_ym <- latest_ym[grepl("Ingestion/ALL_EMP",latest_ym,ignore.case = T)] #keeping only relevant str pattern
latest_ym <- setdiff(as.numeric(substr(latest_ym,1,6)),c(NA,"")) #str pattern - isolating yyyymm

latest_ym0 <- ifelse(as.numeric(as.character(format(Sys.Date(),format = "%d")))%in% 1,
                    as.numeric(as.character(format(Sys.Date(),format = "%Y%m"))),
                    as.numeric(as.character(format(as.Date(as.character(format(Sys.Date(),format = "%Y%m01")),"%Y%m%d")-1,format = "%Y%m"))))
latest_ym <- latest_ym[latest_ym <= latest_ym0]

latest_ym <- max(latest_ym,na.rm = TRUE)

#outfile path and outfile name
yyyymm<- paste0("_",latest_ym)#_yyyymm (latest YM)
print(paste0("Using latest from: ",yyyymm))

mdy_date <- as.Date(paste0(gsub("_","",yyyymm),"01"),"%Y%m%d")
Business_DateMMMyyyy <- toupper(as.character(format(mdy_date,"%b%Y")))

#model folder directory path
latest_f <- paste0("//path/Channel/Mobile/") #PATH

# code directory path
latest_s <- paste0(latest_f,gsub("","",yyyymm),"/","Scripts/")

#Helpers
source(paste0(latest_s,"install_packages.R")) 
source(paste0(latest_s,"helper_func.R"))
source(paste0(latest_s,"helper_unpac.R"))

#reference table directory
rt <- paste0(latest_f,gsub("_","",yyyymm),"/","Reference Tables/") 

rtLst <- list.files(path = rt, pattern = ".csv", all.files = FALSE,
                         full.names = FALSE, recursive = FALSE,
                         ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

#output folder
latest_o <- paste0(latest_f,gsub("","",yyyymm),"/","Outputs/")
dir.create(latest_o)
latest_o <- paste0(latest_o,"run 1/")
dir.create(latest_o)

##########################################################################################
#master script to run job
##########################################################################################  

source(paste0(latest_s,"Master.R")) #placeholder for master script