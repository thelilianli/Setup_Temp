##########################################################################################
# one hot vectors
##########################################################################################
char <- which(sapply(master,is.character))
master[,char][is.na((master[,char]))] <- "-"
master[,char][master[,char] == ""] <- "-"

  onehot <- function(dt,onehotcols){
    for(i in onehotcols){
      newcol <- unique(dt[[i]])
      for(ii in newcol){
        iiname <- paste0("OH_",i,"_",ii)
        dt[[iiname]] <- ifelse(dt[[i]] %in% ii,1,0)
        print(paste0("CREATED ONE-HOT ",iiname))
      }
    }
    return(dt)}

#one hot vecors
onehotcols <-c("field_name1","field_name2")

master <- onehot(master,onehotcols)
names(master) <- gsub(" |>|<","",names(master))
master[sapply(master, is.character)] <- lapply(master[sapply(master, is.character)],
                                               as.factor)

##########################################################################################
# imputation of missing values (numeric only)
##########################################################################################
# excluded fields
date_fields <- unique(c(grep('DATE', names(master), ignore.case = TRUE, value=TRUE),
                        grep('ym', names(master), ignore.case = TRUE, value=TRUE),
                        grep('year', names(master), ignore.case = TRUE, value=TRUE),
                        grep('month', names(master), ignore.case = TRUE, value=TRUE)))

id_fields <- unique(c(grep('_ID', names(master),ignore.case = TRUE,value=TRUE),
                      grep('_IAD', names(master),ignore.case = TRUE,value=TRUE),
                      grep('name', names(master), ignore.case = TRUE, value=TRUE),
                      grep('MORTGAGE_NUM', names(master), ignore.case = TRUE, value=TRUE),
                      grep('LOAN_NUM', names(master), ignore.case = TRUE, value=TRUE)))

oh_fields <- grep('OH_', names(master),ignore.case = TRUE,value=TRUE)#one hot fields

Num <- which(sapply(master,is.numeric))
sort(sapply(master[,Num], function(x) round(sum(is.na(x))/nrow(master),2)), decreasing = TRUE) #checking for missing rate

excl <- c("excl_field_name1","excl_field_name2",date_fields,id_fields,oh_fields,"Target")

# #method 1: median imputation
num_impute <- names(Num)[!(names(Num) %in% excl)]

  #inputation by median
  for(fn in num_impute){
    #imputation using median for numeric values
    if(sum(is.na(master[[fn]]))>0){
      print(paste0("IMPUTING ",fn))
      print(paste0("nrows: ",sum(is.na(master[[fn]]))," w ",median(master[[fn]][!is.na((master[[fn]]))])))
      master[[paste0("MIS_",fn)]]<- ifelse(is.na((master[[fn]])),1,0)
      master[,fn][is.na((master[[fn]]))] <- median(master[[fn]][!is.na((master[[fn]]))])
    }
  }

# # method 2: imputatuion using 0 for missing values
# zero_impute <- c("field_name1","field_name2")
# master[,zero_impute][is.na(master[,zero_impute])] <- 0
# 
# # method 3: imputation of remaining missing with drastically different value
# master[,Num][is.na(master[,Num])] <- -99999

# method 4: if missing = unreliable record then remove for master before modelling

##########################################################################################
# standardization/normalization of values (numeric only)
##########################################################################################
# normalization typically rescales values to a range of [0,1]
# standardization typically rescales values to have a mean of 0 
# using standardization
# scale function is built in (true for center and scale to have a z score standardization)

# master_raw <- master #saving snapshot before standardization

print("STANDARDIZING NUMERIC FIELDS: ")  
print(num_impute)
master[,num_impute] <- scale(master[,num_impute],center = TRUE, scale = TRUE)