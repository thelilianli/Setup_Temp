C0_Master_Raw<-readRDS(paste0(DoPath,masterf))#update to latest file
C0_Master <- C0_Master_Raw
C0_Master <- as.data.frame(C0_Master)

print("TARGET SUMMARY :")
print(ftable(C0_Master$Target))

##########################################################################################
# separating training and testing datasets
##########################################################################################
test <- C0_Master
train <- C0_Master
#creating a suitable training dataset for modelling on rare cases
C0_risky <- subset(C0_Master,Target==1)
C0_nonrisk <- subset(C0_Master,Target==0)
C0_nonrisk1 <- C0_nonrisk[sample(nrow(C0_nonrisk), NROW(C0_risky)*3), ]
train_raw <- as.data.frame(rbind(C0_risky,C0_nonrisk1))
train_raw <- as.data.frame(unclass(train_raw))

#alternative method
# train$Target <- as.factor(train$Target) #required for smote
# train <- SMOTE(Target ~ ., train, perc.over = 200, perc.under = 200) # other option for creating synthetic records

print(prop.table(table(train_raw$Target)))
sum(train_raw$Target==1)
sum(train_raw$Target==0)
NROW(train_raw)

##########################################################################################
# feature selection
##########################################################################################

#check for missing values (str only)
#sort(sapply(C0_Master, function(x) round(sum(is.na(x))/nrow(C0_Master),2)), decreasing = TRUE) # checking for unreliable attributes
#sort(sapply(C0_Master, function(x) length(unique(x))), decreasing = TRUE) # checking for useless factors

fac <- which(sapply(C0_Master,is.factor))
num <- which(sapply(C0_Master,is.numeric))
char <- which(sapply(C0_Master,is.character))

####################

#var_cor <- cor(C0_Master[,num])
# cor(C0_Master[,num])
# graphics.off()
# dev.new(width=1000, height=1000)
# par("mar")
# par(mar=c(1,1,1,1))
# pairs(C0_Master[,num])

# var_cor <- as.data.frame(var_cor)
# fwrite(var_cor,paste0(oPath,"var_cor",mmmddyyyy,".csv"),na= "",row.names = TRUE)
####################


##########################################################################################
# Modelling (Decision Tree)
##########################################################################################

#identifying variables granular variables which could not be used in the model
date_fields <- unique(c(grep('DATE', names(train_raw), ignore.case = TRUE, value=TRUE),
                        grep('ym', names(train_raw), ignore.case = TRUE, value=TRUE),
                        grep('year', names(train_raw), ignore.case = TRUE, value=TRUE),
                        grep('month', names(train_raw), ignore.case = TRUE, value=TRUE)))

id_fields <- unique(c(grep('_ID', names(train_raw),ignore.case = TRUE,value=TRUE),
                      grep('_IAD', names(train_raw),ignore.case = TRUE,value=TRUE),
                      grep('name', names(train_raw), ignore.case = TRUE, value=TRUE)))

exlc_fields <- unique(c(grep('SM_', names(train_raw),ignore.case = TRUE,value=TRUE),
                        grep('BAND', names(train_raw),ignore.case = TRUE,value=TRUE),
                        grep('FILE', names(train_raw),ignore.case = TRUE,value=TRUE)))

rm_fields <- c(date_fields,id_fields,exlc_fields)

##########################################################################################
# clearing new factors in the test set - training set does not have the levels 
##########################################################################################
clear_new_fac_lst <- intersect(names(which(sapply(train_raw,is.factor))),names(C0_Master))
# do not reduce dimensions for fields not used for modeling
clear_new_fac_lst <- setdiff(clear_new_fac_lst,c(date_fields,id_fields,exlc_fields))

for(clear_fac in clear_new_fac_lst){
  
  C0_Master[[clear_fac]] <- as.character(C0_Master[[clear_fac]])
  #if there are new levels - clear it
  C0_Master[[clear_fac]] <- ifelse(!(C0_Master[[clear_fac]] %in% levels(train_raw[[clear_fac]])),
                                   "-",
                                   as.character(C0_Master[[clear_fac]]))
}

##########################################################################################
# model
##########################################################################################

train <- train_raw
# names(train)

fields <- setdiff(names(train),rm_fields)
train <- train[,fields]

#full data set
test <- C0_Master

# RPart Decision trees (input tree depth, complexity parameter, lower limit for risk score likelihood output)
rpart.func(8,.001,1,c(0,1.05,1.,0),
           c(rm_fields))

# cost_lst <- seq(from = 0.8, to = 1.1,by = 0.005)
# optimize_param

#saveRDS(fit.rp,paste0(oPath,"fit.rp",".RDS"))

# Margins
par(oma=c(0,0,0,0) )
par(mar=c(8,5,1,0.2))

#Variable Importance
rpt.var.plot()

# Plot ROC Curve for RPart decision tree
roc.plot(fit.rp)

# Plot lift for decision tree
lift.plot()

# Plot deciles for decision tree
deciles.plot()

# Plot RPart decision tree

# Margins
par(oma=c(0,0,0,0) )
par(mar=c(9,4.5,2,1))

prp(fit.rp,varlen=-15,
    yesno=1,
    box.palette=c("steelblue","red"), 
    cex=0.5,
    split.fun=split.fun)

#printing image
print("PRINTING DECISION TREE...")
jpeg(paste0(oPath,"Decision_Tree.jpg"), width = 1600, height = 950)
par(oma=c(0,0,0,0) )
par(mar=c(5,5,2,1))
prp(fit.rp,varlen=-15,
    yesno=1,
    box.palette=c("steelblue","red"), 
    cex=0.8,
    split.fun=split.fun)
dev.off()


#Calculate AUC
perf_ROC=performance(pred,"tpr","fpr")
perf_AUC=performance(pred,"auc")
#AUC=perf_AUC@y.values[[1]]
#AUC
#plot(perf_ROC, main="ROC plot")
cat("AUC:",perf_AUC@y.values[[1]])

# Fancy Decision tree plot
# Margins
par(oma=c(0,0,0,0) )
par(mar=c(9,4.5,2,1))

rpart.plot(fit.rp, cex=0.5,split.fun=split.fun)

#printing image
print("PRINTING DECISION TREE...")
jpeg(paste0(oPath,"Decision_Tree_Pretty.jpg"), width = 1600, height = 950)
par(oma=c(0,0,0,0) )
par(mar=c(5,5,2,1))
rpart.plot(fit.rp, cex=0.8,split.fun=split.fun)
dev.off()

#fancyRpartPlot(fit.rp,cex=0.5)

fullpreds_raw <- fullpreds

#find which probability group an employee belongs to using the decision tree model
fullpreds$node <- predict_nodes(fit.rp,fullpreds)
node_p <- node_path(fit.rp)
fullpreds <-merge.data.frame(fullpreds,node_p,by.x = "node",by.y = "Rule",all.x = TRUE)