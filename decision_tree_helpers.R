# helper functions used for decision tree model
#maxdepth = 8; cp = .001; pthresh = 1; lmx = c(0,1.05,1,0) ;colX = c(rm_fields)

rpart.func<-function(maxdepth,cp,pthresh,lmx,colX){
  train$Target<-ifelse(train$Target==0,"no","yes");
  train <- train[,!(colnames(train) %in% colX)]
  test0 <- test
  test$Target<-ifelse(test$Target==0,"no","yes");
  test$Target<-as.factor(test$Target);
  
  fit.rp <<- rpart(Target ~ .,
                   train,
                   control=rpart.control(maxdepth=maxdepth,cp=cp),
                   parms=list(prior=c(.5,.5),
                              loss=matrix(lmx,nrow = 2)),
                   method="class")
  
  rpartPred<-predict(fit.rp,test,type="class") 
  
  print(confusionMatrix(rpartPred,test$Target,positive="yes"))
  cmtab<<-confusionMatrix(rpartPred,test$Target)$table
  #cmtab
  fwrite(cmtab,paste0(oPath,"Confusion_Matrix.csv"))
  
  rpreds<-predict(fit.rp,test,type="prob") #retreives the probability values 
  predvals<-rpreds[,2]#gets the probablility of target = 1
  
  probs.id<-cbind(test$id, predvals, rpartPred,test$Target)
  colnames(probs.id)<-c("id","test.Target.prob","test.Target.class","Target")
  
  test_n <- names(test0)
  fullpreds<-cbind(test0,predvals, rpartPred)
  colnames(fullpreds)<-c(test_n,c("test.Target.prob","test.Target.class"))
  fullpreds <<-fullpreds[with(fullpreds,order(-as.numeric(test.Target.prob))),]
  
  test.probs.id<-as.data.frame(probs.id)
  test.probs.id<-as.data.frame(subset(probs.id,predvals>=pthresh))
  ordered.probs.ts1<-test.probs.id[with(test.probs.id,order(-as.numeric(test.Target.prob))),]
  ordered.probs.ts<<-subset(ordered.probs.ts1,!duplicated(ordered.probs.ts1$id))
  print(ordered.probs.ts)
  print(cmtab)
  
  cmtab2<-prop.table(cmtab,margin=2)
  colnames(cmtab2)<-c("lowrisk","risky")
  rownames(cmtab2)<-c("lowrisk","risky")
  print(qplot(Prediction, y=Reference, data=melt(cmtab2), fill=value, geom="tile")+
          geom_text(aes(label = round(value,2)), vjust = 1) +
          scale_fill_gradient2(high='red', space='Lab'))
  
  # printing image
  print("PRINTING CONFUSION MATRIX...")
  jpeg(paste0(oPath,"Confusion_Matrix.jpg"), width = 400, height = 350)
  print(qplot(Prediction, y=Reference, data=melt(cmtab2), fill=value, geom="tile")+
          geom_text(aes(label = round(value,2)), vjust = 1) +
          scale_fill_gradient2(high='red', space='Lab'))
  dev.off()
  # 
  performance_m <- as.data.frame(cmtab2)
  #current performance metric = accuracy (pTP +pTN)
  performance_m <- performance_m$Freq[performance_m$Prediction %in% "lowrisk" & performance_m$Reference %in% "lowrisk"] + performance_m$Freq[performance_m$Prediction %in% "risky" & performance_m$Reference %in% "risky"]
  
  return(performance_m)
}

#variable importance
rpt.var.plot<-function(){
  #writing out the table for variable importance
  fwrite(as.data.frame(fit.rp$variable.importance),paste0(oPath,"Variable_Importance.csv"),row.names = TRUE)
  
  print(as.data.frame(fit.rp$variable.importance))
  x <- barplot(fit.rp$variable.importance, xaxt="n",ylab="Variable importance",main="Decision Tree Variable Importance",
               ylim=c(0,fit.rp$variable.importance[1]+20),col="steelblue")
  labs <- abbreviate(names(fit.rp$variable.importance),20)
  text(cex=.85, x=x+.35, y=-8.5, labs, xpd=TRUE, srt=45, pos=2)
  
  #printing image
  print("PRINTING VAR IMPORTANCE...")
  jpeg(paste0(oPath,"Variable_Importance.jpg"), width = 950, height = 850)
  par(mar=c(20,5,1,0.2))
  barplot(fit.rp$variable.importance,
          names.arg = names(fit.rp$variable.importance),
          legend.text = NULL, beside = FALSE,
          horiz = FALSE, 
          ylab="Variable importance",
          main="Decision Tree Variable Importance",
          ylim=c(0,fit.rp$variable.importance[1]+20),
          col="steelblue",
          cex.names = 0.8,
          axisnames = TRUE,
          las=2)
  dev.off()
}

#function that plots the ROC curve
roc.plot<-function(model){
  predictions <<- predict(model, test, type="prob")
  pred <<- ROCR::prediction(predictions[,2], test$Target)
  perf <<- ROCR::performance(pred, "tpr","fpr")
  plot(perf, main="ROC Curve",lwd=2,col="red")
  abline(a=0,b=1,lwd=1.75,lty=2,col="gray")
  #
  perf_AUC=performance(pred,"auc")
  cat("\n","Area Under the Curve (AUC):",perf_AUC@y.values[[1]],"\n")
  #result.roc <- roc(churnc$churn, result.predicted.prob[,2]) 
  #result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
  #print(result.coords)
}


#function that plots the Lift
lift.plot<-function(){
  perf2 <- ROCR::performance(pred,"lift","rpp")
  plot(perf2, main="Lift Curve", colorize=T)
}

#function that plots the deciles
deciles.plot<-function(){
  rpreds<-predict(fit.rp,test,type="prob") #retreives the probability values 
  predvals<-rpreds[,2]#gets the probablility of target = 1
  probs.id<-cbind(test$id,predvals, test$Target)
  colnames(probs.id)<-c("id","test.Target.prob","Target")
  test.probs.id<-as.data.frame(probs.id)
  test.probs.id$test.Target.prob <- as.numeric(as.character(test.probs.id$test.Target.prob))
  test.probs.id$test.Target.prob1 <- ifelse(test.probs.id$test.Target.prob==1,0.95,as.numeric(test.probs.id$test.Target.prob))
  ordered.probs.ts1 <- test.probs.id[with(test.probs.id,order(-test.Target.prob)),]
  ordered.probs.ts1$Decile <- (floor(ordered.probs.ts1$test.Target.prob1*10)-10)*(-1)
  #ordered.probs.ts1 <- ordered.probs.ts1 %>% mutate(quantile = ntile(-test.Target.prob,10))
  if("package:plyr" %in% search()==TRUE){detach("package:plyr")}
  if("package:dplyr" %in% search()==FALSE){library("dplyr")}
  deciles.analysis <<- ordered.probs.ts1 %>%
    group_by(Decile) %>%
    summarise(flag_rate = sum(Target==1)/length(Target),
              flagged = sum(Target==1),
              notflagged = sum(Target==0))
  
  print(as.data.frame(deciles.analysis))
  x <- barplot(deciles.analysis$flag_rate, xaxt="n",ylab="Flag Rate",main="Decision Tree Deciles",
               ylim=c(0,deciles.analysis$flag_rate[1]+0.005),col="steelblue")
  labs <- abbreviate(deciles.analysis$Decile,20)
  text(cex=.6, x=x+.35, y=-(max(deciles.analysis$flag_rate)/10), labs, xpd=TRUE, pos=2)
}


predict_nodes <- function (object, newdata,na.action = na.pass) {
  where <-
    if (missing(newdata))
      object$where
  else {
    if (is.null(attr(newdata,"terms"))) {
      Terms <- delete.response(object$terms)
      newdata <- model.frame(Terms,newdata,na.action=na.action,
                             xlev = attr(object,"xlevels"))
      if(!is.null(cl <- attr(Terms,"dataClasses")))
        .checkMFClasses(cl,newdata,TRUE)
    }
    rpart:::pred.rpart(object,rpart:::rpart.matrix(newdata))
  }
  as.integer(row.names(object$frame))[where]
}


#getting paths for each terminal node
node_path <- function (object) {
  if("package:plyr" %in% search()==TRUE){library("dplyr")}
  subrules <<- rpart.subrules.table(object)
  subrules$Variable <- as.character(subrules$Variable)
  subrules$compare <- ifelse(is.na(subrules$Value),
                             subrules$Variable,
                             ifelse(!is.na(subrules$Value)
                                    &(!is.na(subrules$Less)|!is.na(subrules$Greater)),
                                    paste(subrules$Variable,"_eq",sep=""),
                                    paste(subrules$Variable,"_eq",subrules$Value,sep="")))
  subrules$compare <- ifelse(is.na(subrules$Less),subrules$compare,paste(subrules$compare,"_lt",subrules$Less,sep =""))
  subrules$compare <- ifelse(is.na(subrules$Greater),subrules$compare,paste(subrules$compare,"_gt",subrules$Greater,sep =""))
  subrules$compare <- paste(subrules$Subrule,subrules$compare,sep="-")
  subrules$LR <- substr(subrules$Subrule,1,1)
  subrules$leafnum <- gsub("R","",subrules$Subrule)
  subrules$leafnum <- as.numeric(gsub("L","",subrules$leafnum))
  subrules <- subrules[order(subrules$leafnum),]
  
  
  tnodes <- (rpart.rules.table(object))
  tnodes <- tnodes[tnodes$Leaf == TRUE,]
  
  # merging node statements to terminal nodes
  tnodes_paths1 <-merge.data.frame(tnodes,subrules,by.x = "Subrule",by.y = "Subrule",all.x = TRUE)
  tnodes_paths1 <- tnodes_paths1[order(tnodes_paths1$leafnum),]
  tnodes_paths1 <<- tnodes_paths1[,c("Rule","Subrule","compare")]
  tnodes_paths1$Subrule <- tnodes_paths1$compare
  
  tnodes_paths <- cast(tnodes_paths1,Rule~Subrule,length)
  tnodes_paths <- tnodes_paths[,c(names(tnodes_paths)[1], subrules$compare)]
  
  node_rule <- ddply(tnodes_paths1,.(Rule),tail,1)
  node_rule$last_rule <- node_rule$compare
  node_rule <<- node_rule[,c("Rule","last_rule")]
  tnodes_paths <-merge.data.frame(tnodes_paths,node_rule,by.x = "Rule",by.y = "Rule",all.x = TRUE)
  
  return(tnodes_paths)
}


split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width=10), collapse="\n")
  }
  labs
}