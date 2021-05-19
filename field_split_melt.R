#separating a field when you know how many values it contains
fulldata <- fulldata %>%
  separate(Linked_ID_28 in the SOURCE Connection,c(paste0("Linked_ID_",as.character(1:28))))

fulldata <- melt(data = fulldata, id.vars = setdiff(names(fulldata),c(paste0("Linked_ID_",as.character(1:28)))),variable_name = "source_ID",na.rm = TRUE)
fulldata <- fulldata[!(fulldata$value %in% ""),]#removing missing

#separating a field when you don't know how many values it contains  
fulldata_Linked_ID <- data.frame(do.call('rbind', strsplit(as.character(fulldata$Linked_ID), ',', fixed=TRUE))) #separating field
names(fulldata_Linked_IDs) <- gsub("X","Linked_ID",names(fulldata_Linked_ID))
fulldata <-separate(fulldata, "Linked_ID",into = names(fulldata_Linked_ID),sep = ",",remove = TRUE)# using the names to define separated column names and transform fulldata
#View(fulldata[,names(fulldata)[grepl("Linked_ID",names(fulldata))]])
