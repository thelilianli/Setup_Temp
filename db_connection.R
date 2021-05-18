library(RJDBC)
library(RODBC)

#Connect to the database
user <- Sys.info()[names(Sys.info()) %in% "login"]
pw <- as.data.frame(fread(paste0("C:/Users/",user,"/path/pathpw.csv")))


#oracle connection
login <- pw$User[pw$LIBNAME %in% "test"]
pass <- pw$Pass[pw$LIBNAME %in% "test"]

jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="//path/ojdbc6.jar")
jdbcConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//server:port/schema", login, pass)
ch<- odbcConnect("short_name", uid = login, pwd = pass) 


#informatica connection
inf_login <- pw$User[pw$LIBNAME %in% "test2"]
inf_pass <- pw$Pass[pw$LIBNAME %in% "test2"]
ch<-odbcConnect("short_name", uid=inf_login, pwd=inf_pass)
infConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//INFserver:INFport/INFschema", inf_login, inf_pass)


#netezza connection
netezza_login <- pw$User[pw$LIBNAME %in% "test3"]
netezza_pass <- pw$Pass[pw$LIBNAME %in% "test3"]


ntzaConnection <- DBI::dbConnect(odbc::odbc(),
                                 Driver = "NetezzaSQL",
                                 Server   = "NTZserver",
                                 Port = "NTZport",
                                 Database = "NTZschema",
                                 UID    = netezza_login,
                                 PWD    = netezza_pass)

#test
test <- dbGetQuery(ntzaConnection,
                   paste0('select * from schema.table LIMIT 10'))


# SAS ez connect str
# DM 'LOG;CLEAR;OUT;CLEAR;';
# %include '/userhome/lilili/Pass.sas';