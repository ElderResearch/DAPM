###
### Info for the Production NRDB. Please source this:
###
###     source ("D:/ERI/Working/nrdbPassword.R")
###
### (the path name in this example is for Kansas; use the appropriate path)
###
### Then you can connect to the DB with:
###
### DRV <- JDBC ("org.postgresql.Driver", APM_DB_DRV)
### 
### conn <- dbConnect (DRV,
###                    sprintf ("jdbc:postgresql://%s:%s/%s", APM_SERVER_URL, APM_DB_PORT, APM_DB_NAME),
###                    dbname=APM_DB_NAME, host=APM_SERVER_URL, port=APM_DB_PORT,
###                    user=APM_DB_USERNAME, password=APM_DB_PASSWORD)
### 
### Do not include the unscrubbed version of this file in git, and you'll
### be safe from security problems. Also, one change will work for everyone.
###

cat ("==================== nrdbPassword.R RUNNING  ", date (), "\n")

### This is the way that NTELX has standardized for variables to be
### communicated (as global R variables) to R scripts from Kettle.
### If APM_DB_NAME exists, we assume that we are running via Kettle
### and do not set that or the other global variables. If it does
### not exist, we set them here, assuming we are running from RStudio or
### other development/test environment.
###
### These values should match Kettle, which is authoritative. They will
### probably be different for each state.

APM_DB_NAME     <- "sidapmdev"
APM_SERVER_URL  <- "localhost"
APM_DB_PORT     <- "5432"
APM_DB_USERNAME <- "XXXXXX"
APM_DB_PASSWORD <- "XXXXXX"
APM_DB_DRIVER   <- "C:/Program Files (x86)/JDBC-Drivers/postgresql-9.4-1201.jdbc41.jar"

APM_CYCL_DT <- "2016-02-20"  ## WOULD CHANGE AS TIME MOVES ON!

APM_MODEL_DIR <- sprintf ("I:/ERI/Working/%s/Production/Model", Sys.getenv ("USERNAME"))

if (!file.exists (APM_MODEL_DIR))
{ APM_MODEL_DIR <- sprintf ("I:/ERI/Working/%s/Domino/Production/Model", Sys.getenv ("USERNAME")) }
