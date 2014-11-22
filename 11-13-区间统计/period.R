library(DBI)
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "renet", username = "root", password="welcome1", host = "127.0.0.1",port=2002)
tmax <- 735810
tmin <- 735689
# tmax <- as.numeric(dbGetQuery(conn, "select to_days(max(time)) from analyzeuser;"))
# tmin <- as.numeric(dbGetQuery(conn, "select to_days(min(time)) from analyzeuser;"))

score <- data.frame()

dbGetQuery(conn, "drop view if exists period;")
dbGetQuery(conn, "drop view if exists period_n;")

for(p in c(7,14,28,30))
{
	temp <- NULL

	dbGetQuery(conn, paste("create view period as
					 select distinct (to_days(time) -",tmin,") div ",p," as period, userId from analyzeuser;")) #  -((",tmax,"-",tmin,") mod ",p,")
	dbGetQuery(conn, "create view period_n as select userId, count(*) as n from period group by userId;")	
	temp <- dbGetQuery(conn, paste("select count(*) from period_n where n = (",tmax," - ",tmin,") div ",p,";"))
	score <- rbind(score, cbind(p,temp))
	cat("score",as.character(score),"\n")
	dbGetQuery(conn, "drop view period;")
	dbGetQuery(conn, "drop view period_n;")
}
write.table(score,"/home/scuiting/Workspace/sql/score.txt", quote=FALSE, sep=",")
dbDisconnect(conn)






