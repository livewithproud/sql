library(DBI)
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "renet", username = "root", password="welcome1", host = "127.0.0.1",port=2002)
tmax <- 735810
tmin <- 735689

# tmax <- as.numeric(dbGetQuery(conn, "select to_days(max(time)) from analyzeuser;"))
# tmin <- as.numeric(dbGetQuery(conn, "select to_days(min(time)) from analyzeuser;"))


dbGetQuery(conn, "drop view if exists period;")
dbGetQuery(conn, "drop view if exists period_n;")
dbGetQuery(conn, "drop view if exists user;")

p <- 30


dbGetQuery(conn, paste("create view period as
					 select distinct (to_days(time) -",tmin,") div ",p," as period, userId from analyzeuser;"))  #  ((",tmax,"-",tmin,") mod ",p,")
dbGetQuery(conn, "create view period_n as select userId, count(*) as n from period group by userId;")	
dbGetQuery(conn, paste("create view user as select userId from period_n where n = (",tmax," - ",tmin,") div ",p,";"))


data1 <- dbGetQuery(conn, "select userId, itemKind, payChannel, payRegional 
						   from analyzeuser 
						   where userId in (select * from user) 
						   and length(itemKind)>0 and itemKind is not null
						   and length(payChannel)>0 and payChannel is not null
						   and length(payRegional)>0 and payRegional is not null;")
# cat("data1",as.character(data1),"\n")
data1$userId <- 1
# cat("data1",as.character(data1),"\n")
data2 <- dbGetQuery(conn, "select userId, itemKind, payChannel, payRegional 
						   from analyzeuser 
						   where NOT userId in (select * from user) 
						   and length(itemKind)>0 and itemKind is not null
						   and length(payChannel)>0 and payChannel is not null
						   and length(payRegional)>0 and payRegional is not null
						   limit 100;")
cat("data2",as.character(data2),"\n")
data2$userId <- 0
cat("data2",as.character(data2),"\n")
dataall <- rbind(data1, data2)
cat("dataall",as.character(dataall),"\n")
# write.table(movie,"/home/scuiting/Workspace/sql/movie.txt", quote=FALSE, sep=",")


dbGetQuery(conn, "drop view period;")
dbGetQuery(conn, "drop view period_n;")
dbGetQuery(conn, "drop view user;")

dbDisconnect(conn)


# item the vector of itemKind
mitem <- c("喜剧","动作","剧情","悬疑","动画","科幻","纪录片","爱情","家庭","儿童","历史","惊悚","短片","奇幻","歌舞","武侠","恐怖","战争","西部","古装","犯罪","运动","冒险","舞台","戏曲","传记","情色","成人","真人秀","同性","鬼怪","黑色","文艺")
nm <- dim(dataall)
n <- nm[1]

temp <- rep(0,n)
item <- data.frame(temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp, temp)
names(item) <- mitem

itemtemp <- dataall$itemKind # character
t <- 1
while(t < n + 1)
{
	temp <- strsplit(itemtemp[t], "|", fixed = TRUE) # list
	temp <- unlist(temp)
	ztemp <- rep(0,33)
	l <- length(temp)
	for( i in 1 : l)
	{
		ztemp[temp[i] == mitem] <- 1
	}
	item[t,] <- ztemp
	t <- t + 1
}


userdata <- cbind(dataall[c(1,3,4)],item)

Cnew <- Cold <- userdata[,2]
Coldname <- c("新支付宝","动漫基地","一键支付","无线支付宝","支付宝直连银行","微信支付","微信APP支付","支付宝","财付通","中国银联","联动优势","易充支付","PC机","手机","浏览器")
Cnewname <- c("XZFB","DMJD","YJZF","WXZFB","ZFBZLYH","WXZF","WXAPPZF","ZFB","CFT","ZGYL","LDYS","YCZF","pcJ","SJ","LLQ")
lC <- length(Coldname)
for(i in 1:lC)
{
	Cnew[Cold == Coldname[i]] <- Cnewname[i]
}
userdata[,2] <- Cnew

Rnew <- Rold <- userdata[,3]
Roldname <- c("北京市","江苏省","广东省","上海市","重庆市","浙江省","天津市","新疆维吾尔自治区","广西壮族自治区","西藏自治区","辽宁省","河北省","安徽省","湖北省","山东省","四川省","青海省","海南省","陕西省","贵州省","黑龙江省","湖南省","云南省","福建省","甘肃省","江西省","山西省","吉林省","河南省")
Rnewname <- c("BJ","JS","GD","SH","CQ","ZJ","TJ","XJ","GX","XZ","LN","HB","AH","HUB","SD","SC","QH","HAIN","S3X","GZ","HLJ","HUN","YN","FJ","GS","JX","S1X","JL","HEN")

lR <- length(Roldname)
for(i in 1:lR)
{
	Rnew[Rold == Roldname[i]] <- Rnewname[i]
}
userdata[,3] <- Rnew


library(rpart)
library(rpart.plot)

ct <- rpart.control(xval = 10, minsplit = 20, cp= 0.1)

fit <- rpart(userId~., data = userdata, method = "class", parms = list(prior = c(0.5, 0.5), split = "information")) #control = ct, 

rpart.plot(fit, branch=1, branch.type=2, type=1, extra=102,
           shadow.col="gray", box.col="green",
           border.col="blue", split.col="red",
           split.cex=1.2, main="决策树")

#rpart.plot(fit, main="决策树")

#printcp(fit)

#prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# fit2 <- prune(fit, cp=0.01)

#tree <- rpart(userId~., userdata, method = "class")
#plot(fit, uniform = TRUE, branch = 0, margin = 0.1, main= "classification tree")
#text(fit, use.n = TRUE, fancy = TRUE, col = "blue")


