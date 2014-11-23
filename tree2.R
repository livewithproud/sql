library(DBI)
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "renet", username = "root", password="welcome1", host = "127.0.0.1",port=2002)

tm <- 4 # total month
# tm <- as.numeric(dbGetQuery(conn, "select max(month(time)) - min(month(time)) + 1 from analyzeuser;")) # total month
usernum <- 1022979
# usernum <- as.numeric(dbGetQuery(conn, "select count(distinct userId) from analyzeuser;"))

userid <- dbGetQuery(conn, paste("select userId, count(distinct(month(time))) as totalmonth from analyzeuser group by userId having count(distinct(month(time))) = ",tm,";"))	
userid <- userid[,1]
dataall <- dbGetQuery(conn, "select userId, itemKind, payChannel, payRegional 
						     from analyzeuser 
						     where length(itemKind)>0 and itemKind is not null
						     and length(payChannel)>0 and payChannel is not null
						     and length(payRegional)>0 and payRegional is not null;")  # dim--- 1057482 4

dbDisconnect(conn)

#the vector of itemKind
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
dataall <- cbind(dataall[c(1,3,4)],item)


# chinese convert to english
Cnew <- Cold <- dataall[,2]
Coldname <- c("新支付宝","动漫基地","一键支付","无线支付宝","支付宝直连银行","微信支付","微信APP支付","支付宝","财付通","中国银联","联动优势","易充支付","PC机","手机","浏览器")
Cnewname <- c("XZFB","DMJD","YJZF","WXZFB","ZFBZLYH","WXZF","WXAPPZF","ZFB","CFT","ZGYL","LDYS","YCZF","PCJ","SJ","LLQ")
lC <- length(Coldname)
for(i in 1:lC)
{
	Cnew[Cold == Coldname[i]] <- Cnewname[i]
}
dataall[,2] <- Cnew


Rnew <- Rold <- dataall[,3]
Roldname <- c("安徽省","北京市","福建省","甘肃省","广东省","广西壮族自治区","贵州省","海南省","河北省","河南省","黑龙江省","湖北省","湖南省","吉林省","江苏省","江西省","辽宁省","内蒙古自治区","宁夏回族自治区","青海省","山东省","山西省","陕西省","上海市","四川省","天津市","西藏自治区","新疆维吾尔自治区","云南省","浙江省","重庆市","澳门特别行政区","香港特别行政区","台湾省")
Rnewname <- c("AH","BJ","FJ","GS","GD","GX","GZ","HI","HE","HA","HL","HB","HN","JL","JS","JX","LN","NM","NX","QH","SD","SX","SN","SH","SC","TJ","XZ","XJ","YN","ZJ","CQ","MO","HK","TW")
lR <- length(Roldname)
for(i in 1:lR)
{
	Rnew[Rold == Roldname[i]] <- Rnewname[i]
}
dataall[,3] <- Rnew


# userid
idnew <- idold <- dataall[,1]
lu <- length(userid)
for(i in 1:lu)
{
	idnew[idold == userid[i]] <- 1
}
idnew[! idnew == 1] <- 0
dataall[,1] <- idnew

library(rpart)
library(rpart.plot)

# ct <- rpart.control(xval = 10, minsplit = 20, cp= 0.1)
p <- lu/usernum
fit <- rpart(userId~., data = dataall, method = "class", parms = list(prior = c(p, 1-p), split = "information")) #control = ct, 

# rpart.plot(fit, branch=1, branch.type=2, type=1, extra=102,
#            shadow.col="gray", box.col="green",
#            border.col="blue", split.col="red",
#             main="决策树") # split.cex=1.2


rpart.plot(fit,main="决策树", 
           ycompress=TRUE,extra=1,cex=0.7,varlen=0,branch=1,digits=4, 
           round=0.5,shadow.col="gray",xsep=" / ",box.col="green",split.cex=1.1, 
           split.suffix=" ?", 
           split.box.col="lightgray", 
           split.border.col="darkgray", split.round=.5,yesno.yshift=0.6, 
           boxes.include.gap=TRUE,eq=" ",lt=" < ", ge=" >= ")  #

#rpart.plot(fit, main="决策树")

#printcp(fit)

#prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# fit2 <- prune(fit, cp=0.01)

#tree <- rpart(userId~., userdata, method = "class")
#plot(fit, uniform = TRUE, branch = 0, margin = 0.1, main= "classification tree")
#text(fit, use.n = TRUE, fancy = TRUE, col = "blue")
setwd("/home/scuiting/Workspace/sql")
dev.copy(jpeg, filename = "tree.jpeg")
dev.off()