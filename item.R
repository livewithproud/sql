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
lu <- dim(userid)[1]
for(i in 1:lu)
{
	idnew[idold == userid[i]] <- 1
}
idnew[! idnew == 1] <- 0
dataall[,1] <- idnew



