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

