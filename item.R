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

# item the vector of itemKind
