userdata <- read.table("/home/scuiting/Workspace/sql/userdata.txt",head = TRUE, sep = "," ,as.is=T)
library(rpart)
library(rpart.plot)
fit <- rpart(userId~., data = userdata, method = "class", parms = list(prior = c(0.5, 0.5), split = "information")) #control = ct, 
rpart.plot(fit,main="决策树", 
           ycompress=TRUE,extra=1,cex=0.7,varlen=0,branch=1,digits=4, 
           round=0.5,shadow.col="gray",xsep=" / ",box.col="green",split.cex=1.1, 
           split.suffix=" ?", 
           split.box.col="lightgray", 
           split.border.col="darkgray", split.round=.5,yesno.yshift=0.6, 
           boxes.include.gap=TRUE,eq=" ",lt=" < ", ge=" >= ") 