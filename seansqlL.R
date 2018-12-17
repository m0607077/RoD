# function SQL
install.packages("data.table")

# 攻破屬性函數
rDisAr <- function(Odata,e){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,1] + noise1
  
  noise2 <- rlaplace(n, location = 0, scale = b )
  Sdata2 <- Odata[,2] + noise2
  
  noise3 <- rlaplace(n, location = 0, scale = b )
  Sdata3 <- Odata[,3] + noise3
  
  
  P <- 1:n
  for(i in 1:n){
    which1 <- which(Odata[i,1]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,1]+max(abs(noise1)))
    which2 <- which(Odata[i,2]-max(abs(noise2)) <= Sdata2 & Sdata2 <= Odata[i,2]+max(abs(noise2)))
    which3 <- which(Odata[i,3]-max(abs(noise3)) <= Sdata3 & Sdata3 <= Odata[i,3]+max(abs(noise3)))
    
    P[i] <- sum(Odata[unique(
      which3[which3 %in% which2[which2 %in% which1]]
    ),4] %in% 1) / length(unique(
      which3[which3 %in% which2[which2 %in% which1]]
    ))
    
  }
  
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1,Sdata2,Sdata3))
  
}

# 攻破機率函數
rDisP1dim <- function(Odata,e,Var){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,Var] + noise1
  
  
  P <- 1:n
  for(i in 1:n){
    which1 <- which(Odata[i,Var]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var]+max(abs(noise1)))
    
    Out <- c(which1)
    P[i] <- 1 / length(names(table(Out))[which(table(Out) == 1)])
    
  }
  
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1))
  
}

rDisP2dim <- function(Odata,e,Var1,Var2){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,Var1] + noise1
  
  noise2 <- rlaplace(n, location = 0, scale = b )
  Sdata2 <- Odata[,Var2] + noise2
  
  P <- 1:n
  for(i in 1:n){
    which1 <- which(Odata[i,Var1]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var1]+max(abs(noise1)))
    which2 <- which(Odata[i,Var2]-max(abs(noise2)) <= Sdata2 & Sdata2 <= Odata[i,Var2]+max(abs(noise2)))
    
    Out <- c(which1,which2)
    P[i] <- 1 / length(names(table(Out))[which(table(Out) == 2)])
    print(i)
  }
  
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1,Sdata2))
  
}

rDisP3dim <- function(Odata,e,Var1,Var2,Var3){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,Var1] + noise1
  
  noise2 <- rlaplace(n, location = 0, scale = b )
  Sdata2 <- Odata[,Var2] + noise2
  
  noise3 <- rlaplace(n, location = 0, scale = b )
  Sdata3 <- Odata[,Var3] + noise3
  
  
  P <- 1:n
  for(i in 1:n){
    which1 <- which(Odata[i,Var1]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var1]+max(abs(noise1)))
    which2 <- which(Odata[i,Var2]-max(abs(noise2)) <= Sdata2 & Sdata2 <= Odata[i,Var2]+max(abs(noise2)))
    which3 <- which(Odata[i,Var3]-max(abs(noise3)) <= Sdata3 & Sdata3 <= Odata[i,Var3]+max(abs(noise3)))
    
    Out <- c(which1,which2,which3)
    P[i] <- 1 / length(names(table(Out))[which(table(Out) == 3)])
    
  }
  
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1,Sdata2,Sdata3))
  
}
###
seanrDisP1dim <- function(Odata,e,Var){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,Var] + noise1
  
  
  P <- 1:n
  for(i in 1:n){
    which1 <- which(Odata[i,Var]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var]+max(abs(noise1)))
    
    Out <- c(which1)
    P[i] <- 1 / length(Out)
    
  }
  
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1))
  
}

seanrDisP2dim <- function(Odata,e,Var1,Var2){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,Var1] + noise1
  
  noise2 <- rlaplace(n, location = 0, scale = b )
  Sdata2 <- Odata[,Var2] + noise2
  
  P <- 1:n
  for(i in 1:n){
    which1 <- which(Odata[i,Var1]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var1]+max(abs(noise1)))
    which2 <- which(Odata[i,Var2]-max(abs(noise2)) <= Sdata2 & Sdata2 <= Odata[i,Var2]+max(abs(noise2)))
    
    A <- data.table(X=which1)
    B <- data.table(X=which2)
    P[i] <- 1 / length(semi_join(A,B,by="X")[,1])
  }
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1,Sdata2))
}

seanrDisP3dim <- function(Odata,e,Var1,Var2,Var3){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,Var1] + noise1
  
  noise2 <- rlaplace(n, location = 0, scale = b )
  Sdata2 <- Odata[,Var2] + noise2
  
  noise3 <- rlaplace(n, location = 0, scale = b )
  Sdata3 <- Odata[,Var3] + noise3
  
  
  P <- 1:n
  for(i in 1:n){
    which1 <- which(Odata[i,Var1]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var1]+max(abs(noise1)))
    which2 <- which(Odata[i,Var2]-max(abs(noise2)) <= Sdata2 & Sdata2 <= Odata[i,Var2]+max(abs(noise2)))
    which3 <- which(Odata[i,Var3]-max(abs(noise3)) <= Sdata3 & Sdata3 <= Odata[i,Var3]+max(abs(noise3)))
    A <- data.table(X=which1)
    B <- data.table(X=which2)
    C <- data.table(X=which3)
    P[i] <- 1 / length(semi_join(semi_join(A,B,by="X"),C,by="X")[,1])
  }
  
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1,Sdata2,Sdata3))
  
}

RodParameter <- function(Odata,e,dimensions,varN){
  dataFinal <- NULL
  nameList <- NULL
  if(dimensions ==1){
    for(y in c(1:length(e))){
      data <- seanrDisP1dim(Odata = Odata,e = e[y],Var = varN)
      dataname <- paste0(deparse(substitute(Odata)),"ep",e[y],"RoD")
      dataL <- list(dataname=data)
      dataFinal <- c(dataFinal,dataL)
      nameList <- c(nameList,dataname)
    }
    names(dataFinal) <- nameList
  return(dataFinal)
  }
  else if(dimensions ==2){
    for(y in c(1:length(e))){
      data <- seanrDisP2dim(Odata = Odata,e = e[y],Var1 = varN[1],Var2 = varN[2])
      dataname <- paste0(deparse(substitute(Odata)),"ep",e[y],"RoD")
      dataL <- list(dataname=data)
      dataFinal <- c(dataFinal,dataL)
      nameList <- c(nameList,dataname)
    }
    names(dataFinal) <- nameList
    return(dataFinal)
  }
  else if(dimensions ==3){
    for(y in c(1:length(e))){
      data <- seanrDisP3dim(Odata = Odata,e = e[y],Var1 = varN[1],Var2 = varN[2],Var3 = varN[3])
      dataname <- paste0(deparse(substitute(Odata)),"ep",e[y],"RoD")
      dataL <- list(dataname=data)
      dataFinal <- c(dataFinal,dataL)
      nameList <- c(nameList,dataname)
    }
    names(dataFinal) <- nameList
    return(dataFinal)
  }
}
# 原始資料K匿名計算
KPNdim <- function(VarN){
  if(length(VarN) == 1){
    TB <- table(OdataB[,VarN])
    TryOne <- (sapply(1:length(OdataB[,VarN]),function(i){ 1/TB[which(names(TB) %in% OdataB[i,VarN])]}))
  } else {
    TryData <- OdataB[,VarN]
    UseData <- sapply(1:dim(TryData)[1],function(i){ paste0(TryData[i,],collapse = ',')})
    TB <- table(UseData)
    TryOne <- sapply(1:length(UseData),function(i){ 1/TB[which(names(TB) %in% UseData[i])]})
  }
  
  list('everyP' = data.frame(TryOne),'Summary' = summary(TryOne))
}

# 誤差估計
findDisE <- function(Data1,Data2,horiz = FALSE){
  library('TSA')
  
  # Summary statistics
  Output <- rbind(
    abs(apply(Data1,2,summary) - apply(Data2,2,summary)) / apply(Data1,2,summary) * 100,
    abs(apply(Data1,2,sd) - apply(Data2,2,sd)) / apply(Data1,2,sd) * 100,
    abs(apply(Data1,2,var) - apply(Data2,2,var)) / apply(Data1,2,var) * 100,
    abs(apply(Data1,2,skewness) - apply(Data2,2,skewness)) / apply(Data1,2,skewness) * 100,
    abs(apply(Data1,2,kurtosis) - apply(Data2,2,kurtosis)) / apply(Data1,2,kurtosis) * 100,
    diag(cor(Data1,Data2))
  )
  rownames(Output) = c('Min.','1st Qu.','Median','Mean','3rd Qu.','Max.','sd','var','skewness','kurtosis','Cor for 2 Data')
  
  if(horiz){
    Output <- data.frame(t(Output))
    return(Output)
  } else {
    return(Output)
  }
}

heyrDisP1dim <- function(Odata,e,Var,CheckVar){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,Var] + noise1
  Checkdata <- Odata[,CheckVar]
  
  P <- 1:n
  for(i in 1:n){
    which1 <- which(Odata[i,Var]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var]+max(abs(noise1))& Checkdata==2)
    which0 <- which(Odata[i,Var]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var]+max(abs(noise1))& Checkdata==1)
    P[i] <- length(which1)/(length(which0)+length(which1))
  }
  
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1))
  
}

heyrDisP3dim <- function(Odata,e,Var1,Var2,Var3,CheckVar){
  
  n = dim(Odata)[1]
  b <- 1/e
  noise1 <- rlaplace(n, location = 0, scale = b )
  noise1 <- rlaplace(n, location = 0, scale = b )
  Sdata1 <- Odata[,Var1] + noise1
  noise2 <- rlaplace(n, location = 0, scale = b )
  Sdata2 <- Odata[,Var2] + noise2
  noise3 <- rlaplace(n, location = 0, scale = b )
  Sdata3 <- Odata[,Var3] + noise3
  Checkdata <- Odata[,CheckVar]
  
  P <- 1:n
  for(i in 1:n){
    which1R <- which(Odata[i,Var1]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var1]+max(abs(noise1))& Checkdata==2)
    which2R <- which(Odata[i,Var2]-max(abs(noise2)) <= Sdata2 & Sdata2 <= Odata[i,Var2]+max(abs(noise2))& Checkdata==2)
    which3R <- which(Odata[i,Var3]-max(abs(noise3)) <= Sdata3 & Sdata3 <= Odata[i,Var3]+max(abs(noise3))& Checkdata==2)
    which1L <- which(Odata[i,Var1]-max(abs(noise1)) <= Sdata1 & Sdata1 <= Odata[i,Var1]+max(abs(noise1))& Checkdata==1)
    which2L <- which(Odata[i,Var2]-max(abs(noise2)) <= Sdata2 & Sdata2 <= Odata[i,Var2]+max(abs(noise2))& Checkdata==1)
    which3L <- which(Odata[i,Var3]-max(abs(noise3)) <= Sdata3 & Sdata3 <= Odata[i,Var3]+max(abs(noise3))& Checkdata==1)
    OutR <- c(which1R,which2R,which3R)
    OutL <- c(which1L,which2L,which3L)
    P[i] <- length(which(table(OutR)==3))/(length(which(table(OutL)==3))+length(which(table(OutR)==3)))
  }
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1))
}

ep0heyrDisP3dim <- function(Odata,Var1,Var2,Var3,CheckVar){
  
  n = dim(Odata)[1]
  Sdata1 <- Odata[,Var1]
  Sdata2 <- Odata[,Var2]
  Sdata3 <- Odata[,Var3]
  Checkdata <- Odata[,CheckVar]
  
  P <- 1:n
  for(i in 1:n){
    which1R <- which(Odata[i,Var1] == Sdata1& Checkdata==2)
    which2R <- which(Odata[i,Var2] == Sdata2& Checkdata==2)
    which3R <- which(Odata[i,Var3] == Sdata3& Checkdata==2)
    which1L <- which(Odata[i,Var1] == Sdata1& Checkdata==1)
    which2L <- which(Odata[i,Var2] == Sdata2& Checkdata==1)
    which3L <- which(Odata[i,Var3 ]== Sdata3& Checkdata==1)
    OutR <- c(which1R,which2R,which3R)
    OutL <- c(which1L,which2L,which3L)
    P[i] <- length(which(table(OutR)==3))/(length(which(table(OutL)==3))+length(which(table(OutR)==3)))
  }
  list('P' = P,'Summary' = summary(P),'Output' = data.frame(Sdata1))
}
#繪圖and輸出
Dong2 <- function(data,dataname){
  datapath <- paste("D:/ROD/",dataname,".csv",sep = "")
  jpgpath <- paste("D:/ROD/",dataname,".jpg",sep = "")
  epspath <- paste("D:/ROD/",dataname,".eps",sep = "")
  dataf <- data.frame(data[[1]])
  write.csv(dataf,file = datapath)
  jpeg(filename =jpgpath,width = 1600,height = 1200)
  print(ggplot(as.data.frame(data[[1]]),aes(x=data$P))+
          ylim(0,length(data[[1]]))+
          xlim(-0.1,1.1)+
          ylab("Individuals")+
          xlab("RoD")+
          geom_histogram(data =as.data.frame(data[[1]]),fill="blue",colour="black",size=2)+
          scale_color_manual(name="",values = c("black"="black","red"="red"),breaks=c("black","red"),labels=c(expression("S"["AGE"]),expression("O"["AGE"])))+
          
          theme(text = element_text(family = "Times New Roman"),
                panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.text = element_text(size=rel(2.5)),axis.title = element_text(size=rel(3.5)),
                #   axis.text = element_text(size=rel),axis.title = element_text(size=75),axis.title.x = element_text(size=100),
                legend.title = element_text(size=rel(3)),
                legend.text=element_text(size=rel(4)), legend.position = "right",legend.key=element_rect(),legend.background =element_rect(fill="white",colour="black"),
                panel.background = (element_rect(colour="black",size=rel(2)))))
  dev.off()
  
  setEPS()
  postscript(file = epspath,width = 12,height = 8)
  print(ggplot(as.data.frame(data[[1]]),aes(x=data$P))+
          ylim(0,length(data[[1]]))+
          xlim(-0.1,1.1)+
          ylab("Individuals")+
          xlab("RoD")+
          geom_histogram(data =as.data.frame(data[[1]]),fill="blue",colour="black",size=2)+
          scale_color_manual(name="",values = c("black"="black","red"="red"),breaks=c("black","red"),labels=c(expression("S"["AGE"]),expression("O"["AGE"])))+
          
          theme(#text = element_text(family = "Times New Roman"),
            panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border = element_blank(),
            axis.text = element_text(size=rel(2.5)),axis.title = element_text(size=rel(3)),axis.title.x = element_text(size=rel(1.5)),
            #   axis.text = element_text(size=rel),axis.title = element_text(size=75),axis.title.x = element_text(size=100),
            legend.text=element_text(size=rel(2)), legend.position = "right",legend.key=element_rect(),legend.background =element_rect(fill="white",colour="black"),
            panel.background = (element_rect(colour="black",size=rel(2)))))
  dev.off()
}

Dong <- function(data,data0,dataname){
  datapath <- paste("D:/ROD/",dataname,".csv",sep = "")
  jpgpath <- paste("D:/ROD/",dataname,".jpg",sep = "")
  epspath <- paste("D:/ROD/",dataname,".eps",sep = "")
  dataf <- data.frame(data,data0)
  write.csv(dataf,file = datapath)
  jpeg(filename =jpgpath,width = 1600,height = 1200)
  print(ggplot(as.data.frame(data),aes(y=rod,x=ep))+
          ylab("RoD")+
          xlab(expression(epsilon))+
          geom_point(data =as.data.frame(data0),aes(colour="red"),size=6)+
          geom_line(data =as.data.frame(data0),aes(colour="red"),size=3)+
          geom_point(data =as.data.frame(data),aes(colour="black"),size=6)+
          geom_line(data =as.data.frame(data),aes(colour="black"),size=3)+
          scale_color_manual(name=dataname,values = c("black"="black","red"="red"),breaks=c("black","red"),labels=c(expression("S"["data"]),expression("O"["data"])))+
          
          theme(text = element_text(family = "Times New Roman"),
                panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border = element_blank(),
                axis.text = element_text(size=rel(2.5)),axis.title = element_text(size=rel(3.5)),axis.title.x = element_text(size=rel(2)),
                #   axis.text = element_text(size=rel),axis.title = element_text(size=75),axis.title.x = element_text(size=100),
                legend.title = element_text(size=rel(3)),
                legend.text=element_text(size=rel(4)), legend.position = "right",legend.key=element_rect(),legend.background =element_rect(fill="white",colour="black"),
                panel.background = (element_rect(colour="black",size=rel(2)))))
  dev.off()
  
  setEPS()
  postscript(file = epspath,width = 12,height = 8)
  print(ggplot(as.data.frame(data),aes(y=rod,x=ep))+
          ylab("RoD")+
          xlab(expression(epsilon))+
          geom_point(data =as.data.frame(data0),aes(colour="red"),size=6)+
          geom_line(data =as.data.frame(data0),aes(colour="red"),size=3)+
          geom_point(data =as.data.frame(data),aes(colour="black"),size=6)+
          geom_line(data =as.data.frame(data),aes(colour="black"),size=3)+
          scale_color_manual(name=dataname,values = c("black"="black","red"="red"),breaks=c("black","red"),labels=c(expression("S"["data"]),expression("O"["data"])))+
          
          theme(#text = element_text(family = "Times New Roman"),
            panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.border = element_blank(),
            axis.text = element_text(size=rel(2.5)),axis.title = element_text(size=rel(3)),axis.title.x = element_text(size=rel(1.5)),
            #   axis.text = element_text(size=rel),axis.title = element_text(size=75),axis.title.x = element_text(size=100),
            legend.text=element_text(size=rel(2)), legend.position = "right",legend.key=element_rect(),legend.background =element_rect(fill="white",colour="black"),
            panel.background = (element_rect(colour="black",size=rel(2)))))
  dev.off()
}


rlaplace(30, location = 0, scale = (1/0.1) )