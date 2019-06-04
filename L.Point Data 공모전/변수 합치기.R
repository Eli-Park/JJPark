library("data.table")
library("plyr")

sample_data = sampli

class_var =sample_data[12:51] 

class_var1=class_var
class_var1=t(class_var1)

mds<-cmdscale(as.matrix(dist(class_var1)))

plot(mds,type="n")
text(mds,labels=rownames(class_var1))

km=kmeans(mds,4)
plot(mds,col=km$cluster,pch=19,main="K=3")
points(km$centers,col=km$cluster,pch=8,cex=2)
text(mds,labels=rownames(class_var))
nas <- km[1]
nas <- unlist(nas)
sort(nas)
colnames(data7) <- gsub("/", "", colnames(data7))
colnames(data7) <- gsub(4, "사월", colnames(data7))
colnames(data7) <- gsub(5, "오월", colnames(data7))
colnames(data7) <- gsub(6, "육월", colnames(data7))
colnames(data7) <- gsub(7, "칠월", colnames(data7))
colnames(data7) <- gsub(8, "팔월", colnames(data7))
colnames(data7) <- gsub(9, "구월", colnames(data7))
data7.1 <- ddply(data7, .(CLNT_ID), summarise, var1 = sum(남성의류, 속옷양말홈웨어, 스포츠패션, 화장품뷰티케어, 여성의류상의, 여성의류전신, 유아동의류),
                                                var2 = sum(가구, 건강식품, 계절가전, 과일, 구기필드스포츠, 냉동식품, 냉장세탁가전, 냉장식품, 모바일, 문구사무용품, 상품권,
                                                             생활주방가전, 세제위생, 시즌스포츠, 식기조리기구, 아웃도어레저, 여성의류아우터, 여성의류하의, 영상음향가전,
                                                             완구, 원예애완, 음료, 인테리어조명, 자동차용품, 주방잡화, 축산물, 출산육아용품, 침구수예, 컴퓨터, 퍼스널케어, 헬스피트니스
                                                             ), var3 = sum(패션잡화))
data7.2 <- data7[,c(1:10)]
data7.3 <- ddply(data7, .(CLNT_ID), summarise, old = sum(사월, 오월, 육월), recent = sum(칠월, 팔월, 구월))
data8<-merge(data7.2, data7.1, by = "CLNT_ID")
data8 <- merge(data8, data7.3, by = "CLNT_ID")
data8 <- data8[,c(1:4, 11:15)]
data7$TotalExpense1 <- scale(data7$TotalExpense)
data7 <- data7[,-51]


#PCA

R=cor(data7[,c(-1, -2,-4)])
pca = princomp(R, cor = T)
summary(pca)

mat <- pca$scores
mat <- mat[,1:26]
pet <- data7[,c(-1,-2,-4)]
pet <- as.matrix(pet)
pcaa <- pet%*%mat

colnames(pcaa) <- c(paste("C",c(1:26),sep=""))
Final <- cbind(data8$CLNT_ID, pcaa)
head(Final)
Final <- as.data.frame(Final)
colnames(Final)[1] <- "CLNT_ID"
str(Final)

library('kselection')
KM1 <- kmeans(Final[,2:27], 8)
KM1
kselection(Final[,2:27])
ncol(data7)
data8$cluster <- KM1$cluster
colnames(data8)[11] <- "cluster"

write.csv(data8, "data8.csv")
