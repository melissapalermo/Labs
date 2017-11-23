###CODIGOS

#CODIGOS
salmon<-read.table("salmon.txt",header=F)


library(MASS)
library(CCA)
library(xtable)
library(car)
library(plyr)
library(klaR)

set.seed(1)

#Banco de dados
#salmon = read.table("C:/Users/Fran/Desktop/Multivariada/Salmon.txt")
#salmon = read.table(url("http://www.ime.unicamp.br/~cnaber/Salmon.txt"))
colnames(salmon) = c("Procedencia","Sexo","Diam_doce","Diam_sal")
salmon[,1:2] = lapply(salmon[,1:2], as.factor)
salmon[,3:4] = lapply(salmon[,3:4], as.numeric)
levels(salmon$Sexo) = c("Fêmea","Macho")
levels(salmon$Procedencia) = c("Alasca","Canadá")

#Analise descritiva 
summary(salmon)

Procedencia = salmon[,1]

# Medidas resumo
med_doce = ddply(salmon,.(Procedencia),summarise,media=mean(Diam_doce),dp=sqrt(var(Diam_doce)),vari=var(Diam_doce),cv=100*((sqrt(var(Diam_doce))/mean(Diam_doce))),minimo=min(Diam_doce),mediana=quantile(Diam_doce,0.5),maximo=max(Diam_doce))
colnames(med_doce)<-c("Procedência","Média","DP","Var.","CV","Mínimo","Mediana","Máximo")
format(med_doce, digits=4, nsmall=4)

med_sal = ddply(salmon,.(Procedencia),summarise,media=mean(Diam_sal),dp=sqrt(var(Diam_sal)),vari=var(Diam_sal),cv=100*((sqrt(var(Diam_sal))/mean(Diam_sal))),minimo=min(Diam_sal),mediana=quantile(Diam_sal,0.5),maximo=max(Diam_sal))
colnames(medados)<-c("Procedência","Média","DP","Var.","CV","Mínimo","Mediana","Máximo")
format(med_sal, digits=4, nsmall=4)

#Gráfico de dispersão
alasca <- subset(salmon, salmon$Procedencia =="Alasca") 
canada <- subset(salmon, salmon$Procedencia == "Canadá") 

plot(alasca$Diam_doce, alasca$Diam_sal, pch = 20, xlim=c(50,200), ylim=c(300, 550), xlab = "Diâmetro água salgada", ylab="Diâmetro água doce")
points(canada$Diam_doce, canada$Diam_sal, col=2, pch=20)
legend("topright", legend =c("Alasca", "Canadá"), bty="n", cex=1, pch=c(20,20), col=c(1:2))

#histograma 
par(mfrow=c(2,2))
hist(alasca$Diam_doce,main="Alasca: Fase em água doce", xlab="Diâmetro das guelras", ylab="Frequência",)
hist(canada$Diam_doce,main="Canadá: Fase em água doce", xlab="Diâmetro das guelras", ylab="Frequência",)
hist(alasca$Diam_sal,main="Alasca: Fase em água salgada", xlab="Diâmetro das guelras", ylab="Frequência",)
hist(canada$Diam_sal,main="Canadá: Fase em água salgada", xlab="Diâmetro das guelras", ylab="Frequência",)

#Boxplot
par(mfrow=c(1,2))
plot(salmon[,3]~salmon[,1], xlab = "", ylab="Diâmetro das guelras", main="Diâmetro das guelras em água doce", cex.main=1)
plot(salmon[,4]~salmon[,1], xlab = "", ylab="Diâmetro das guelras", main = "Diâmetro das guelras em água salgada", cex.main=1)

# Gráficos das distâncias de Mahalanobis
salmonsd = salmon[,3:4]
par(mfrow=c(1,1))
mxS <- salmonsd[Procedencia=="Alasca",]
nS <- nrow(mxS)
vmuS <- apply(mxS,2,mean)
s2S <- cov(mxS)
mmuS <- t(matrix(t(vmuS),4,nS))
vQS<-nS*mahalanobis(mxS,center=vmuS,cov=s2S)
#
mxV <- salmonsd[Procedencia=="Canadá",]
nV <- nrow(mxV)
vmuV <- apply(mxV,2,mean)
s2V <- cov(mxV)
mmuV <- t(matrix(t(vmuV),4,nV))
vQV<-  nV*mahalanobis(mxV,center=vmuV,cov=s2V)
#
plot(density(vQS),lwd=2,xlab="valor",main="Distância de Mahalanobis",ylab="densidade",cex=1,cex.lab=1, ylim=c(0,0.008))
lines(density(vQV),lwd=2,cex=1,cex.lab=1,cex.main=1.2,col=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1.2)

# Densidades suavizadas
par(mfrow=c(1,1))
plot(density(salmon[Procedencia=="Alasca",3]),lwd=2,xlim=c(20,220),xlab="Diâmetro das guelras",ylab="densidade",cex=1,cex.lab=1,cex.main=1.2,main="Fase em água doce")
lines(density(salmon[Procedencia=="Canadá",3]),col=2,lwd=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1)

Procedencia = salmon[,1]
plot(density(salmon[Procedencia=="Alasca",4]),lwd=2,xlim=c(250,570),ylim=c(0,0.014),xlab="Diâmetro das guelras",ylab="densidade",cex=1,cex.lab=1,cex.main=1.2,main="Fase em água salgada")
lines(density(salmon[Procedencia=="Canadá",4]),col=2,lwd=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1)

p = ggplot(salmon, aes(salmon[,3], colour = salmon[,1])) + geom_density(alpha = 0.1)
p + theme_bw() + xlim(c(20,215)) + labs(x="Diâmetro das guelras", y="Densidade")

#envelope
par(mfrow=c(2,2))
qqPlot(scale(alasca[,3]),dist="norm",mean=0,sd=1,col.lines=2,grid="TRUE",xlab="quantil da N(0,1)",main="Alasca: Fase em água doce",ylab ="quantil da distribuição do diâmetro da guelra",cex=1,id.cex=1,pch=16)
qqPlot(scale(canada[,3]),dist="norm",mean=0,sd=1,col.lines=2,grid="TRUE",xlab="quantil da N(0,1)",main="Canadá: Fase em água doce",ylab ="quantil da distribuição do diâmetro da guelra",cex=1,id.cex=1, pch=16)
qqPlot(scale(alasca[,4]),dist="norm",mean=0,sd=1,col.lines=2,grid="TRUE",xlab="quantil da N(0,1)",main="Alasca: Fase em água salgada",ylab ="quantil da distribuição do diâmetro da guelra",cex=1,id.cex=1, pch=16)
qqPlot(scale(canada[,4]),dist="norm",mean=0,sd=1,col.lines=2,grid="TRUE",xlab="quantil da N(0,1)",main="Canadá: Fase em água salgada",ylab ="quantil da distribuição do diâmetro da guelra",cex=1,id.cex=1,pch=16)

#envelope apenas por grupo
especie <- salmon[,1]
nvar=2
mx <- salmon[,3:4]
vmu <- apply(mx[especie=="Alasca",],2,mean)
s2 <- cov(mx[especie=="Alasca",])
n0<-nrow(mx[especie=="Alasca",])
mmu <- t(matrix(t(vmu),nvar,n0))
vF <- apply(as.matrix((mx-vmu)*(mx-vmu))%*%as.matrix(solve(s2)),1,sum)
vF<- (n0-nvar)*vF/((n0-1)*nvar)
qqPlot(vF,dist="f",df1=nvar,df2=n0-nvar,col.lines=1,grid="FALSE",xlab="quantil da distribuição F",ylab="quantil da forma quadrática",cex=1.2,id.cex=1.2,main="Alasca")

vmu <- apply(mx[especie=="Canada",],2,mean)
s2 <- cov(mx[especie=="Canada",])
n0<-nrow(mx[especie=="Canada",])
mmu <- t(matrix(t(vmu),nvar,n0))
vF <- apply(as.matrix((mx-vmu)*(mx-vmu))%*%as.matrix(solve(s2)),1,sum)
vF<- (n0-nvar)*vF/((n0-1)*nvar)
qqPlot(vF,dist="f",df1=nvar,df2=n0-nvar,col.lines=1,grid="FALSE",xlab="quantil da distribuição F",ylab="quantil da forma quadrática",cex=1.2,id.cex=1.2,main="Canadá")



##Análise de discriminante

# Selecionando as amostras
set.seed(137647)
treinog1 <- sort(sample(1:50,25,replace=FALSE))
treinog2 <- sort(sample(51:100,25,replace=FALSE)) 
treino <- c(treinog1,treinog2)

# Utilizando a função lda
var = rbind(salmon[1:50,3:4],salmon[51:100,3:4])
Proc = rep(c("Alasca","Canadá"), rep(50,2))
train = salmon[treino,]

result.ad = lda(train$Procedencia~train$Diam_doce+train$Diam_sal, prior = c(0.6,0.4))
result.ad

test = salmon[-treino,]
pred = predict(result.ad, test[,3:4])$class

y  = predict(result.ad, salmon[-treino, ])$x 

tabela = table(test[,1],pred)
tabela

TEA <- (sum(tabela)-sum(diag(tabela)))/sum(tabela)
100*TEA

# TOE
Dados.1 = (train[train[,1]=="Alasca",][3:4])
Dados.2 = (train[train[,1]=="Canadá",][3:4])

v.mean1 = cbind(c(result.ad$means[1,]))
v.mean2 = cbind(c(result.ad$means[2,]))

S21 = cov(Dados.1)
S22 = cov(Dados.2)

Sp = ((nrow(Dados.1)-1)*S21 +  ((nrow(Dados.2)-1)*S22))/(nrow(Dados.1) + nrow(Dados.2) - 2)

delta2 = t(v.mean1-v.mean2)%*%solve(Sp)%*%(v.mean1 - v.mean2)

TOE = pnorm(-sqrt(delta2)/2)

100*TOE


##Análise considerando o grupo

set.seed(115824)
# Selecionando as amostras
gtreinog1 <- sort(sample(1:50,25,replace=FALSE))
gtreinog2 <- sort(sample(51:100,25,replace=FALSE)) 
gtreino <- c(treinog1,treinog2)

# Utilizando a função lda
gvar = rbind(salmon[1:50,2:4],salmon[51:100,2:4])
gProc = rep(c("Alasca","Canadá"), rep(50,2))
gtrain = salmon[treino,]

gresult.ad = lda(gtrain$Procedencia~gtrain$Diam_doce+gtrain$Diam_sal+gtrain$Genero,prior=c(0.6,0.4))
gresult.ad

gtest = salmon[-gtreino,]
gpred = predict(gresult.ad, gtest[,2:4])
gy  = predict(gresult.ad,gtest[,2:4])$x 


tabelatotal = data.frame(test$Procedencia,test$Genero,gpred$class)

tabelafemea = subset(tabelatotal, tabelatotal$test.Genero=="Femea")
tcf = table(tabelafemea$test.Procedencia,tabelafemea$gpred.class)
tcf

tabelamacho = subset(tabelatotal, tabelatotal$test.Genero=="Macho")
tcm = table(tabelamacho$test.Procedencia,tabelamacho$gpred.class)
tcm

TEAF = (sum(tcf)-sum(diag(tcf)))/sum(tcf)
100*TEAF

TEAM = (sum(tcm)-sum(diag(tcm)))/sum(tcm)
100*TEAM


###separando por sexo
#Gráfico de dispersão
alasca_f <- subset(salmon, salmon$Procedencia =="Alasca"&salmon$Sexo =="Fêmea")
alasca_m <- subset(salmon, salmon$Procedencia =="Alasca"&salmon$Sexo =="Macho") 

canada_f <- subset(salmon, salmon$Procedencia == "Canadá"&salmon$Sexo =="Fêmea") 
canada_m <- subset(salmon, salmon$Procedencia == "Canadá"&salmon$Sexo =="Macho") 

par(mfrow=c(1,2))
plot(alasca_f$Diam_doce, alasca_f$Diam_sal, pch = 20, xlim=c(50,200), ylim=c(300, 520), xlab = "Diâmetro água salgada", ylab="Diâmetro água doce", main="Fêmeas")
points(canada_f$Diam_doce, canada_f$Diam_sal, col=2, pch=20)
legend("topright", legend =c("Alasca", "Canadá"), bty="n", cex=1, pch=c(20,20), col=c(1:2))

plot(alasca_m$Diam_doce, alasca_m$Diam_sal, pch = 20, xlim=c(50,200), ylim=c(300, 520), xlab = "Diâmetro água salgada", ylab="Diâmetro água doce", main="Machos")
points(canada_m$Diam_doce, canada_m$Diam_sal, col=2, pch=20)
#histograma 

library(scales)
par(mfrow=c(2,2))
hist(alasca_f$Diam_doce, col=alpha("red", 0.5), ylim=c(0, 10),main="Alasca:Fase em água doce", xlab="Diâmetro das guelras", ylab="Frequência")
hist(alasca_m$Diam_doce, main=NULL, col=alpha("black", 0.5), add=T)

hist(canada_f$Diam_doce,main="Canadá: Fase em água doce", xlab="Diâmetro das guelras", ylab="Frequência",col=alpha("red", 0.5), ylim=c(0, 10))
hist(canada_m$Diam_doce, main=NULL, col=alpha("black", 0.5), add=T)

hist(alasca_f$Diam_sal,main="Alasca: Fase em água salgada", xlab="Diâmetro das guelras", ylab="Frequência",col=alpha("red", 0.5), ylim=c(0, 10))
hist(alasca_m$Diam_sal, main=NULL, col=alpha("black", 0.5), add=T)


hist(canada_f$Diam_sal,main="Canadá: Fase em água salgada", xlab="Diâmetro das guelras", ylab="Frequência",col=alpha("red", 0.5), ylim=c(0, 10))
hist(canada_m$Diam_sal, main=NULL, col=alpha("black", 0.5), add=T)
#boxplot
par(mfrow=c(2,2))
salmon_f = subset(salmon, salmon$Sexo =="Fêmea")
salmon_m = subset(salmon, salmon$Sexo =="Macho")

plot(salmon_f[,3]~salmon_f[,1], xlab = "Diâmetro das guelras em água doce", ylab="Diâmetro das guelras", main="Fêmeas", cex.main=1)
plot(salmon_f[,4]~salmon_f[,1], xlab = "Diâmetro das guelras em água salgada", ylab="Diâmetro das guelras", main = "Fêmeas", cex.main=1)

plot(salmon_m[,3]~salmon_m[,1], xlab = "Diâmetro das guelras em água doce", ylab="Diâmetro das guelras", main="Machos", cex.main=1)
plot(salmon_m[,4]~salmon_m[,1], xlab = "Diâmetro das guelras em água salgada", ylab="Diâmetro das guelras", main = "Machos", cex.main=1)

#densidades suavizadas 
plot(density(alasca_f[,3]),lwd=2,xlim=c(20,220), ylim=c(0,0.024),xlab="Diâmetro das guelras",ylab="Densidade para fêmeas",cex=1,cex.lab=1,cex.main=1.2,main="Fase em água doce")
lines(density(canada_f[,3]),col=2,lwd=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1)

plot(density(alasca_m[,3]),lwd=2,xlim=c(20,220), ylim=c(0,0.024),xlab="Diâmetro das guelras",ylab="Densidade  para machos",cex=1,cex.lab=1,cex.main=1.2,main="Fase em água doce")
lines(density(canada_m[,3]),col=2,lwd=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1)

plot(density(alasca_f[,4]),lwd=2,xlim=c(250,520), ylim=c(0,0.011),xlab="Diâmetro das guelras",ylab="Densidade para fêmeas",cex=1,cex.lab=1,cex.main=1.2,main="Fase em água salgada")
lines(density(canada_f[,4]),col=2,lwd=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1)

plot(density(alasca_m[,4]),lwd=2,xlim=c(250,570), ylim=c(0,0.014),xlab="Diâmetro das guelras",ylab="Densidade  para machos",cex=1,cex.lab=1,cex.main=1.2,main="Fase em água salgada")
lines(density(canada_m[,4]),col=2,lwd=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1)

# Medidas resumo
salmon_f = subset(salmon, salmon$Sexo =="Fêmea")

salmon_m = subset(salmon, salmon$Sexo =="Macho")

med_doce = ddply(salmon_f,.(Procedencia),summarise,media=mean(Diam_doce),dp=sqrt(var(Diam_doce)),vari=var(Diam_doce),cv=100*((sqrt(var(Diam_doce))/mean(Diam_doce))),minimo=min(Diam_doce),mediana=quantile(Diam_doce,0.5),maximo=max(Diam_doce))
colnames(med_doce)<-c("Procedência","Média","DP","Var.","CV","Mínimo","Mediana","Máximo")
format(med_doce, digits=4, nsmall=4)
med_doce = ddply(salmon_m,.(Procedencia),summarise,media=mean(Diam_doce),dp=sqrt(var(Diam_doce)),vari=var(Diam_doce),cv=100*((sqrt(var(Diam_doce))/mean(Diam_doce))),minimo=min(Diam_doce),mediana=quantile(Diam_doce,0.5),maximo=max(Diam_doce))
colnames(med_doce)<-c("Procedência","Média","DP","Var.","CV","Mínimo","Mediana","Máximo")
format(med_doce, digits=4, nsmall=4)

med_sal = ddply(salmon_f,.(Procedencia),summarise,media=mean(Diam_sal),dp=sqrt(var(Diam_sal)),vari=var(Diam_sal),cv=100*((sqrt(var(Diam_sal))/mean(Diam_sal))),minimo=min(Diam_sal),mediana=quantile(Diam_sal,0.5),maximo=max(Diam_sal))
colnames(med_sal)<-c("Procedência","Média","DP","Var.","CV","Mínimo","Mediana","Máximo")
format(med_sal, digits=4, nsmall=4)
med_sal = ddply(salmon_m,.(Procedencia),summarise,media=mean(Diam_sal),dp=sqrt(var(Diam_sal)),vari=var(Diam_sal),cv=100*((sqrt(var(Diam_sal))/mean(Diam_sal))),minimo=min(Diam_sal),mediana=quantile(Diam_sal,0.5),maximo=max(Diam_sal))
colnames(med_sal)<-c("Procedência","Média","DP","Var.","CV","Mínimo","Mediana","Máximo")
format(med_sal, digits=4, nsmall=4)

# Gráficos das distâncias de Mahalanobis
salmon_falasca = subset(salmon, salmon$Sexo =="Fêmea" & salmon$Procedencia=="Alasca")
mxS = salmon_falasca[,3:4]
par(mfrow=c(1,2))

nS <- nrow(mxS)
vmuS <- apply(mxS,2,mean)
s2S <- cov(mxS)
mmuS <- t(matrix(t(vmuS),4,nS))
vQS<-nS*mahalanobis(mxS,center=vmuS,cov=s2S)
#
salmon_fcana = subset(salmon, salmon$Sexo =="Fêmea" & salmon$Procedencia=="Canadá")
mxV <- salmon_fcana[,3:4]
nV <- nrow(mxV)
vmuV <- apply(mxV,2,mean)
s2V <- cov(mxV)
mmuV <- t(matrix(t(vmuV),4,nV))
vQV<-  nV*mahalanobis(mxV,center=vmuV,cov=s2V)
#
plot(density(vQS),lwd=2,xlab="Distância de Mahalanobis",main="Fêmeas",ylab="Densidade",cex=1,cex.lab=1, ylim=c(0,0.013))
lines(density(vQV),lwd=2,cex=1,cex.lab=1,cex.main=1.2,col=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1.2)

salmon_malasca = subset(salmon, salmon$Sexo =="Macho" & salmon$Procedencia=="Alasca")
mxS = salmon_malasca[,3:4]

nS <- nrow(mxS)
vmuS <- apply(mxS,2,mean)
s2S <- cov(mxS)
mmuS <- t(matrix(t(vmuS),4,nS))
vQS<-nS*mahalanobis(mxS,center=vmuS,cov=s2S)
#
salmon_mcana = subset(salmon, salmon$Sexo =="Macho" & salmon$Procedencia=="Canadá")
mxV <- salmon_fcana[,3:4]
nV <- nrow(mxV)
vmuV <- apply(mxV,2,mean)
s2V <- cov(mxV)
mmuV <- t(matrix(t(vmuV),4,nV))
vQV<-  nV*mahalanobis(mxV,center=vmuV,cov=s2V)
#

plot(density(vQS),lwd=2,xlab="Distância de Mahalanobis",main="Machos",ylab="Densidade",cex=1,cex.lab=1, ylim=c(0,0.013))
lines(density(vQV),lwd=2,cex=1,cex.lab=1,cex.main=1.2,col=2)
legend("topright",lwd=c(2,2,2),col=c(1,2,3),legend=c("Alasca","Canadá"),bty="n",cex=1.2)

