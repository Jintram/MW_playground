
# http://rstudio-pubs-static.s3.amazonaws.com/331052_6fddf169d2eb4cc1b666ca78103a4bdc.html

library(dagdata) # devtools::install_github("genomicsclass/dagdata")
library(rafalib) # install.packages(rafalib)

data(admissions)
admissions$total=admissions$Percent*admissions$Number/100

##percent men get in
sum(admissions$total[admissions$Gender==1]/sum(admissions$Number[admissions$Gender==1]))

##percent women get in
sum(admissions$total[admissions$Gender==0]/sum(admissions$Number[admissions$Gender==0]))

##make a 2 x 2 table
index = admissions$Gender==1
men = admissions[index,]
women = admissions[!index,]
menYes = sum(men$Number*men$Percent/100)
menNo = sum(men$Number*(1-men$Percent/100))
womenYes = sum(women$Number*women$Percent/100)
womenNo = sum(women$Number*(1-women$Percent/100))
tab = matrix(c(menYes,womenYes,menNo,womenNo),2,2)
print(chisq.test(tab)$p.val)


######

y=cbind(admissions[1:6,c(1,3)],admissions[7:12,3])
colnames(y)[2:3]=c("Male","Female")
y

#####

y=cbind(admissions[1:6,5],admissions[7:12,5])
y=sweep(y,2,colSums(y),"/")*100
x=rowMeans(cbind(admissions[1:6,3],admissions[7:12,3]))

rafalib::mypar()
matplot(x,y,xlab="percent that gets in the major",
        ylab="percent that applies to major",
        col=c("blue","red"),cex=1.5)
legend("topleft",c("Male","Female"),col=c("blue","red"),pch=c("1","2"),box.lty=0)



#####

rafalib::mypar()
malepoints <- vector("list",length(men))
femalepoints <- vector("list",length(women))
N<- length(males)
 
ADDY <- vector("numeric",N+1)
for(i in 1:N){
  malepoints[[i]] <- makematrix(males[[i]],NC,0,ADDY[i])
  femalepoints[[i]] <- makematrix(females[[i]],FNC,NC+NC/10,ADDY[i])
   ADDY[i+1] <- max(malepoints[[i]][,2],femalepoints[[i]][,2])+1
}

plot(0,type="n",
     xlim=c( min(sapply(malepoints,function(x)min(x[,1]))),max(sapply(femalepoints,function(x)max(x[,1])))),
  ylim=c(0,max(sapply(femalepoints,function(x)max(x[,2])))),xaxt="n",yaxt="n",xlab="",ylab="")
          
for(i in 1:N){
  points(malepoints[[i]],col=2+sort(-males[[i]]),pch=LETTERS[i],cex=CEX)
  points(femalepoints[[i]],col=2+sort(-females[[i]]),pch=LETTERS[i],cex=CEX)
  if(i>1) abline(h=ADDY[i])
  }
abline(v=NC+NC/20)
axis(side=3,c(NC/2,NC+FNC/2),c("Male","Female"),tick=FALSE)
axis(side=2,ADDY[-1]/2+ADDY[-length(ADDY)]/2,LETTERS[1:N],tick=FALSE,las=1)
