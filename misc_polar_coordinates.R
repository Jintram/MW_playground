

# ################################################################################
# Trying to automatically encircle a cluster
# ################################################################################


# ################################################################################
# Using ggalt package
# Explanation: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

# install 'ggalt' pkg
# devtools::install_github("hrbrmstr/ggalt")

library(ggalt)

ggplot(data=df_tsne_3sets_goi)+
    geom_point(aes(x=tSNE1_Mcle_he,y=tSNE2_Mcle_he, color=Cluster_Monocle))+
    geom_encircle(data=df_tsne_3sets_goi[df_tsne_3sets_goi$Cluster_Monocle==4,],aes(x=tSNE1_Mcle_he,y=tSNE2_Mcle_he),expand=0)
    theme(legend.position ='top')+
    give_better_textsize_plot(15)+
    ggtitle('RaceID3 analysis\nTTN expression')+
    scale_color_gradientn(colors=c('lightblue1','black'))

geom_encircle() 

# ################################################################################
# My own simple code

x<-runif(100)
y<-runif(100)

x<-x-mean(x)
y<-y-mean(y)

topolar<-function(x,y){
    
    # calculate angles 
    alphas<-atan(y/x)
    
    # correct angles per quadrant
    quad2<-which(x<0&y>0)
    quad3<-which(x<0&y<0)
    quad4<-which(x>0&y<0)
    alphas[quad2]<-alphas[quad2]+pi
    alphas[quad3]<-alphas[quad3]+pi
    alphas[quad4]<-alphas[quad4]+2*pi
    
    # calculate distances to 0,0
    r<-sqrt(x^2+y^2)
    
    # create output
    polar<-data.frame(alphas=alphas,r=r)
    
}

ggplot(data.frame(x=x,y=y),aes(x=x,y=y))+
    geom_point()

polarout<-topolar(x,y)
alphas<-polarout$alphas
r<-polarout$r
hist(alphas,seq(0,2*pi+.1,.1))

ggplot(data.frame(x=x,y=y,alpha=round(alphas/(2*pi)*360,0)),aes(x=x,y=y,label=alpha))+
    geom_point()+
    geom_text()

ggplot(data.frame(x=x,y=y,r=round(polarout$r,2),0),aes(x=x,y=y,label=r))+
    geom_point()+
    geom_text()

mybins<-cut(alphas,labels=F,breaks = seq(0,2*pi,2*pi/10))
#mybins<-cut(alphas,labels=F,breaks = 10)

maxpoints<-integer()
for (ii in 1:10) {
    temp_var<-r
    temp_var[mybins!=ii]<-NaN
    maxpoints[ii] <- which.max(temp_var)
}
alphas[maxpoints]

ggplot(data.frame(x=x,y=y,mybins=mybins),aes(x=x,y=y))+
    geom_point(aes(color=as.factor(mybins)))+
    geom_line(data=data.frame(x=x[maxpoints],y=y[maxpoints]),aes(x=x,y=y))





