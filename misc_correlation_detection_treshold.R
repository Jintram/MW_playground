
# Example of correlation and detection limit ========================================

var_c <- runif(50, min=0, max=DEP_MAX)
var_x <- runif(50, min=0, max=IND_MAX)+var_c
var_y <- runif(50, min=0, max=IND_MAX)+var_c

# plot
df_xy<-data_frame(x=var_x,y=var_y)
ggplot(data=df_xy)+
  geom_point(aes(x=x,y=y))

# Check correlation
cor.test(var_x,var_y)

# Systematic approach =====================

# Range of interaction and noise
IND_MAX=50 # independent parameter max (noise range)
DEP_MAX=50 # dependent parameter max (interaction range)

# Initialize plot
p<-ggplot()+
  give_better_textsize_plot(15)+
  xlab('Signal detected above ..')+ylab('Correlation')

for (repeats in seq(1,5)) {

  # Example of correlation and detection limit ========================================

  var_c <- runif(50, min=0, max=DEP_MAX)
  var_x <- runif(50, min=0, max=IND_MAX)+var_c
  var_y <- runif(50, min=0, max=IND_MAX)+var_c
  
  # plot
  df_xy<-data_frame(x=var_x,y=var_y)
  ggplot(data=df_xy)+
    geom_point(aes(x=x,y=y))
  
  # Check correlation
  cor.test(var_x,var_y)
  
  # Now introduce an artificial detection limit ========================================
  
  LIMX <- 50
  LIMY <- 50
  var_x_det <- var_x
  var_x_det[var_x_det<LIMX] <- LIMX
  var_y_det <- var_y
  var_y_det[var_y_det<LIMY] <- LIMY
  
  # plot
  df_xy<-data_frame(x=var_x_det,y=var_y_det)
  ggplot(data=df_xy)+
    geom_point(aes(x=x,y=y))
  
  # Check correlation
  cor.test(var_x,var_y)
  
  # Now sweep over parameter range and calculate corr for range ======================================== 
  
  # Set a sequence of detection limits
  LIMSEQ <- seq(0,100) 
  
  # Loop over it, applying symmetric detection limit
  corrs<-double()
  corrs_select<-double()
  for (LIMX in LIMSEQ) {
  
    LIMY <- LIMX
    
    # apply limit
    var_x_det <- var_x
    var_x_det[var_x_det<LIMX] <- LIMX
    var_y_det <- var_y
    var_y_det[var_y_det<LIMY] <- LIMY
    
    # Check correlation
    out<-cor.test(var_x_det,var_y_det)
    
    # Check correlation filtering out points
    both_idx<-which(var_x_det>LIMX & var_y_det>LIMY)
    if (length(both_idx)>2){
      out_select<-cor.test(var_x_det[both_idx],var_y_det[both_idx])
    } else {
      out<-list()
      out$estimate<-NaN
    }
    
    # keep list
    corrs[length(corrs)+1]               <-out$estimate
    corrs_select[length(corrs_select)+1] <-out_select$estimate
    
  }
  
  # Remove one-sided
  corrs_asymm<-double()
  for (LIMX in LIMSEQ) {
    
    # apply limit
    var_x_det <- var_x
    var_x_det[var_x_det<LIMX] <- LIMX
    var_y_det <- var_y
    #var_y_det[var_y_det<LIMY] <- LIMY
    
    # Check correlation
    out<-cor.test(var_x_det,var_y_det)
    
    # keep list
    corrs_asymm[length(corrs_asymm)+1]<-out$estimate
    
  }
  
  # and add to plot
  df_corrs<-data_frame(limit=LIMSEQ, corr=corrs,corr_asymm=corrs_asymm,corr_select=corrs_select)
  p<-p+
    geom_point(data=df_corrs,aes(x=limit,y=corr))+
    geom_point(data=df_corrs,aes(x=limit,y=corr_asymm),color='red')+
    geom_line(data=df_corrs,aes(x=limit,y=corr_select),color='black')
  
}

# show plot
p




