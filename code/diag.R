require(ggplot2)
cleanbodyfat=read.csv("clean dataset.csv")
model=lm(cleanbodyfat[,2]~cleanbodyfat[,5]+cleanbodyfat[,10])
model=lm(BODYFAT~ ., data=cleanbodyfat[,c(-1,-3)])
#1.Residuals vs Fitted values Plot
p1=ggplot(model, aes(.fitted, .resid))+
    geom_point()+
    stat_smooth(method="loess")+
    geom_hline(yintercept=0, col="red", linetype="solid")+
     xlab("Fitted values")+ylab("Residuals")+
     ggtitle("Residuals vs Fitted values Plot")+
     theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))

#2.qq Plot  
p2=ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+
    geom_point(na.rm = TRUE)+
   geom_abline(intercept = 0, slope = 1, alpha = 0.5) +
    xlab("Theoretical Quantiles")+
    ylab("Standardized Residuals")+
    ggtitle("qq Plot")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))

 #3.Scale vs Location Plot  
 p3=ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+
   geom_point(na.rm=TRUE)+
   stat_smooth(method="loess", na.rm = TRUE)+
   xlab("Fitted Value")+
   ylab(expression(sqrt("|Standardized residuals|")))+
   ggtitle("Scale vs Location Plot")+
   theme_bw()+
   theme(plot.title = element_text(hjust = 0.5))

#4. Cook's distance Plot
  p4=ggplot(model, aes(seq_along(.cooksd), .cooksd))+
    geom_bar(stat="identity", position="identity")+
    xlab("Obs. Number")+
    ylab("Cook's distance")+
    ggtitle("Cook's distance Plot")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))

#5. Residual vs Leverage Plot
  p5=ggplot(model, aes(.hat, .stdresid))+
    geom_point(aes(size=.cooksd), na.rm=TRUE)+
    stat_smooth(method="loess", na.rm=TRUE)+
    xlab("Leverage")+ylab("Standardized Residuals")+
    ggtitle("Residual vs Leverage Plot")+
    scale_size_continuous("Cook's Distance", range=c(1,5))+
    theme_bw()+
    theme(legend.position="bottom")+
    theme(plot.title = element_text(hjust = 0.5))

#6. Cook's distance vs Leverage hii/(1-hii) Plot
  p6=ggplot(model, aes(.hat, .cooksd))+
    geom_point(na.rm=TRUE)+
    stat_smooth(method="loess", na.rm=TRUE)+
    xlab("Leverage hii")+ylab("Cook's Distance")+
    ggtitle("Cook's distance vs Leverage hii/(1-hii) Plot")+
    geom_abline(slope=seq(0,4,0.5), color="red", linetype="dashed")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  
 library(magrittr)
 library(multipanelfigure)
 figure1 <- multi_panel_figure(columns = 3, rows = 2, panel_label_type = "none") 
 figure1 %<>%
   fill_panel(p1, column = 1, row = 1) %<>%
   fill_panel(p2, column = 2, row = 1) %<>%
   fill_panel(p3, column = 3, row = 1) %<>%
   fill_panel(p4, column = 1, row = 2) %<>%
   fill_panel(p5, column = 2, row = 2) %<>%
   fill_panel(p6, column = 3, row = 2)
 figure1 
 