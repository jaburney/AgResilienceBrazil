# Nonlinear marginal impacts plots - for each product, with covariate at national level 
# (can edit - see commented rows a few lines down; can also use splines, etc.)

prods.to.include = unique(df$product)
coeffs = c(3,5,1)
pltdir = "Output/Figures/"
my.level = "national" #(state or national)
cut.low = FALSE

for (i in 1:length(prods.to.include)) {

  # select product
  my.prod = prods.to.include[i]

  # select scale coefficient (to put revenue and yields on similar scales)
  coeff = coeffs[i]
  
  # filter data
  testdat = df %>% filter(yield.kgperha>0) %>% filter(value.r>0) %>% filter(price.r>0) %>% filter(product==my.prod)
  
  # if eliminating low, cut out lowest quartile
  if (cut.low==TRUE) {
    testdat = testdat %>% filter(planted.ha>summary(testdat$planted.ha)[2])
  }
  
  # yield & revenue model (choose scale of shock above)
  if (my.level=="national") {
    
    mod_ytest = lm(log(yield.kgperha) ~ t_hat1 + t_trend + t_seasonal + t_shock_y + t_shock + p_hat1 + p_trend + p_seasonal + p_shock_y + p_shock + 
                  I(t_seasonal^2) + I(t_shock_y^2) + I(t_shock^2) + I(p_seasonal^2) + I(p_shock_y^2) + I(p_shock^2),data=testdat)
    
    mod_rtest = lm(log(value.r) ~ t_hat1 + t_trend + t_seasonal + t_shock_y + t_shock + p_hat1 + p_trend + p_seasonal + p_shock_y + p_shock + 
                     I(t_seasonal^2) + I(t_shock_y^2) + I(t_shock^2) + I(p_seasonal^2) + I(p_shock_y^2) + I(p_shock^2),data=testdat)
    
    } else {
    
    mod_ytest = lm(log(yield.kgperha) ~ t_hat1 + t_trend + t_seasonal + t_shock_y_state + t_shock_state + p_hat1 + p_trend + p_seasonal + p_shock_y_state + p_shock_state + 
                   I(t_seasonal^2) + I(t_shock_y_state^2) + I(t_shock_state^2) + I(p_seasonal^2) + I(p_shock_y_state^2) + I(p_shock_state^2),data=testdat)
  
    mod_rtest = lm(log(value.r) ~ t_hat1 + t_trend + t_seasonal + t_shock_y_state + t_shock_state + p_hat1 + p_trend + p_seasonal + p_shock_y_state + p_shock_state + 
                     I(t_seasonal^2) + I(t_shock_y_state^2) + I(t_shock_state^2) + I(p_seasonal^2) + I(p_shock_y_state^2) + I(p_shock_state^2),data=testdat)
    
    }
  
  
  # yield predictions
  xpred=ggpredict(mod_ytest,terms="t_hat1 [all]",back.transform=TRUE)
  trendpred=ggpredict(mod_ytest,terms="t_trend [all]",back.transform=TRUE)
  seaspred=ggpredict(mod_ytest,terms="t_seasonal [all]",back.transform=TRUE)
  xpredp=ggpredict(mod_ytest,terms="p_hat1 [all]",back.transform=TRUE)
  trendpredp=ggpredict(mod_ytest,terms="p_trend [all]",back.transform=TRUE)
  seaspredp=ggpredict(mod_ytest,terms="p_seasonal [all]",back.transform=TRUE)
 
  # revenue predictions
  xpredr=ggpredict(mod_rtest,terms="t_hat1 [all]",back.transform=TRUE)
  trendpredr=ggpredict(mod_rtest,terms="t_trend [all]",back.transform=TRUE)
  seaspredr=ggpredict(mod_rtest,terms="t_seasonal [all]",back.transform=TRUE)
  xpredpr=ggpredict(mod_rtest,terms="p_hat1 [all]",back.transform=TRUE)
  trendpredpr=ggpredict(mod_rtest,terms="p_trend [all]",back.transform=TRUE)
  seaspredpr=ggpredict(mod_rtest,terms="p_seasonal [all]",back.transform=TRUE)
  
  # shock terms for both
  if (my.level=="national") {
    covshockpred=ggpredict(mod_ytest,terms="t_shock_y [all]",back.transform=TRUE)
    shockpred=ggpredict(mod_ytest,terms="t_shock [all]",back.transform=TRUE)
    covshockpredp=ggpredict(mod_ytest,terms="p_shock_y [all]",back.transform=TRUE)
    shockpredp=ggpredict(mod_ytest,terms="p_shock [all]",back.transform=TRUE)
    
    covshockpredr=ggpredict(mod_rtest,terms="t_shock_y [all]",back.transform=TRUE)
    shockpredr=ggpredict(mod_rtest,terms="t_shock [all]",back.transform=TRUE)
    covshockpredpr=ggpredict(mod_rtest,terms="p_shock_y [all]",back.transform=TRUE)
    shockpredpr=ggpredict(mod_rtest,terms="p_shock [all]",back.transform=TRUE) 
  } else {
    covshockpred=ggpredict(mod_ytest,terms="t_shock_y_state [all]",back.transform=TRUE)
    shockpred=ggpredict(mod_ytest,terms="t_shock_state [all]",back.transform=TRUE)
    covshockpredp=ggpredict(mod_ytest,terms="p_shock_y_state [all]",back.transform=TRUE)
    shockpredp=ggpredict(mod_ytest,terms="p_shock_state [all]",back.transform=TRUE)
    
    covshockpredr=ggpredict(mod_rtest,terms="t_shock_y_state [all]",back.transform=TRUE)
    shockpredr=ggpredict(mod_rtest,terms="t_shock_state [all]",back.transform=TRUE)
    covshockpredpr=ggpredict(mod_rtest,terms="p_shock_y_state [all]",back.transform=TRUE)
    shockpredpr=ggpredict(mod_rtest,terms="p_shock_state [all]",back.transform=TRUE) 
  }
  
  p.xpred = ggplot(xpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="red")+geom_line()+
    geom_ribbon(data=xpredr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkorange",alpha=0.5)+geom_line(data=xpredr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Baseline Temperature")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis=sec_axis(~./coeff, name="Predicted Value ($R)"))

  p.trendpred = ggplot(trendpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="red")+geom_line()+
    geom_ribbon(data=trendpredr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkorange",alpha=0.5)+geom_line(data=trendpredr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Temperature Trend")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis=sec_axis(~./coeff, name="Predicted Value ($R)"))
  
  p.seaspred = ggplot(seaspred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="red")+geom_line()+
    geom_ribbon(data=seaspredr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkorange",alpha=0.5)+geom_line(data=seaspredr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Seasonal Temperature")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis=sec_axis(~./coeff, name="Predicted Value ($R)"),limits=c(0,5e3))

  # some manual limit adjustment
  if (my.prod %in% c("sorghum")) {
    p.seaspred = p.seaspred+scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis = sec_axis(~./coeff, name="Predicted Value ($R)"),limits=c(0,1e4))
  }
  
  p.covshockpred = ggplot(covshockpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="red")+geom_line()+
    geom_ribbon(data=covshockpredr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkorange",alpha=0.5)+geom_line(data=covshockpredr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Cov. Temperature Shock")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis=sec_axis(~./coeff, name="Predicted Value ($R)"))

  p.shockpred = ggplot(shockpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="red")+geom_line()+
    geom_ribbon(data=shockpredr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkorange",alpha=0.5)+geom_line(data=shockpredr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Temperature Shock")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis=sec_axis(~./coeff, name="Predicted Value ($R)"))

  # In dataset, p is in units of 100mm, so multiply back here to display more sensible numbers
  pfac = 100
  p.xpredp = ggplot(xpredp,aes(x=x*pfac,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="blue")+geom_line()+
    geom_ribbon(data=xpredpr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkgreen",alpha=0.5)+geom_line(data=xpredpr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Baseline Precipitation")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis = sec_axis(~./coeff, name="Predicted Value ($R)"))
 
  if (my.prod %in% c("sorghum","maize")) {
    p.xpredp = p.xpredp+scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis = sec_axis(~./coeff, name="Predicted Value ($R)"),limits=c(0,1e4))
  }

  p.trendpredp = ggplot(trendpredp,aes(x=x*pfac,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="blue")+geom_line()+
    geom_ribbon(data=trendpredpr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkgreen",alpha=0.5)+geom_line(data=trendpredpr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Precipitation Trend")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis = sec_axis(~./coeff, name="Predicted Value ($R)"))
  
  p.seaspredp = ggplot(seaspredp,aes(x=x*pfac,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="blue")+geom_line()+
    geom_ribbon(data=seaspredpr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkgreen",alpha=0.5)+geom_line(data=seaspredpr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Seasonal Precipitation")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis = sec_axis(~./coeff, name="Predicted Value ($R)"))

    # some manual limit adjustment
    if (my.prod=="soybeans") {
      p.seaspredp = p.seaspredp+scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis = sec_axis(~./coeff, name="Predicted Value ($R)"),limits=c(0,3e4))
    }
    
  p.covshockpredp = ggplot(covshockpredp,aes(x=x*pfac,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="blue")+geom_line()+
    geom_ribbon(data=covshockpredpr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkgreen",alpha=0.5)+geom_line(data=covshockpredpr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Cov. Precipitation Shock")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis = sec_axis(~./coeff, name="Predicted Value ($R)"))
  
  p.shockpredp = ggplot(shockpredp,aes(x=x*pfac,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="blue")+geom_line()+
    geom_ribbon(data=shockpredpr,aes(ymax=conf.high*coeff,ymin=conf.low*coeff),fill="darkgreen",alpha=0.5)+geom_line(data=shockpredpr,aes(y=predicted*coeff))+
    theme_minimal()+xlab("Precipitation Shock")+ylab("Predicted Yield ")+
    scale_y_continuous(name="Predicted Yield (kg/ha)",sec.axis = sec_axis(~./coeff, name="Predicted Value ($R)"))
  
  ggarrange(p.xpred,p.trendpred,p.seaspred,p.covshockpred,p.shockpred,
            p.xpredp,p.trendpredp,p.seaspredp,p.covshockpredp,p.shockpredp,nrow=2,ncol=5)
  
  if (my.level=="national") {
    ggsave(paste(pltdir,my.prod,"_cut",cut.low,"fancyfig2_nonlog.png",sep=""),width=15,height=6,units="in",dpi=300)
  } else {
    ggsave(paste(pltdir,my.prod,"_cut",cut.low,"fancyfig2_nonlog_state.png",sep=""),width=15,height=6,units="in",dpi=300)
  }
}
