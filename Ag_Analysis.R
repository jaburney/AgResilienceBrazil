# ----------------------------------
# Ag_Analysis.R
# ----------------------------------
# Conducts regression analysis for paper, produces figures and tables
# Saves analysis in "Output/Ag_Climate_Analysis.RData"

# ----------------------------------
# Load data
# ----------------------------------
load(file="Output/Ag_Climate_Analysis.Rdata")

# ----------------------------------
# Get Price Data to Check
# ----------------------------------
df = df %>% mutate(price.r=value.r/quantity.t)

# ----------------------------------
# Clean up data - create two sets
# one signed shock components, 
# one absval 
# (comparison easier w/ same names)
# ----------------------------------

dat.signed = df %>% dplyr::select(!(contains("abs")))
dat.absval = df %>% mutate(t_seasonal=abs_t_seasonal,p_seasonal=abs_p_seasonal,
                            t_shock=abs_t_shock,p_shock=abs_p_shock,
                            t_shock_region=abs_t_shock_region,p_shock_region=abs_p_shock_region,
                            t_shock_state=abs_t_shock_state,p_shock_state=abs_p_shock_state,
                            t_shock_y=abs_t_shock_y,p_shock_y=abs_p_shock_y,
                            t_shock_y_region=abs_t_shock_y_region,p_shock_y_region=abs_p_shock_y_region,
                            t_shock_y_state=abs_t_shock_y_state,p_shock_y_state=abs_p_shock_y_state
)
dat.absval = dat.absval %>% dplyr::select(!(contains("abs")))

# ----------------------------------
# Run Models
# + exclude first (1990) and last (2014) years due to partial season
# + DVs: Yields, Revenue
# + IVs: linear, linear+quadratic
# + Scale: covariate and idiosyncratic shocks defined at state, regional, national levels
# + Comparison to regular FE models
# ----------------------------------

## Non-absolute Values
mods_nat = feols(c(log(yield.kgperha),log(value.r),log(price.r)) ~ t_hat1 + t_trend + t_seasonal + t_shock_y + t_shock + p_hat1 + p_trend + p_seasonal + p_shock_y + p_shock + 
                   sw0(t_seasonal**2 + t_shock_y**2 + t_shock**2 + p_seasonal**2 + p_shock_y**2 + p_shock**2),data=dat.signed,fsplit=~product)

mods_reg = feols(c(log(yield.kgperha),log(value.r),log(price.r)) ~ t_hat1 + t_trend + t_seasonal + t_shock_y_region + t_shock_region + p_hat1 + p_trend + p_seasonal + p_shock_y_region + p_shock_region + 
                   sw0(t_seasonal**2 + t_shock_y_region**2 + t_shock_region**2 + p_seasonal**2 + p_shock_y_region**2 + p_shock_region**2),data=dat.signed,fsplit=~product)

mods_sta = feols(c(log(yield.kgperha),log(value.r),log(price.r)) ~ t_hat1 + t_trend + t_seasonal + t_shock_y_state + t_shock_state + p_hat1 + p_trend + p_seasonal + p_shock_y_state + p_shock_state + 
                   sw0(t_seasonal**2 + t_shock_y_state**2 + t_shock_state**2 + p_seasonal**2 + p_shock_y_state**2 + p_shock_state**2),data=dat.signed,fsplit=~product)

## Absolute Values
mods_nat_absval = feols(c(log(yield.kgperha),log(value.r),log(price.r)) ~ t_hat1 + t_trend + t_seasonal + t_shock_y + t_shock + p_hat1 + p_trend + p_seasonal + p_shock_y + p_shock + 
                   sw0(t_seasonal**2 + t_shock_y**2 + t_shock**2 + p_seasonal**2 + p_shock_y**2 + p_shock**2),data=dat.absval,fsplit=~product)

mods_reg_absval = feols(c(log(yield.kgperha),log(value.r),log(price.r)) ~ t_hat1 + t_trend + t_seasonal + t_shock_y_region + t_shock_region + p_hat1 + p_trend + p_seasonal + p_shock_y_region + p_shock_region + 
                   sw0(t_seasonal**2 + t_shock_y_region**2 + t_shock_region**2 + p_seasonal**2 + p_shock_y_region**2 + p_shock_region**2),data=dat.absval,fsplit=~product)

mods_sta_absval = feols(c(log(yield.kgperha),log(value.r),log(price.r)) ~ t_hat1 + t_trend + t_seasonal + t_shock_y_state + t_shock_state + p_hat1 + p_trend + p_seasonal + p_shock_y_state + p_shock_state + 
                   sw0(t_seasonal**2 + t_shock_y_state**2 + t_shock_state**2 + p_seasonal**2 + p_shock_y_state**2 + p_shock_state**2),data=dat.absval,fsplit=~product)

## FE comparison models
# Usual municipality and year FE, season average T and total P
mods_fe = feols(c(log(yield.kgperha),log(value.r),log(price.r)) ~ temperature + precipitation + sw0(temperature**2 + precipitation**2) | id + year,vcov="twoway",data=dat.signed,fsplit=~product)

save(df,dat.signed,dat.absval,mods_nat,mods_nat_absval,mods_reg,mods_reg_absval,mods_sta,mods_sta_absval,mods_fe,file="Output/Ag_Climate_Analysis.RData")

# ----------------------------------
# Tables and Figures
# ----------------------------------

# First, basic regression tables and coefficient plots - for each product, with covariate at national or state level

prods.to.include = unique(df$product)
tbdir = "Output/Tables/Ag"
pltdir = "Output/Figures/AgCoefPlots"

for (i in 1:length(prods.to.include)) {
  
  # loop over products
  prod = prods.to.include[i]
  
  # yield tables and coefficient plots - national covariate - skip yield for "total"
  if (prod!="total") {
    etable(mods_nat[sample=prod,lhs=1],mods_reg[sample=prod,lhs=1],mods_sta[sample=prod,lhs=1],mods_fe[sample=prod,lhs=1],se="hetero",dict=my.dict,tex=TRUE,view=TRUE,fontsize="tiny",notes=c("1-2:national, 3-4:regional, 5-6:state, 7-8:municipal,fe; T in C, P in 100mm"),order=my.order,file=paste(tbdir,prods.to.include[i],"_yield.tex",sep=""))
    etable(mods_nat_absval[sample=prod,lhs=1],mods_reg_absval[sample=prod,lhs=1],mods_sta_absval[sample=prod,lhs=1],mods_fe[sample=prod,lhs=1],se="hetero",dict=my.dict,tex=TRUE,view=TRUE,fontsize="tiny",notes=c("1-2:national, 3-4:regional, 5-6:state, 7-8:municipal,fe; T in C, P in 100mm"),order=my.order,file=paste(tbdir,prods.to.include[i],"_absvals_yield.tex",sep=""))
  
    png(width=6,height=5,res=300,units="in",file=paste(pltdir,prod,"_Y_nat.png",sep=""))
      coefplot(c(mods_nat[sample=c(prod),lhs=1]),drop="(Intercept)",se="hetero",dict=my.dict,horiz=TRUE,main=paste("Effect on log(Yield)",prod,"National Covariate"))
    dev.off()
  
    png(width=6,height=5,res=300,units="in",file=paste(pltdir,prod,"_Y_sta.png",sep=""))
      coefplot(c(mods_sta[sample=c(prod),lhs=1]),drop="(Intercept)",se="hetero",dict=my.dict,horiz=TRUE,main=paste("Effect on log(Yield)",prod,"State Covariate"))
    dev.off()
  }
  
  # revenue tables and coefficient plots - national covariate
  etable(mods_nat[sample=prod,lhs=2],mods_reg[sample=prod,lhs=2],mods_sta[sample=prod,lhs=2],mods_fe[sample=prod,lhs=2],se="hetero",dict=my.dict,tex=TRUE,view=TRUE,fontsize="tiny",notes=c("1-2:national, 3-4:regional, 5-6:state, 7-8:municipal,fe; T in C, P in 100mm"),order=my.order,file=paste(tbdir,prods.to.include[i],"_revenue.tex",sep=""))
  etable(mods_nat_absval[sample=prod,lhs=2],mods_reg_absval[sample=prod,lhs=2],mods_sta_absval[sample=prod,lhs=2],mods_fe[sample=prod,lhs=2],se="hetero",dict=my.dict,tex=TRUE,view=TRUE,fontsize="tiny",notes=c("1-2:national, 3-4:regional, 5-6:state, 7-8:municipal,fe; T in C, P in 100mm"),order=my.order,file=paste(tbdir,prods.to.include[i],"_absvals_revenue.tex",sep=""))

  png(width=6,height=5,res=300,units="in",file=paste(pltdir,prod,"_Rev_nat.png",sep=""))
    coefplot(c(mods_nat[sample=c(prod),lhs=2]),drop="(Intercept)",se="hetero",dict=my.dict,horiz=TRUE,main=paste("Effect on log($R)",prod,"National Covariate"))
  dev.off()

  png(width=6,height=5,res=300,units="in",file=paste(pltdir,prod,"_Rev_sta.png",sep=""))
    coefplot(c(mods_sta[sample=c(prod),lhs=2]),drop="(Intercept)",se="hetero",dict=my.dict,horiz=TRUE,main=paste("Effect on log($R)",prod,"State Covariate"))
  dev.off()
  
  # prices tables and coefficient plots - national covariate
  etable(mods_nat[sample=prod,lhs=3],mods_reg[sample=prod,lhs=3],mods_sta[sample=prod,lhs=3],mods_fe[sample=prod,lhs=3],se="hetero",dict=my.dict,tex=TRUE,view=TRUE,fontsize="tiny",notes=c("1-2:national, 3-4:regional, 5-6:state, 7-8:municipal,fe; T in C, P in 100mm"),order=my.order,file=paste(tbdir,prods.to.include[i],"_price.tex",sep=""))
  etable(mods_nat_absval[sample=prod,lhs=3],mods_reg_absval[sample=prod,lhs=3],mods_sta_absval[sample=prod,lhs=3],mods_fe[sample=prod,lhs=3],se="hetero",dict=my.dict,tex=TRUE,view=TRUE,fontsize="tiny",notes=c("1-2:national, 3-4:regional, 5-6:state, 7-8:municipal,fe; T in C, P in 100mm"),order=my.order,file=paste(tbdir,prods.to.include[i],"_absvals_price.tex",sep=""))
  
  png(width=6,height=5,res=300,units="in",file=paste(pltdir,prod,"_Price_nat.png",sep=""))
    coefplot(c(mods_nat[sample=c(prod),lhs=3]),drop="(Intercept)",se="hetero",dict=my.dict,horiz=TRUE,main=paste("Effect on Price, log($R)",prod,"National Covariate"))
  dev.off()
  
  png(width=6,height=5,res=300,units="in",file=paste(pltdir,prod,"_Price_sta.png",sep=""))
    coefplot(c(mods_sta[sample=c(prod),lhs=3]),drop="(Intercept)",se="hetero",dict=my.dict,horiz=TRUE,main=paste("Effect on Price, log($R)",prod,"State Covariate"))
  dev.off()
  
}

# Second, nonlinear marginal impacts plots - for each product, with covariate at national level (can edit - see commented rows a few lines down; can also use splines, etc.)

pltdir = "Output/Figures/AgMIPlots/"

for (i in 1:length(prods.to.include)) {
  
  my.prod = prods.to.include[i]
  testdat = df %>% filter(yield.kgperha>0) %>% filter(value.r>0) %>% filter(price.r>0) %>% filter(product==my.prod)
  
  #testdat$t_shock = testdat$t_shock_state
  #testdat$p_shock = testdat$p_shock_state
  #testdat$t_shock_y = testdat$t_shock_y_state
  #testdat$p_shocky_ = testdat$p_shock_y_state
  
  mod_test = lm(log(yield.kgperha) ~ t_hat1 + t_trend + t_seasonal + t_shock_y + t_shock + p_hat1 + p_trend + p_seasonal + p_shock_y + p_shock + 
                  I(t_seasonal^2) + I(t_shock_y^2) + I(t_shock^2) + I(p_seasonal^2) + I(p_shock_y^2) + I(p_shock^2),data=testdat)
  #mod_test = lm(log(yield.kgperha) ~ t_hat1 + ns(t_trend,df=3) + ns(t_seasonal,df=3) + ns(t_shock_y,df=3) + ns(t_shock,df=3) + p_hat1 + ns(p_trend,df=3) + ns(p_seasonal,df=3) + ns(p_shock_y,df=3) + ns(p_shock,df=3),data=testdat)
  
  xpred=ggpredict(mod_test,terms="t_hat1 [all]",back.transform=TRUE)
    p.xpred = ggplot(xpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Baseline Temperature")+ylab("Predicted Yield ")
  trendpred=ggpredict(mod_test,terms="t_trend [all]",back.transform=TRUE)
    p.trendpred = ggplot(trendpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Temperature Trend")+ylab("Predicted Yield ")
  seaspred <- ggpredict(mod_test,terms="t_seasonal [all]",back.transform=TRUE)
    p.seaspred = ggplot(seaspred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Seasonal Temperature")+ylab("Predicted Yield ")
  covshockpred <- ggpredict(mod_test,terms="t_shock_y [all]",back.transform=TRUE)
    p.covshockpred = ggplot(covshockpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Cov. Temperature Shock")+ylab("Predicted Yield ")
  shockpred <- ggpredict(mod_test,terms="t_shock [all]",back.transform=TRUE)
    p.shockpred = ggplot(shockpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Temperature Shock")+ylab("Predicted Yield ")
  xpredp=ggpredict(mod_test,terms="p_hat1 [all]",back.transform=TRUE)
    p.xpredp = ggplot(xpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Baseline Precipitation")+ylab("Predicted Yield ")
  trendpredp=ggpredict(mod_test,terms="p_trend [all]",back.transform=TRUE)
    p.trendpredp = ggplot(trendpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Precipitation Trend")+ylab("Predicted Yield ")
  seaspredp <- ggpredict(mod_test,terms="p_seasonal [all]",back.transform=TRUE)
    p.seaspredp = ggplot(seaspredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Seasonal Precipitation")+ylab("Predicted Yield ")
  covshockpredp <- ggpredict(mod_test,terms="p_shock_y [all]",back.transform=TRUE)
    p.covshockpredp = ggplot(covshockpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Cov. Precipitation Shock")+ylab("Predicted Yield ")
  shockpredp <- ggpredict(mod_test,terms="p_shock [all]",back.transform=TRUE)
    p.shockpredp = ggplot(shockpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Precipitation Shock")+ylab("Predicted Yield ")
  
  ggarrange(p.xpred,p.trendpred,p.seaspred,p.covshockpred,p.shockpred,
            p.xpredp,p.trendpredp,p.seaspredp,p.covshockpredp,p.shockpredp,nrow=2,ncol=5)
  ggsave(paste(pltdir,my.prod,"_temp_y_fits_nonlog.png",sep=""),width=10,height=6,units="in",dpi=300)
  
  mod_test = lm(log(value.r) ~ t_hat1 + t_trend + t_seasonal + t_shock_y + t_shock + p_hat1 + p_trend + p_seasonal + p_shock_y + p_shock + 
                  I(t_seasonal^2) + I(t_shock_y^2) + I(t_shock^2) + I(p_seasonal^2) + I(p_shock_y^2) + I(p_shock^2),data=testdat)
  #mod_test = lm(log(value.r) ~ t_hat1 + ns(t_trend,df=3) + ns(t_seasonal,df=3) + ns(t_shock_y,df=3) + ns(t_shock,df=3) + p_hat1 + ns(p_trend,df=3) + ns(p_seasonal,df=3) + ns(p_shock_y,df=3) + ns(p_shock,df=3),data=testdat)
  
  xpred=ggpredict(mod_test,terms="t_hat1 [all]",back.transform=TRUE)
    p.xpred = ggplot(xpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Baseline Temperature")+ylab("Predicted Value ($R)")
  trendpred=ggpredict(mod_test,terms="t_trend [all]",back.transform=TRUE)
    p.trendpred = ggplot(trendpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Temperature Trend")+ylab("Predicted Value ($R)")
  seaspred <- ggpredict(mod_test,terms="t_seasonal [all]",back.transform=TRUE)
    p.seaspred = ggplot(seaspred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Seasonal Temperature")+ylab("Predicted Value ($R)")
  covshockpred <- ggpredict(mod_test,terms="t_shock_y [all]",back.transform=TRUE)
    p.covshockpred = ggplot(covshockpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Cov. Temperature Shock")+ylab("Predicted Value ($R)")
  shockpred <- ggpredict(mod_test,terms="t_shock [all]",back.transform=TRUE)
    p.shockpred = ggplot(shockpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Temperature Shock")+ylab("Predicted Value ($R)")
  xpredp=ggpredict(mod_test,terms="p_hat1 [all]",back.transform=TRUE)
    p.xpredp = ggplot(xpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Baseline Precipitation")+ylab("Predicted Value ($R)")
  trendpredp=ggpredict(mod_test,terms="p_trend [all]",back.transform=TRUE)
    p.trendpredp = ggplot(trendpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Precipitation Trend")+ylab("Predicted Value ($R)")
  seaspredp <- ggpredict(mod_test,terms="p_seasonal [all]",back.transform=TRUE)
    p.seaspredp = ggplot(seaspredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Seasonal Precipitation")+ylab("Predicted Value ($R)")
  covshockpredp <- ggpredict(mod_test,terms="p_shock_y [all]",back.transform=TRUE)
    p.covshockpredp = ggplot(covshockpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Cov. Precipitation Shock")+ylab("Predicted Value ($R)")
  shockpredp <- ggpredict(mod_test,terms="p_shock [all]",back.transform=TRUE)
    p.shockpredp = ggplot(shockpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Precipitation Shock")+ylab("Predicted Value ($R)")
  
  ggarrange(p.xpred,p.trendpred,p.seaspred,p.covshockpred,p.shockpred,
            p.xpredp,p.trendpredp,p.seaspredp,p.covshockpredp,p.shockpredp,nrow=2,ncol=5)
  ggsave(paste(pltdir,my.prod,"_temp_rev_fits_nonlog.png",sep=""),width=10,height=6,units="in",dpi=300)
  
  mod_test = lm(log(price.r) ~ t_hat1 + t_trend + t_seasonal + t_shock_y + t_shock + p_hat1 + p_trend + p_seasonal + p_shock_y + p_shock + 
                  I(t_seasonal^2) + I(t_shock_y^2) + I(t_shock^2) + I(p_seasonal^2) + I(p_shock_y^2) + I(p_shock^2),data=testdat)
  
  xpred=ggpredict(mod_test,terms="t_hat1 [all]",back.transform=TRUE)
  p.xpred = ggplot(xpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Baseline Temperature")+ylab("Predicted Value ($R)")
  trendpred=ggpredict(mod_test,terms="t_trend [all]",back.transform=TRUE)
  p.trendpred = ggplot(trendpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Temperature Trend")+ylab("Predicted Value ($R)")
  seaspred <- ggpredict(mod_test,terms="t_seasonal [all]",back.transform=TRUE)
  p.seaspred = ggplot(seaspred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Seasonal Temperature")+ylab("Predicted Value ($R)")
  covshockpred <- ggpredict(mod_test,terms="t_shock_y [all]",back.transform=TRUE)
  p.covshockpred = ggplot(covshockpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Cov. Temperature Shock")+ylab("Predicted Value ($R)")
  shockpred <- ggpredict(mod_test,terms="t_shock [all]",back.transform=TRUE)
  p.shockpred = ggplot(shockpred,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Temperature Shock")+ylab("Predicted Value ($R)")
  xpredp=ggpredict(mod_test,terms="p_hat1 [all]",back.transform=TRUE)
  p.xpredp = ggplot(xpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Baseline Precipitation")+ylab("Predicted Value ($R)")
  trendpredp=ggpredict(mod_test,terms="p_trend [all]",back.transform=TRUE)
  p.trendpredp = ggplot(trendpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Precipitation Trend")+ylab("Predicted Value ($R)")
  seaspredp <- ggpredict(mod_test,terms="p_seasonal [all]",back.transform=TRUE)
  p.seaspredp = ggplot(seaspredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Seasonal Precipitation")+ylab("Predicted Value ($R)")
  covshockpredp <- ggpredict(mod_test,terms="p_shock_y [all]",back.transform=TRUE)
  p.covshockpredp = ggplot(covshockpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Cov. Precipitation Shock")+ylab("Predicted Value ($R)")
  shockpredp <- ggpredict(mod_test,terms="p_shock [all]",back.transform=TRUE)
  p.shockpredp = ggplot(shockpredp,aes(x=x,y=predicted))+geom_ribbon(aes(ymax=conf.high,ymin=conf.low),fill="lightblue")+geom_line()+theme_minimal()+xlab("Precipitation Shock")+ylab("Predicted Value ($R)")
  
  ggarrange(p.xpred,p.trendpred,p.seaspred,p.covshockpred,p.shockpred,
            p.xpredp,p.trendpredp,p.seaspredp,p.covshockpredp,p.shockpredp,nrow=2,ncol=5)
  ggsave(paste(pltdir,my.prod,"_temp_price_fits_nonlog.png",sep=""),width=10,height=6,units="in",dpi=300)

}