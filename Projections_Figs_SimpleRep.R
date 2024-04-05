# ------------------------------------------------------------------------------------
# Projections_Figs_SimpleRep.R
# ------------------------------------------------------------------------------------
# Creates projections plots (Figure 3, Supplemental Figs)
# Only ag info (no FI info)
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# (1) Maize Yields
# ------------------------------------------------------------------------------------

# Decomposition (Default)
maize.trends.t.h = ag.coeffs$lmy[3]*int.trend.hist.t[,2]*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F)
maize.seas.t.h = ag.coeffs$lmy[4]*matrix(rep(maize.t.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[12]*matrix(rep((maize.t.seas.hist)^2,times=27),nrow=nrow(municipalities),byrow=F)
maize.cov.t.h = ag.coeffs$lmy[5]*matrix(rep(maize.t.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[13]*matrix(rep((maize.t.shock.cov.hist)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T)
maize.id.t.h = ag.coeffs$lmy[6]*maize.t.shock.id.hist + 
  ag.coeffs$lmy[14]*(maize.t.shock.id.hist)^2

maize.trends.p.h = ag.coeffs$lmy[3]*int.trend.hist.p[,2]/fac*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F)
maize.seas.p.h = ag.coeffs$lmy[4]*matrix(rep(maize.p.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[12]*matrix(rep((maize.p.seas.hist)^2,times=27),nrow=nrow(municipalities),byrow=F)
maize.cov.p.h = ag.coeffs$lmy[5]*matrix(rep(maize.p.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[13]*matrix(rep((maize.p.shock.cov.hist)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T)
maize.id.p.h = ag.coeffs$lmy[6]*maize.p.shock.id.hist + 
  ag.coeffs$lmy[14]*(maize.p.shock.id.hist)^2

maize.trends.t.f = ag.coeffs$lmy[3]*int.trend.future.t[,2]*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F)
maize.seas.t.f = ag.coeffs$lmy[4]*matrix(rep(maize.t.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[12]*matrix(rep((maize.t.seas.future)^2,times=53),nrow=nrow(municipalities),byrow=F)
maize.cov.t.f = ag.coeffs$lmy[5]*matrix(rep(maize.t.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[13]*matrix(rep((maize.t.shock.cov.future)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T)
maize.id.t.f = ag.coeffs$lmy[6]*maize.t.shock.id.future + 
  ag.coeffs$lmy[14]*(maize.t.shock.id.future)^2

maize.trends.p.f = ag.coeffs$lmy[3]*int.trend.future.p[,2]/fac*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F)
maize.seas.p.f = ag.coeffs$lmy[4]*matrix(rep(maize.p.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[12]*matrix(rep((maize.p.seas.future)^2,times=53),nrow=nrow(municipalities),byrow=F)
maize.cov.p.f = ag.coeffs$lmy[5]*matrix(rep(maize.p.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[13]*matrix(rep((maize.p.shock.cov.future)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T)
maize.id.p.f = ag.coeffs$lmy[6]*maize.p.shock.id.future + 
  ag.coeffs$lmy[14]*(maize.p.shock.id.future)^2


p1 = t.test(as.vector(maize.trends.t.h[,1:12]),as.vector(maize.trends.t.f[,1:12]),paired=T)
p2 = t.test(as.vector(maize.seas.t.h[,1:12]),as.vector(maize.seas.t.f[,1:12]),paired=T)
p3 = t.test(as.vector(maize.cov.t.h[1,1:25]),as.vector(maize.cov.t.f[1,29:53]),paired=F)
p4 = t.test(matrix(maize.id.t.h[,1:25],ncol=1),matrix(maize.id.t.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/Yield_Decomposition_Impacts_T.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(maize.trends.t.h[,1]),as.vector(maize.trends.t.f[,1]),as.vector(maize.seas.t.h[,1:12]),as.vector(maize.seas.t.f[,1:12]),as.vector(maize.cov.t.h[1,]),as.vector(maize.cov.t.f[1,]),as.vector(maize.id.t.h[,]),as.vector(maize.id.t.f[,]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F,ylim=c(-0.6,0.2))
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(Yield) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("T Trends","T Seasonality","T Covariate Shock","T Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,0.1,expression(paste(Delta," = 0.0026 (***)")),cex=1.5)
  text(3.5,0.1,expression(paste(Delta," = -0.035 (***)")),cex=1.5)
  text(5.5,0.1,expression(paste(Delta," = 0.0031 (NS)")),cex=1.5,col="orange3")
  text(7.5,0.1,expression(paste(Delta," = 0.01 (***)")),cex=1.5)
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

p5 = t.test(as.vector(maize.trends.p.h[,1:12]),as.vector(maize.trends.p.f[,1:12]),paired=T)
p6 = t.test(as.vector(maize.seas.p.h[,1:12]),as.vector(maize.seas.p.f[,1:12]),paired=T)
p7 = t.test(as.vector(maize.cov.p.h[1,1:25]),as.vector(maize.cov.p.f[1,29:53]),paired=F)
p8 = t.test(matrix(maize.id.p.h[,1:25],ncol=1),matrix(maize.id.p.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/Yield_Decomposition_Impacts_P.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(maize.trends.p.h[,1]),as.vector(maize.trends.p.f[,1]),as.vector(maize.seas.p.h[,1:12]),as.vector(maize.seas.p.f[,1:12]),as.vector(maize.cov.p.h[1,1:25]),as.vector(maize.cov.p.f[1,29:53]),as.vector(maize.id.p.h[,1:25]),as.vector(maize.id.p.f[,29:53]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F,ylim=c(-2.5,1))
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(Y) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("P Trends","P Seasonality","P Covariate Shock","P Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,1,expression(paste(Delta," = -0.0019 (***)")),cex=1.5)
  text(3.5,1,expression(paste(Delta," = 0.097 (***)")),cex=1.5)
  text(5.5,1,expression(paste(Delta," = -0.002 (NS)")),cex=1.5,col="orange3")
  text(7.5,1,expression(paste(Delta," = -0.07 (***)")),cex=1.5)
  #legend("topright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

p9 = t.test(as.vector(maize.trends.t.h[,1:12]+maize.trends.p.h[,1:12]),as.vector(maize.trends.t.f[,1:12]+maize.trends.p.f[,1:12]),paired=T)
p10 = t.test(as.vector(maize.seas.t.h[,1:12]+maize.seas.p.h[,1:12]),as.vector(maize.seas.t.f[,1:12]+maize.seas.p.f[,1:12]),paired=T)
p11 = t.test(as.vector(maize.cov.t.h[1,1:25]+maize.cov.p.h[1,1:25]),as.vector(maize.cov.t.f[1,29:53]+maize.cov.p.f[1,29:53]),paired=F)
p12 = t.test(matrix(maize.id.t.h[,1:25],ncol=1)+matrix(maize.id.p.h[,1:25],ncol=1),
             matrix(maize.id.t.f[,29:53],ncol=1)+matrix(maize.id.p.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/Yield_Decomposition_Impacts_TandP.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(maize.trends.t.h[,1]+maize.trends.p.h[,1]),as.vector(maize.trends.t.f[,1]+maize.trends.p.f[,1]),
          as.vector(maize.seas.t.h[,1:12]+maize.seas.p.h[,1:12]),as.vector(maize.seas.t.f[,1:12]+maize.seas.p.f[,1:12]),
          as.vector(maize.cov.t.h[1,1:25]+maize.cov.p.h[1,1:25]),as.vector(maize.cov.p.f[1,]),as.vector(maize.id.p.h[,]),as.vector(maize.id.p.f[,]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F)
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(Y) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("Trends","Seasonality","Covariate Shock","Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,0.5,expression(paste(Delta," = 6.8e-4 (***)")),cex=1.5)
  text(3.5,0.5,expression(paste(Delta," = 0.063 (***)")),cex=1.5)
  text(5.5,0.5,expression(paste(Delta," = 0.0005 (NS)")),cex=1.5,col="orange3")
  text(7.5,0.5,expression(paste(Delta," = -0.06 (***)")),cex=1.5)
  #legend("topright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

# Density plots and box plots
png(width=6,height=6,units="in",res=300,file="Output/Figures/Projections/Yield_Dists.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-100,-100,ylim=c(0,1),xlim=c(4,10),ylab="Density",xlab="log(Yields)")
  lines(density(maize.yields[!is.na(maize.yields)]),lwd=2,col="black")
  lines(density(maize.future.yields[!is.na(maize.yields)]),lwd=2,col="blue",lty=2)
dev.off()  

# Density plots and box plots
hist.order = order(apply(maize.yields[,1:25],2,median,na.rm=T))
future.order = order(apply(maize.future.yields[,29:53],2,median,na.rm=T))+28
png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/Maize_Yields_Year_Dist.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-1000,-1000,xlim=c(1,25),ylim=c(0,12),xlab="",ylab="(log) Maize Yields",xaxt="n")
  grid(col="darkgrey")
  boxplot(maize.yields[,hist.order],outline=T,add=T,xaxt="n",whisklty=1,whiskcol="black",lwd=2,outpch="+",outcol="black")
  boxplot(maize.future.yields[,future.order],col="blue",boxwex=0.4,xaxt="n",add=T,outline=T,whisklty=1,whiskcol="blue",outcol="red")
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

mun.sd.h = apply(maize.yields[,1:25],1,sd,na.rm=T)
mun.sd.f = apply(maize.future.yields[,29:53],1,sd,na.rm=T)
png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/Maize_Yields_Mun_SD.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-100,-100,ylim=c(0,8),xlim=c(0,1),ylab="Density",xlab="sd(log(Yields))")
  lines(density(mun.sd.h[!is.na(mun.sd.h)]),lwd=2,col="black")
  lines(density(mun.sd.f[!is.na(mun.sd.f)]),lwd=2,col="blue",lty=2)
dev.off()  

png(width=6,height=6,units="in",res=300,file="Output/Figures/Projections/Maize_Yields_Year_CDF.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-1000,-1000,xlim=c(5,10),ylim=c(0,1),xlab="log(Yield)",ylab="CDF",xaxt="n")
  grid(col="darkgrey")
  lines(ecdf(maize.yields),col="black",lwd=2)
  lines(ecdf(maize.future.yields),col="blue",lwd=2)
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

# ------------------------------------------------------------------------------------
# (2) Maize Revenue
# ------------------------------------------------------------------------------------

# Decomposition (Default)
maize.trends.t.h = ag.coeffs$lmr[3]*int.trend.hist.t[,2]*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F)
maize.seas.t.h = ag.coeffs$lmr[4]*matrix(rep(maize.t.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[12]*matrix(rep((maize.t.seas.hist)^2,times=27),nrow=nrow(municipalities),byrow=F)
maize.cov.t.h = ag.coeffs$lmr[5]*matrix(rep(maize.t.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[13]*matrix(rep((maize.t.shock.cov.hist)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T)
maize.id.t.h = ag.coeffs$lmr[6]*maize.t.shock.id.hist + 
  ag.coeffs$lmr[14]*(maize.t.shock.id.hist)^2

maize.trends.p.h = ag.coeffs$lmr[3]*int.trend.hist.p[,2]/fac*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F)
maize.seas.p.h = ag.coeffs$lmr[4]*matrix(rep(maize.p.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[12]*matrix(rep((maize.p.seas.hist)^2,times=27),nrow=nrow(municipalities),byrow=F)
maize.cov.p.h = ag.coeffs$lmr[5]*matrix(rep(maize.p.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[13]*matrix(rep((maize.p.shock.cov.hist)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T)
maize.id.p.h = ag.coeffs$lmr[6]*maize.p.shock.id.hist + 
  ag.coeffs$lmr[14]*(maize.p.shock.id.hist)^2

maize.trends.t.f = ag.coeffs$lmr[3]*int.trend.future.t[,2]*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F)
maize.seas.t.f = ag.coeffs$lmr[4]*matrix(rep(maize.t.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[12]*matrix(rep((maize.t.seas.future)^2,times=53),nrow=nrow(municipalities),byrow=F)
maize.cov.t.f = ag.coeffs$lmr[5]*matrix(rep(maize.t.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[13]*matrix(rep((maize.t.shock.cov.future)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T)
maize.id.t.f = ag.coeffs$lmr[6]*maize.t.shock.id.future + 
  ag.coeffs$lmr[14]*(maize.t.shock.id.future)^2

maize.trends.p.f = ag.coeffs$lmr[3]*int.trend.future.p[,2]/fac*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F)
maize.seas.p.f = ag.coeffs$lmr[4]*matrix(rep(maize.p.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[12]*matrix(rep((maize.p.seas.future)^2,times=53),nrow=nrow(municipalities),byrow=F)
maize.cov.p.f = ag.coeffs$lmr[5]*matrix(rep(maize.p.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[13]*matrix(rep((maize.p.shock.cov.future)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T)
maize.id.p.f = ag.coeffs$lmr[6]*maize.p.shock.id.future + 
  ag.coeffs$lmr[14]*(maize.p.shock.id.future)^2


p1 = t.test(as.vector(maize.trends.t.h[,1:12]),as.vector(maize.trends.t.f[,1:12]),paired=T)
p2 = t.test(as.vector(maize.seas.t.h[,1:12]),as.vector(maize.seas.t.f[,1:12]),paired=T)
p3 = t.test(as.vector(maize.cov.t.h[1,1:25]),as.vector(maize.cov.t.f[1,29:53]),paired=F)
p4 = t.test(matrix(maize.id.t.h[,1:25],ncol=1),matrix(maize.id.t.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/MaizeRevenue_Decomposition_Impacts_T.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(maize.trends.t.h[,1]),as.vector(maize.trends.t.f[,1]),as.vector(maize.seas.t.h[,1:12]),as.vector(maize.seas.t.f[,1:12]),as.vector(maize.cov.t.h[1,]),as.vector(maize.cov.t.f[1,]),as.vector(maize.id.t.h[,]),as.vector(maize.id.t.f[,]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F,ylim=c(-0.6,0.2))
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(Revenue) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("T Trends","T Seasonality","T Covariate Shock","T Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,0.1,expression(paste(Delta," = 0.0026 (***)")),cex=1.5)
  text(3.5,0.1,expression(paste(Delta," = -0.024 (***)")),cex=1.5)
  text(5.5,0.1,expression(paste(Delta," = 0.025 (NS)")),cex=1.5,col="orange3")
  text(7.5,0.1,expression(paste(Delta," = 0.037 (***)")),cex=1.5)
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

p5 = t.test(as.vector(maize.trends.p.h[,1:12]),as.vector(maize.trends.p.f[,1:12]),paired=T)
p6 = t.test(as.vector(maize.seas.p.h[,1:12]),as.vector(maize.seas.p.f[,1:12]),paired=T)
p7 = t.test(as.vector(maize.cov.p.h[1,1:25]),as.vector(maize.cov.p.f[1,29:53]),paired=F)
p8 = t.test(matrix(maize.id.p.h[,1:25],ncol=1),matrix(maize.id.p.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/MaizeRevenue_Decomposition_Impacts_P.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(maize.trends.p.h[,1]),as.vector(maize.trends.p.f[,1]),as.vector(maize.seas.p.h[,1:12]),as.vector(maize.seas.p.f[,1:12]),as.vector(maize.cov.p.h[1,1:25]),as.vector(maize.cov.p.f[1,29:53]),as.vector(maize.id.p.h[,1:25]),as.vector(maize.id.p.f[,29:53]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F,ylim=c(-3.5,1))
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(Revenue) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("P Trends","P Seasonality","P Covariate Shock","P Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,1,expression(paste(Delta," = -0.019 (***)")),cex=1.5)
  text(3.5,1,expression(paste(Delta," = 0.07 (***)")),cex=1.5)
  text(5.5,1,expression(paste(Delta," = -0.01 (NS)")),cex=1.5,col="orange3")
  text(7.5,1,expression(paste(Delta," = -0.25 (***)")),cex=1.5)
  #legend("topright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

p9 = t.test(as.vector(maize.trends.t.h[,1:12]+maize.trends.p.h[,1:12]),as.vector(maize.trends.t.f[,1:12]+maize.trends.p.f[,1:12]),paired=T)
p10 = t.test(as.vector(maize.seas.t.h[,1:12]+maize.seas.p.h[,1:12]),as.vector(maize.seas.t.f[,1:12]+maize.seas.p.f[,1:12]),paired=T)
p11 = t.test(as.vector(maize.cov.t.h[1,1:25]+maize.cov.p.h[1,1:25]),as.vector(maize.cov.t.f[1,29:53]+maize.cov.p.f[1,29:53]),paired=F)
p12 = t.test(matrix(maize.id.t.h[,1:25],ncol=1)+matrix(maize.id.p.h[,1:25],ncol=1),
             matrix(maize.id.t.f[,29:53],ncol=1)+matrix(maize.id.p.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/MaizeRevenue_Decomposition_Impacts_TandP.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(maize.trends.t.h[,1]+maize.trends.p.h[,1]),as.vector(maize.trends.t.f[,1]+maize.trends.p.f[,1]),
          as.vector(maize.seas.t.h[,1:12]+maize.seas.p.h[,1:12]),as.vector(maize.seas.t.f[,1:12]+maize.seas.p.f[,1:12]),
          as.vector(maize.cov.t.h[1,1:25]+maize.cov.p.h[1,1:25]),as.vector(maize.cov.p.f[1,]),as.vector(maize.id.p.h[,]),as.vector(maize.id.p.f[,]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F)
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(R) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("Trends","Seasonality","Covariate Shock","Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,1.5,expression(paste(Delta," = 0.0007 (***)")),cex=1.5)
  text(3.5,1.5,expression(paste(Delta," = 0.047 (***)")),cex=1.5)
  text(5.5,1.5,expression(paste(Delta," = 0.015 (NS)")),cex=1.5,col="orange3")
  text(7.5,1.5,expression(paste(Delta," = -0.21 (***)")),cex=1.5)
  #legend("topright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

# Density plots and box plots
png(width=6,height=6,units="in",res=300,file="Output/Figures/Projections/MaizeRevenue_Dists.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-100,-100,ylim=c(0,0.5),xlim=c(3,9),ylab="Density",xlab="log(revenues)")
  lines(density(maize.revenue[!is.na(maize.revenue)]),lwd=2,col="black")
  lines(density(maize.future.revenue[!is.na(maize.future.revenue)]),lwd=2,col="blue",lty=2)
dev.off()  

# Density plots and box plots
hist.order = order(apply(maize.revenue[,1:25],2,median,na.rm=T))
future.order = order(apply(maize.future.revenue[,29:53],2,median,na.rm=T))+28
png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/Maize_Revenues_Year_Dist.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-1000,-1000,xlim=c(1,25),ylim=c(0,12),xlab="",ylab="(log) Maize Revenues",xaxt="n")
  grid(col="darkgrey")
  boxplot(maize.revenue[,hist.order],outline=T,add=T,xaxt="n",whisklty=1,whiskcol="black",lwd=2,outpch="+",outcol="black")
  boxplot(maize.future.revenue[,future.order],col="blue",boxwex=0.4,xaxt="n",add=T,outline=T,whisklty=1,whiskcol="blue",outcol="red")
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

mun.sd.h = apply(maize.revenue[,1:25],1,sd,na.rm=T)
mun.sd.f = apply(maize.future.revenue[,29:53],1,sd,na.rm=T)
png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/Maize_Revenues_Mun_SD.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-100,-100,ylim=c(0,4),xlim=c(0,1),ylab="Density",xlab="sd(log(revenues))")
  lines(density(mun.sd.h[!is.na(mun.sd.h)]),lwd=2,col="black")
  lines(density(mun.sd.f[!is.na(mun.sd.f)]),lwd=2,col="blue",lty=2)
dev.off()  


png(width=6,height=6,units="in",res=300,file="Output/Figures/Projections/Maize_Revenues_Year_CDF.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-1000,-1000,xlim=c(3,10),ylim=c(0,1),xlab="log(Revenue)",ylab="CDF")
  grid(col="darkgrey")
  lines(ecdf(maize.revenue),col="black",lwd=2)
  lines(ecdf(maize.future.revenue),col="blue",lwd=2)
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

# ------------------------------------------------------------------------------------
# (3) Total Ag Revenue
# ------------------------------------------------------------------------------------

# Decomposition (Default)     
total.trends.t.h = ag.absval.coeffs$ltr[2]*int.trend.hist.t[,1]*matrix(1,nrow=nrow(municipalities),ncol=27) 
total.seas.t.h = ag.absval.coeffs$ltr[3]*int.trend.hist.t[,2]*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F) +
  ag.absval.coeffs$ltr[4]*matrix(rep(annual.abs.t.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + ag.absval.coeffs$ltr[12]*matrix(rep((annual.abs.t.seas.hist)^2,times=27),nrow=nrow(municipalities),byrow=F) 
total.cov.t.h = ag.absval.coeffs$ltr[5]*matrix(rep(annual.abs.t.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[13]*matrix(rep((annual.abs.t.shock.cov.hist)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) 
total.id.t.h = ag.absval.coeffs$ltr[6]*annual.abs.t.shock.id.hist + 
  ag.absval.coeffs$ltr[14]*(annual.abs.t.shock.id.hist)^2

total.trends.p.h = ag.absval.coeffs$ltr[8]*int.trend.hist.p[,2]/fac*(1/12)*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F)
total.seas.p.h = ag.absval.coeffs$ltr[9]*matrix(rep(annual.abs.p.seas.hist/fac,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[15]*matrix(rep(annual.abs.p.seas.hist/fac^2,times=27),nrow=nrow(municipalities),byrow=F) 
total.cov.p.h = ag.absval.coeffs$ltr[10]*matrix(rep(annual.abs.p.shock.cov.hist/fac,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[16]*matrix(rep(annual.abs.p.shock.cov.hist/fac^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) 
total.id.p.h = ag.absval.coeffs$ltr[11]*annual.abs.p.shock.id.hist/fac +
  ag.absval.coeffs$ltr[17]*annual.abs.p.shock.id.hist/fac^2

# future
total.trends.t.f = ag.absval.coeffs$ltr[2]*int.trend.future.t[,1]*matrix(1,nrow=nrow(municipalities),ncol=53) 
total.seas.t.f = ag.absval.coeffs$ltr[3]*int.trend.future.t[,2]*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F) +
  ag.absval.coeffs$ltr[4]*matrix(rep(annual.abs.t.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + ag.absval.coeffs$ltr[12]*matrix(rep((annual.abs.t.seas.future)^2,times=53),nrow=nrow(municipalities),byrow=F) 
total.cov.t.f = ag.absval.coeffs$ltr[5]*matrix(rep(annual.abs.t.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[13]*matrix(rep((annual.abs.t.shock.cov.future)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) 
total.id.t.f = ag.absval.coeffs$ltr[6]*annual.abs.t.shock.id.future + 
  ag.absval.coeffs$ltr[14]*(annual.abs.t.shock.id.future)^2

total.trends.p.f = ag.absval.coeffs$ltr[8]*int.trend.future.p[,2]/fac*(1/12)*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F)
total.seas.p.f = ag.absval.coeffs$ltr[9]*matrix(rep(annual.abs.p.seas.future/fac,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[15]*matrix(rep(annual.abs.p.seas.future/fac^2,times=53),nrow=nrow(municipalities),byrow=F) 
total.cov.p.f = ag.absval.coeffs$ltr[10]*matrix(rep(annual.abs.p.shock.cov.future/fac,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[16]*matrix(rep(annual.abs.p.shock.cov.future/fac^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) 
total.id.p.f = ag.absval.coeffs$ltr[11]*annual.abs.p.shock.id.future/fac +
  ag.absval.coeffs$ltr[17]*annual.abs.p.shock.id.future/fac^2


p1 = t.test(as.vector(total.trends.t.h[,1:12]),as.vector(total.trends.t.f[,1:12]),paired=T)
p2 = t.test(as.vector(total.seas.t.h[,1:12]),as.vector(total.seas.t.f[,1:12]),paired=T)
p3 = t.test(as.vector(total.cov.t.h[1,1:25]),as.vector(total.cov.t.f[1,29:53]),paired=F)
p4 = t.test(matrix(total.id.t.h[,1:25],ncol=1),matrix(total.id.t.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/totalRevenue_Decomposition_Impacts_T.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(total.trends.t.h[,1]),as.vector(total.trends.t.f[,1]),as.vector(total.seas.t.h[,1:12]),as.vector(total.seas.t.f[,1:12]),as.vector(total.cov.t.h[1,]),as.vector(total.cov.t.f[1,]),as.vector(total.id.t.h[,]),as.vector(total.id.t.f[,]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F,ylim=c(-1.2,2))
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(Revenue) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("T Trends","T Seasonality","T Covariate Shock","T Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,0.3,expression(paste(Delta," = 0.003 (***)")),cex=1.5)
  text(3.5,0.3,expression(paste(Delta," = 0.034 (***)")),cex=1.5)
  text(5.5,0.3,expression(paste(Delta," = -0.08 (NS)")),cex=1.5,col="orange3")
  text(7.5,0.3,expression(paste(Delta," = 0.24 (***)")),cex=1.5)
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

p5 = t.test(as.vector(total.trends.p.h[,1:12]),as.vector(total.trends.p.f[,1:12]),paired=T)
p6 = t.test(as.vector(total.seas.p.h[,1:12]),as.vector(total.seas.p.f[,1:12]),paired=T)
p7 = t.test(as.vector(total.cov.p.h[1,1:25]),as.vector(total.cov.p.f[1,29:53]),paired=F)
p8 = t.test(matrix(total.id.p.h[,1:25],ncol=1),matrix(total.id.p.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/totalRevenue_Decomposition_Impacts_P.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(total.trends.p.h[,1]),as.vector(total.trends.p.f[,1]),as.vector(total.seas.p.h[,1:12]),as.vector(total.seas.p.f[,1:12]),as.vector(total.cov.p.h[1,1:25]),as.vector(total.cov.p.f[1,29:53]),as.vector(total.id.p.h[,1:25]),as.vector(total.id.p.f[,29:53]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F,ylim=c(-0.5,3))
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(Revenue) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("P Trends","P Seasonality","P Covariate Shock","P Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,-0.5,expression(paste(Delta," = -2.9e-6 (***)")),cex=1.5)
  text(3.5,-0.5,expression(paste(Delta," = 6e-4 (***)")),cex=1.5)
  text(5.5,-0.5,expression(paste(Delta," = 0.041 (NS)")),cex=1.5,col="orange3")
  text(7.5,-0.5,expression(paste(Delta," = -0.23 (***)")),cex=1.5)
  #legend("topright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

p9 = t.test(as.vector(total.trends.t.h[,1:12]+total.trends.p.h[,1:12]),as.vector(total.trends.t.f[,1:12]+total.trends.p.f[,1:12]),paired=T)
p10 = t.test(as.vector(total.seas.t.h[,1:12]+total.seas.p.h[,1:12]),as.vector(total.seas.t.f[,1:12]+total.seas.p.f[,1:12]),paired=T)
p11 = t.test(as.vector(total.cov.t.h[1,1:25]+total.cov.p.h[1,1:25]),as.vector(total.cov.t.f[1,29:53]+total.cov.p.f[1,29:53]),paired=F)
p12 = t.test(matrix(total.id.t.h[,1:25],ncol=1)+matrix(total.id.p.h[,1:25],ncol=1),
             matrix(total.id.t.f[,29:53],ncol=1)+matrix(total.id.p.f[,29:53],ncol=1),paired=F)

png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/totalRevenue_Decomposition_Impacts_TandP.png")
  par(mar=c(5,6,1,1),las=1)
  boxplot(as.vector(total.trends.t.h[,1]+total.trends.p.h[,1]),as.vector(total.trends.t.f[,1]+total.trends.p.f[,1]),
          as.vector(total.seas.t.h[,1:12]+total.seas.p.h[,1:12]),as.vector(total.seas.t.f[,1:12]+total.seas.p.f[,1:12]),
          as.vector(total.cov.t.h[1,1:25]+total.cov.p.h[1,1:25]),as.vector(total.cov.p.f[1,]),as.vector(total.id.p.h[,]),as.vector(total.id.p.f[,]),col=c("white","blue"),cex.axis=1.5,cex.lab=2,xaxt="n",ylab="",outline=F)
  mtext(side=2,las=0,line=4,cex=2,text="Impact on log(R) from Baseline")
  abline(h=0,col="red",lty=2,lwd=2)
  axis(side=1,at=c(1.5,3.5,5.5,7.5),lab=c("Trends","Seasonality","Covariate Shock","Idiosyncratic Shock"),cex.axis=1.5)
  text(1.5,0.5,expression(paste(Delta," = 0.003 (***)")),cex=1.5)
  text(3.5,0.5,expression(paste(Delta," = 0.035 (***)")),cex=1.5)
  text(5.5,0.5,expression(paste(Delta," = -0.040 (NS)")),cex=1.5,col="orange3") # looks funny but true - mean is less.
  text(7.5,0.5,expression(paste(Delta," = 0.27 (***)")),cex=1.5)
  #legend("topright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

# Density plots and box plots
png(width=6,height=6,units="in",res=300,file="Output/Figures/Projections/totalRevenue_Dists.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-100,-100,ylim=c(0,0.5),xlim=c(3,13),ylab="Density",xlab="log(revenues)")
  lines(density(total.revenue[!is.na(total.revenue)]),lwd=2,col="black")
  lines(density(total.future.revenue[!is.na(total.future.revenue)]),lwd=2,col="blue",lty=2)
dev.off()  

# Density plots and box plots
hist.order = order(apply(total.revenue[,1:25],2,median,na.rm=T))
future.order = order(apply(total.future.revenue[,29:53],2,median,na.rm=T))+28
png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/total_Revenues_Year_Dist.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-1000,-1000,xlim=c(1,25),ylim=c(2,15),xlab="",ylab="(log) total Revenues",xaxt="n")
  grid(col="darkgrey")
  boxplot(total.revenue[,hist.order],outline=T,add=T,xaxt="n",whisklty=1,whiskcol="black",lwd=2,outpch="+",outcol="black")
  boxplot(total.future.revenue[,future.order],col="blue",boxwex=0.4,xaxt="n",add=T,outline=T,whisklty=1,whiskcol="blue",outcol="red")
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  

mun.sd.h = apply(total.revenue[,1:25],1,sd,na.rm=T)
mun.sd.f = apply(total.future.revenue[,29:53],1,sd,na.rm=T)
png(width=12,height=8,units="in",res=300,file="Output/Figures/Projections/total_Revenues_Mun_SD.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-100,-100,ylim=c(0,2),xlim=c(0,4),ylab="Density",xlab="sd(log(revenues))")
  lines(density(mun.sd.h[!is.na(mun.sd.h)]),lwd=2,col="black")
  lines(density(mun.sd.f[!is.na(mun.sd.f)]),lwd=2,col="blue",lty=2)
dev.off()  


png(width=6,height=6,units="in",res=300,file="Output/Figures/Projections/total_Revenues_Year_CDF.png")
  par(mar=c(5,6,1,1),las=1)
  plot(-1000,-1000,xlim=c(3,10),ylim=c(0,1),xlab="log(Revenue)",ylab="CDF")
  grid(col="darkgrey")
  lines(ecdf(total.revenue),col="black",lwd=2)
  lines(ecdf(total.future.revenue),col="blue",lwd=2)
  #legend("bottomright",inset=0.01,bty="n",fill=c("white","blue"),legend=c("Past","Future"),cex=1.5)
dev.off()  
