# ------------------------------------------------------------------------------------
# Projections_SimpleRep.R
# ------------------------------------------------------------------------------------
# Merges coefficients and bias corrected and downscaled climate data
# No FI data due to data sharing restrictions

# ------------------------------------------------------------------------------------
# 1. Load ag data, get coefficients - maize, soybeans, total; yields, revenue, price
# ------------------------------------------------------------------------------------
load(file="Output/Ag_Climate_Analysis.RData")

ag.coeffs = data.frame(lmy=mods_nat[[38]]$coefficients,lmr=mods_nat[[40]]$coefficients,lmp=mods_nat[[42]]$coefficients,
                       lsy=mods_nat[[74]]$coefficients,lsr=mods_nat[[76]]$coefficients,lsp=mods_nat[[78]]$coefficients,
                       lty=mods_nat[[86]]$coefficients,ltr=mods_nat[[88]]$coefficients,ltp=mods_nat[[90]]$coefficients)

ag.absval.coeffs = data.frame(lmy=mods_nat_absval[[38]]$coefficients,lmr=mods_nat_absval[[40]]$coefficients,lmp=mods_nat_absval[[42]]$coefficients,
                              lsy=mods_nat_absval[[74]]$coefficients,lsr=mods_nat_absval[[76]]$coefficients,lsp=mods_nat_absval[[78]]$coefficients,
                              lty=mods_nat_absval[[86]]$coefficients,ltr=mods_nat_absval[[88]]$coefficients,ltp=mods_nat_absval[[90]]$coefficients)

# ------------------------------------------------------------------------------------
# 2. No FI data.
# ------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------
# 3. Load model data, set up values (aggregated accordingly)
# ------------------------------------------------------------------------------------
load(file="HADGEM RCP8p5 Municipality T and P 1991-2070.Rdata")

time.hist = rep(1991:2017,each=12) + rep(0:11/12,times=27)
time.future = rep(2018:2070,each=12) + rep(0:11/12,times=53)

# project everything out first
# -----------------------------------------
ba_ind = 200:615

# historical - aggregate to year
annual.t.seas.hist = apply((seas.hist.t),1,sum,na.rm=T)
annual.p.seas.hist = apply((seas.hist.p),1,sum,na.rm=T)
annual.abs.t.seas.hist = apply(abs(seas.hist.t),1,sum,na.rm=T)
annual.abs.p.seas.hist = apply(abs(seas.hist.p),1,sum,na.rm=T)

annual.t.shock.cov.hist = colSums(matrix((t.shock.cov.hist),nrow=12,ncol=length(t.shock.cov.hist)/12))
annual.p.shock.cov.hist = colSums(matrix((p.shock.cov.hist),nrow=12,ncol=length(p.shock.cov.hist)/12))
annual.abs.t.shock.cov.hist = colSums(matrix(abs(t.shock.cov.hist),nrow=12,ncol=length(t.shock.cov.hist)/12))
annual.abs.p.shock.cov.hist = colSums(matrix(abs(p.shock.cov.hist),nrow=12,ncol=length(p.shock.cov.hist)/12))

annual.t.shock.id.hist = t(apply(array((t.shock.id.hist),dim=c(nrow(t.shock.id.hist),12,ncol(t.shock.id.hist)/12)),1,colSums))
annual.p.shock.id.hist = t(apply(array((p.shock.id.hist),dim=c(nrow(p.shock.id.hist),12,ncol(p.shock.id.hist)/12)),1,colSums))
annual.abs.t.shock.id.hist = t(apply(array(abs(t.shock.id.hist),dim=c(nrow(t.shock.id.hist),12,ncol(t.shock.id.hist)/12)),1,colSums))
annual.abs.p.shock.id.hist = t(apply(array(abs(p.shock.id.hist),dim=c(nrow(p.shock.id.hist),12,ncol(p.shock.id.hist)/12)),1,colSums))

# future - aggregate to year
annual.t.seas.future = apply((seas.future.t),1,sum,na.rm=T)
annual.p.seas.future = apply((seas.future.p),1,sum,na.rm=T)
annual.abs.t.seas.future = apply(abs(seas.future.t),1,sum,na.rm=T)
annual.abs.p.seas.future = apply(abs(seas.future.p),1,sum,na.rm=T)

annual.t.shock.cov.future = colSums(matrix((t.shock.cov.future),nrow=12,ncol=length(t.shock.cov.future)/12))
annual.p.shock.cov.future = colSums(matrix((p.shock.cov.future),nrow=12,ncol=length(p.shock.cov.future)/12))
annual.abs.t.shock.cov.future = colSums(matrix(abs(t.shock.cov.future),nrow=12,ncol=length(t.shock.cov.future)/12))
annual.abs.p.shock.cov.future = colSums(matrix(abs(p.shock.cov.future),nrow=12,ncol=length(p.shock.cov.future)/12))

annual.t.shock.id.future = t(apply(array((t.shock.id.future),dim=c(nrow(t.shock.id.future),12,ncol(t.shock.id.future)/12)),1,colSums))
annual.p.shock.id.future = t(apply(array((p.shock.id.future),dim=c(nrow(p.shock.id.future),12,ncol(p.shock.id.future)/12)),1,colSums))
annual.abs.t.shock.id.future = t(apply(array(abs(t.shock.id.future),dim=c(nrow(t.shock.id.future),12,ncol(t.shock.id.future)/12)),1,colSums))
annual.abs.p.shock.id.future = t(apply(array(abs(p.shock.id.future),dim=c(nrow(p.shock.id.future),12,ncol(p.shock.id.future)/12)),1,colSums))

# only do bahia
ba.t.shock.cov.hist = apply(t.shock.hist[ba_ind,],2,mean,na.rm=T)
ba.p.shock.cov.hist = apply(p.shock.hist[ba_ind,],2,mean,na.rm=T)
ba.t.shock.id.hist = t.shock.hist[ba_ind,] - ba.t.shock.cov.hist
ba.p.shock.id.hist = p.shock.hist[ba_ind,] - ba.p.shock.cov.hist

ba.abs.t.shock.cov.hist = abs(ba.t.shock.cov.hist)
ba.abs.p.shock.cov.hist = abs(ba.p.shock.cov.hist)
ba.abs.t.shock.id.hist = abs(ba.t.shock.id.hist)
ba.abs.p.shock.id.hist = abs(ba.p.shock.id.hist)

ba.t.shock.cov.future = apply(t.shock.future[ba_ind,],2,mean,na.rm=T)
ba.p.shock.cov.future = apply(p.shock.future[ba_ind,],2,mean,na.rm=T)
ba.t.shock.id.future = t.shock.future[ba_ind,] - ba.t.shock.cov.future
ba.p.shock.id.future = p.shock.future[ba_ind,] - ba.p.shock.cov.future

ba.abs.t.shock.cov.future = abs(ba.t.shock.cov.future)
ba.abs.p.shock.cov.future = abs(ba.p.shock.cov.future)
ba.abs.t.shock.id.future = abs(ba.t.shock.id.future)
ba.abs.p.shock.id.future = abs(ba.p.shock.id.future)

# historical - aggregate to maize season
fac = 10
seas_ind = 1:8
bb = length(seas_ind)
#bb = fac

maize.t.seas.hist = apply((seas.hist.t[,seas_ind]),1,sum,na.rm=T)/bb
maize.p.seas.hist = apply((seas.hist.p[,seas_ind]),1,sum,na.rm=T)/bb/fac
maize.abs.t.seas.hist = apply(abs(seas.hist.t[,seas_ind]),1,sum,na.rm=T)/bb
maize.abs.p.seas.hist = apply(abs(seas.hist.p[,seas_ind]),1,sum,na.rm=T)/bb/fac

maize.t.shock.cov.hist = colSums(matrix((t.shock.cov.hist),nrow=12,ncol=length(t.shock.cov.hist)/12)[seas_ind,])/bb
maize.p.shock.cov.hist = colSums(matrix((p.shock.cov.hist),nrow=12,ncol=length(p.shock.cov.hist)/12)[seas_ind,])/bb/fac
maize.abs.t.shock.cov.hist = colSums(matrix(abs(t.shock.cov.hist),nrow=12,ncol=length(t.shock.cov.hist)/12)[seas_ind,])/bb
maize.abs.p.shock.cov.hist = colSums(matrix(abs(p.shock.cov.hist),nrow=12,ncol=length(p.shock.cov.hist)/12)[seas_ind,])/bb/fac

maize.t.shock.id.hist = t(apply(array((t.shock.id.hist),dim=c(nrow(t.shock.id.hist),12,ncol(t.shock.id.hist)/12))[,seas_ind,],1,colSums))/bb
maize.p.shock.id.hist = t(apply(array((p.shock.id.hist),dim=c(nrow(p.shock.id.hist),12,ncol(p.shock.id.hist)/12))[,seas_ind,],1,colSums))/bb/fac
maize.abs.t.shock.id.hist = t(apply(array(abs(t.shock.id.hist),dim=c(nrow(t.shock.id.hist),12,ncol(t.shock.id.hist)/12))[,seas_ind,],1,colSums))/bb
maize.abs.p.shock.id.hist = t(apply(array(abs(p.shock.id.hist),dim=c(nrow(p.shock.id.hist),12,ncol(p.shock.id.hist)/12))[,seas_ind,],1,colSums))/bb/fac

# future - aggregate to maize season

maize.t.seas.future = apply((seas.future.t[,seas_ind]),1,sum,na.rm=T)/bb
maize.p.seas.future = apply((seas.future.p[,seas_ind]),1,sum,na.rm=T)/bb/fac
maize.abs.t.seas.future = apply(abs(seas.future.t[,seas_ind]),1,sum,na.rm=T)/bb
maize.abs.p.seas.future = apply(abs(seas.future.p[,seas_ind]),1,sum,na.rm=T)/bb/fac

maize.t.shock.cov.future = colSums(matrix((t.shock.cov.future),nrow=12,ncol=length(t.shock.cov.future)/12)[seas_ind,])/bb
maize.p.shock.cov.future = colSums(matrix((p.shock.cov.future),nrow=12,ncol=length(p.shock.cov.future)/12)[seas_ind,])/bb/fac
maize.abs.t.shock.cov.future = colSums(matrix(abs(t.shock.cov.future),nrow=12,ncol=length(t.shock.cov.future)/12)[seas_ind,])/bb
maize.abs.p.shock.cov.future = colSums(matrix(abs(p.shock.cov.future),nrow=12,ncol=length(p.shock.cov.future)/12)[seas_ind,])/bb/fac

maize.t.shock.id.future = t(apply(array((t.shock.id.future),dim=c(nrow(t.shock.id.future),12,ncol(t.shock.id.future)/12))[,seas_ind,],1,colSums))/bb
maize.p.shock.id.future = t(apply(array((p.shock.id.future),dim=c(nrow(p.shock.id.future),12,ncol(p.shock.id.future)/12))[,seas_ind,],1,colSums))/bb/fac
maize.abs.t.shock.id.future = t(apply(array(abs(t.shock.id.future),dim=c(nrow(t.shock.id.future),12,ncol(t.shock.id.future)/12))[,seas_ind,],1,colSums))/bb
maize.abs.p.shock.id.future = t(apply(array(abs(p.shock.id.future),dim=c(nrow(p.shock.id.future),12,ncol(p.shock.id.future)/12))[,seas_ind,],1,colSums))/bb/fac

# ------------------------------------------------------------------------------------
# 4. Historical 'projections'
# ------------------------------------------------------------------------------------

maize.yields.t =  ag.coeffs$lmy[2]*int.trend.hist.t[,1]*matrix(1,nrow=nrow(municipalities),ncol=27) +
  ag.coeffs$lmy[3]*int.trend.hist.t[,2]*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F) +
  ag.coeffs$lmy[4]*matrix(rep(maize.t.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[12]*matrix(rep((maize.t.seas.hist)^2,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[5]*matrix(rep(maize.t.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[13]*matrix(rep((maize.t.shock.cov.hist)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[6]*maize.t.shock.id.hist + 
  ag.coeffs$lmy[14]*(maize.t.shock.id.hist)^2

maize.yields.p = ag.coeffs$lmy[7]*int.trend.hist.p[,1]/fac*matrix(1,nrow=nrow(municipalities),ncol=27) +
  ag.coeffs$lmy[8]*int.trend.hist.p[,2]/fac*(1/12)*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F) +
  ag.coeffs$lmy[9]*matrix(rep(maize.p.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[15]*matrix(rep(maize.p.seas.hist^2,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[10]*matrix(rep(maize.p.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[16]*matrix(rep(maize.p.shock.cov.hist^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[11]*maize.p.shock.id.hist +
  ag.coeffs$lmy[17]*maize.p.shock.id.hist^2

maize.yields = ag.coeffs$lmy[1] + maize.yields.t + maize.yields.p

# (b) Revenues

maize.revenue.t = ag.coeffs$lmr[2]*int.trend.hist.t[,1]*matrix(1,nrow=nrow(municipalities),ncol=27) +
  ag.coeffs$lmr[3]*int.trend.hist.t[,2]*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F) +
  ag.coeffs$lmr[4]*matrix(rep(maize.t.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[12]*matrix(rep((maize.t.seas.hist)^2,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[5]*matrix(rep(maize.t.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[13]*matrix(rep((maize.t.shock.cov.hist)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[6]*maize.t.shock.id.hist + 
  ag.coeffs$lmr[14]*(maize.t.shock.id.hist)^2

maize.revenue.p = ag.coeffs$lmr[7]*int.trend.hist.p[,1]/fac*matrix(1,nrow=nrow(municipalities),ncol=27) +
  ag.coeffs$lmr[8]*int.trend.hist.p[,2]/fac*(1/12)*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F) +
  ag.coeffs$lmr[9]*matrix(rep(maize.p.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[15]*matrix(rep(maize.p.seas.hist^2,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[10]*matrix(rep(maize.p.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[16]*matrix(rep(maize.p.shock.cov.hist^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[11]*maize.p.shock.id.hist +
  ag.coeffs$lmr[17]*maize.p.shock.id.hist^2

maize.revenue = ag.coeffs$lmr[1] + maize.revenue.t + maize.revenue.p


total.revenue.t = ag.absval.coeffs$ltr[2]*int.trend.hist.t[,1]*matrix(1,nrow=nrow(municipalities),ncol=27) +
  ag.absval.coeffs$ltr[3]*int.trend.hist.t[,2]*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F) +
  ag.absval.coeffs$ltr[4]*matrix(rep(annual.abs.t.seas.hist,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[12]*matrix(rep((annual.abs.t.seas.hist)^2,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[5]*matrix(rep(annual.abs.t.shock.cov.hist,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[13]*matrix(rep((annual.abs.t.shock.cov.hist)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[6]*annual.abs.t.shock.id.hist + 
  ag.absval.coeffs$ltr[14]*(annual.abs.t.shock.id.hist)^2

# note - need fac here in seas+shocks
total.revenue.p = ag.absval.coeffs$ltr[7]*int.trend.hist.p[,1]/fac*matrix(1,nrow=nrow(municipalities),ncol=27) +
  ag.absval.coeffs$ltr[8]*int.trend.hist.p[,2]/fac*(1/12)*matrix(rep(1,times=27,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.hist)/12,byrow=F) +
  ag.absval.coeffs$ltr[9]*matrix(rep(annual.abs.p.seas.hist/fac,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[15]*matrix(rep(annual.abs.p.seas.hist/fac^2,times=27),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[10]*matrix(rep(annual.abs.p.shock.cov.hist/fac,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[16]*matrix(rep(annual.abs.p.shock.cov.hist/fac^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[11]*annual.abs.p.shock.id.hist/fac +
  ag.absval.coeffs$ltr[17]*annual.abs.p.shock.id.hist/fac^2

total.revenue = ag.absval.coeffs$ltr[1] + total.revenue.t + total.revenue.p

# ------------------------------------------------------------------------------------
# 5. Future 'projections'
# ------------------------------------------------------------------------------------
maize.future.yields.t =  ag.coeffs$lmy[2]*int.trend.future.t[,1]*matrix(1,nrow=nrow(municipalities),ncol=53) +
  ag.coeffs$lmy[3]*int.trend.future.t[,2]*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F) +
  ag.coeffs$lmy[4]*matrix(rep(maize.t.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[12]*matrix(rep((maize.t.seas.future)^2,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[5]*matrix(rep(maize.t.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[13]*matrix(rep((maize.t.shock.cov.future)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[6]*maize.t.shock.id.future + 
  ag.coeffs$lmy[14]*(maize.t.shock.id.future)^2

maize.future.yields.p = ag.coeffs$lmy[7]*int.trend.future.p[,1]/fac*matrix(1,nrow=nrow(municipalities),ncol=53) +
  ag.coeffs$lmy[8]*int.trend.future.p[,2]/fac*(1/12)*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F) +
  ag.coeffs$lmy[9]*matrix(rep(maize.p.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[15]*matrix(rep(maize.p.seas.future^2,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmy[10]*matrix(rep(maize.p.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[16]*matrix(rep(maize.p.shock.cov.future^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmy[11]*maize.p.shock.id.future +
  ag.coeffs$lmy[17]*maize.p.shock.id.future^2

maize.future.yields = ag.coeffs$lmy[1] + maize.future.yields.t + maize.future.yields.p

# (b) Revenues

maize.future.revenue.t = ag.coeffs$lmr[2]*int.trend.future.t[,1]*matrix(1,nrow=nrow(municipalities),ncol=53) +
  ag.coeffs$lmr[3]*int.trend.future.t[,2]*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F) +
  ag.coeffs$lmr[4]*matrix(rep(maize.t.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[12]*matrix(rep((maize.t.seas.future)^2,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[5]*matrix(rep(maize.t.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[13]*matrix(rep((maize.t.shock.cov.future)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[6]*maize.t.shock.id.future + 
  ag.coeffs$lmr[14]*(maize.t.shock.id.future)^2

maize.future.revenue.p = ag.coeffs$lmr[7]*int.trend.future.p[,1]/fac*matrix(1,nrow=nrow(municipalities),ncol=53) +
  ag.coeffs$lmr[8]*int.trend.future.p[,2]/fac*(1/12)*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F) +
  ag.coeffs$lmr[9]*matrix(rep(maize.p.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[15]*matrix(rep(maize.p.seas.future^2,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.coeffs$lmr[10]*matrix(rep(maize.p.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[16]*matrix(rep(maize.p.shock.cov.future^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.coeffs$lmr[11]*maize.p.shock.id.future +
  ag.coeffs$lmr[17]*maize.p.shock.id.future^2

maize.future.revenue = ag.coeffs$lmr[1] + maize.future.revenue.t + maize.future.revenue.p


total.future.revenue.t = ag.absval.coeffs$ltr[2]*int.trend.future.t[,1]*matrix(1,nrow=nrow(municipalities),ncol=53) +
  ag.absval.coeffs$ltr[3]*int.trend.future.t[,2]*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F) +
  ag.absval.coeffs$ltr[4]*matrix(rep(annual.abs.t.seas.future,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[12]*matrix(rep((annual.abs.t.seas.future)^2,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[5]*matrix(rep(annual.abs.t.shock.cov.future,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[13]*matrix(rep((annual.abs.t.shock.cov.future)^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[6]*annual.abs.t.shock.id.future + 
  ag.absval.coeffs$ltr[14]*(annual.abs.t.shock.id.future)^2

# note - need fac here in seas+shocks
total.future.revenue.p = ag.absval.coeffs$ltr[7]*int.trend.future.p[,1]/fac*matrix(1,nrow=nrow(municipalities),ncol=53) +
  ag.absval.coeffs$ltr[8]*int.trend.future.p[,2]/fac*(1/12)*matrix(rep(1,times=53,each=nrow(municipalities)),nrow=nrow(municipalities),ncol=length(time.future)/12,byrow=F) +
  ag.absval.coeffs$ltr[9]*matrix(rep(annual.abs.p.seas.future/fac,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[15]*matrix(rep(annual.abs.p.seas.future/fac^2,times=53),nrow=nrow(municipalities),byrow=F) + 
  ag.absval.coeffs$ltr[10]*matrix(rep(annual.abs.p.shock.cov.future/fac,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[16]*matrix(rep(annual.abs.p.shock.cov.future/fac^2,times=nrow(municipalities)),nrow=nrow(municipalities),byrow=T) +
  ag.absval.coeffs$ltr[11]*annual.abs.p.shock.id.future/fac +
  ag.absval.coeffs$ltr[17]*annual.abs.p.shock.id.future/fac^2

total.future.revenue = ag.absval.coeffs$ltr[1] + total.future.revenue.t + total.future.revenue.p

