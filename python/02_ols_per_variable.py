import pandas as pd, numpy as np, json
import statsmodels.formula.api as smf, statsmodels.api as sm
df=pd.read_pickle("df6.pkl"); df["Country"]=np.where(df.Is_Br==1,"US","Brazil")
sc={"RD":[f"RD{i}" for i in range(1,6)],"Misg":[f"Misg{i}" for i in range(1,11)],"SPV":[f"SPV{i}" for i in range(1,5)],"WEPV":["WEPV"],"Chaos":[f"Chaos_{i}" for i in range(1,8)],"SDO":["SDO_1","SDO2","SDO3","SDOD4"],"SDOE":["SDO_1","SDO2"],"SDOD":["SDO3","SDOD4"],"AT":[f"AT{i}" for i in range(1,5)],"Racism":[f"Racism_{i}" for i in range(3,7)],"Ppl":["PPl2","Ppl3_","Ppl4","Ppl5"],"Trust":["Trust1","Trust2","Trust3"],"PSE":["PSE1","PSE2"],"Belief":[f"Belief{i}" for i in range(1,5)],"Share":[f"Share{i}" for i in range(1,5)]}
lab=dict(RD="Relative deprivation",Misg="Misogynistic attitudes",SPV="Support for partisan violence",WEPV="Willingness to engage in political violence",Chaos="Need for chaos",SDO="Social dominance orientation",SDOE="  SDO-Egalitarianism items (2)",SDOD="  SDO-Dominance items (2)",AT="Authoritarian attitudes",Racism="Symbolic racism",Ppl="Populist attitudes",Trust="Political distrust",PSE="Political self-efficacy",Belief="Belief in hostile posts",Share="Willingness to share hostile posts")
def alpha(x):
    x=x.dropna(); k=x.shape[1]; return k/(k-1)*(1-x.var().sum()/x.sum(axis=1).var())
order=["RD","Misg","SPV","WEPV","Chaos","SDO","SDOE","SDOD","AT","Racism","Ppl","Trust","PSE","Belief","Share"]
res=[];desc=[]
for k in order:
    v=sc[k]
    desc.append(dict(key=k,var=lab[k],items=len(v),alpha=alpha(df[v]) if len(v)>1 else None,alphaBR=alpha(df.loc[df.Country=="Brazil",v]) if len(v)>1 else None,alphaUS=alpha(df.loc[df.Country=="US",v]) if len(v)>1 else None,M=df[k].mean(),SD=df[k].std(),MBR=df.loc[df.Country=="Brazil",k].mean(),MUS=df.loc[df.Country=="US",k].mean()))
    for name,d in [("All",df),("Brazil",df[df.Country=="Brazil"]),("US",df[df.Country=="US"])]:
        d=d.dropna(subset=[k,"Pol_Tend"]); m=smf.ols(f"{k} ~ Pol_Tend",d).fit()
        res.append(dict(key=k,var=lab[k],sample=name,n=int(m.nobs),b=m.params["Pol_Tend"],se=m.bse["Pol_Tend"],beta=m.params["Pol_Tend"]*d.Pol_Tend.std()/d[k].std(),p=m.pvalues["Pol_Tend"],r2=m.rsquared))
    d=df.dropna(subset=[k,"Pol_Tend"]); mi=smf.ols(f"{k} ~ Pol_Tend*C(Country)",d).fit()
    nm=[x for x in mi.params.index if x.startswith("Pol_Tend:")][0]
    res.append(dict(key=k,var=lab[k],sample="Interaction",n=int(mi.nobs),b=mi.params[nm],se=mi.bse[nm],beta=None,p=mi.pvalues[nm],r2=None))
# Multiple regression: Pol_Tend ~ all main scales (z-scored), by sample
preds=["RD","Misg","SPV","WEPV","Chaos","SDO","AT","Racism","Ppl","Trust","PSE","Belief","Share"]
mr=[]
for name,d in [("All",df),("Brazil",df[df.Country=="Brazil"]),("US",df[df.Country=="US"])]:
    d=d.dropna(subset=preds+["Pol_Tend"]).copy(); z=(d[preds+["Pol_Tend"]]-d[preds+["Pol_Tend"]].mean())/d[preds+["Pol_Tend"]].std()
    m=sm.OLS(z["Pol_Tend"],sm.add_constant(z[preds])).fit()
    vif=[1/(1-sm.OLS(z[p],sm.add_constant(z[[q for q in preds if q!=p]])).fit().rsquared) for p in preds]
    for p,vf in zip(preds,vif): mr.append(dict(sample=name,key=p,var=lab[p],beta=m.params[p],p=m.pvalues[p],vif=vf,r2=m.rsquared,n=int(m.nobs)))
json.dump(dict(res=res,desc=desc,mr=mr),open("reg7.json","w"),default=float)
pd.set_option("display.width",220)
r=pd.DataFrame(res); print(r.round(3).to_string())
print(pd.DataFrame(desc).round(2).to_string())
print(pd.DataFrame(mr).pivot(index="key",columns="sample",values="beta").round(2)); print(pd.DataFrame(mr).groupby("sample")[["r2","n"]].first())
print(pd.DataFrame(mr).pivot(index="key",columns="sample",values="p").round(3)); print(pd.DataFrame(mr).vif.max())
