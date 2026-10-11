import pandas as pd, numpy as np, json
import statsmodels.formula.api as smf
df=pd.read_pickle("df6.pkl"); df["Country"]=np.where(df.Is_Br==1,"US","Brazil")
for i in range(1,5): df[f"S{i}"]=df[f"Share{i}"]; df[f"B{i}"]=df[f"Belief{i}"]
out=[]
def fit(y,f,d,name,lab):
    d=d.dropna(subset=[y,"Pol_Tend"]); m=smf.ols(f,d).fit()
    r=dict(lab=lab,sample=name,n=int(m.nobs),b=m.params["Pol_Tend"],se=m.bse["Pol_Tend"],beta=m.params["Pol_Tend"]*d.Pol_Tend.std()/d[y].std(),p=m.pvalues["Pol_Tend"],r2=m.rsquared)
    if "Belief" in f: r.update(bb=m.params["Belief"],bp=m.pvalues["Belief"],bbeta=m.params["Belief"]*d.Belief.std()/d[y].std())
    return r
items={"Share":"Share index (4 items)","S1":"Post 1: gender identity","S2":"Post 2: science denial","S3":"Post 3: climate (natural causes)","S4":"Post 4: ANTIFA / political violence"}
for y,l in items.items():
    for n,d in [("All",df),("Brazil",df[df.Country=="Brazil"]),("US",df[df.Country=="US"])]:
        out.append(dict(kind="simple",**fit(y,f"{y} ~ Pol_Tend",d,n,l)))
    d=df.dropna(subset=[y,"Pol_Tend"]); mi=smf.ols(f"{y} ~ Pol_Tend*C(Country)",d).fit(); k=[x for x in mi.params.index if x.startswith("Pol_Tend:")][0]
    out.append(dict(kind="simple",lab=l,sample="Interaction",n=int(mi.nobs),b=mi.params[k],se=mi.bse[k],p=mi.pvalues[k]))
for n,d in [("All",df),("Brazil",df[df.Country=="Brazil"]),("US",df[df.Country=="US"])]:
    out.append(dict(kind="controlBelief",**fit("Share","Share ~ Pol_Tend + Belief",d,n,"Share, controlling belief")))
pd.set_option("display.width",220); r=pd.DataFrame(out); print(r.round(3).to_string())
json.dump(out,open("share.json","w"),default=float)
for n,d in [("All",df),("Brazil",df[df.Country=="Brazil"]),("US",df[df.Country=="US"])]: print(n,d.Share.corr(d.Belief).round(2), d.Share.mean().round(2), d.Share.std().round(2))
