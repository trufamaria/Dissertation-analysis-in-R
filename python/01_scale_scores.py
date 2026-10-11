import pyreadstat, pandas as pd, numpy as np, json
import statsmodels.formula.api as smf
df,m=pyreadstat.read_sav("../data/Merged_BR_US_Updated.sav")
df=df[(df.Age<150)&df.Gender.isin([1,2])].copy()
print([ (c,m.column_names_to_labels.get(c)) for c in ["SDO_1","SDO2","SDO3","SDOD4","Belief1","Share1"]])
sc={"RD":[f"RD{i}" for i in range(1,6)],"Misg":[f"Misg{i}" for i in range(1,11)],"SPV":[f"SPV{i}" for i in range(1,5)],
"WEPV":["WEPV"],"Chaos":[f"Chaos_{i}" for i in range(1,8)],"SDO":["SDO_1","SDO2","SDO3","SDOD4"],"SDOE":["SDO_1","SDO2"],"SDOD":["SDO3","SDOD4"],
"AT":[f"AT{i}" for i in range(1,5)],"Racism":[f"Racism_{i}" for i in range(3,7)],"Ppl":["PPl2","Ppl3_","Ppl4","Ppl5"],
"Trust":["Trust1","Trust2","Trust3"],"PSE":["PSE1","PSE2"],"Belief":[f"Belief{i}" for i in range(1,5)],"Share":[f"Share{i}" for i in range(1,5)]}
def alpha(x):
    x=x.dropna(); k=x.shape[1]; return k/(k-1)*(1-x.var().sum()/x.sum(axis=1).var())
for k,v in sc.items(): df[k]=df[v].mean(axis=1)
for k,i in [("Racism","SRac_i"),("Ppl","PPL_i"),("SDO","SDO_i"),("Belief","Blf_i"),("Share","Share_i"),("SPV","SPV_i"),("Misg","Misg_i"),("Chaos","Chaos_i"),("RD","RD_i"),("AT","AT_i"),("PSE","PSE_i")]:
    if i in df: print(k,i,"corr=%.4f maxdiff=%.3f"%(df[k].corr(df[i]),(df[k]-df[i]).abs().max()))
print({k:df[k].notna().sum() for k in sc})
for k in ["SDO","Racism","Ppl","Belief","Share"]:
    print(k,"alpha %.2f"%alpha(df[sc[k]]))
print(df[["Belief","Share","SDO","Chaos"]].corr().round(2))
df.to_pickle("df6.pkl")
