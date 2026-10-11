import pyreadstat, pandas as pd, re
df,_=pyreadstat.read_sav("../data/Merged_BR_US_Updated.sav")
df["q"]=df.qual.fillna("").str.strip()
df=df[df.q!=""]
junk=re.compile(r"^(n/?a|none|no|nope|nothing|nao|não|nada|ok|okay|\.|-|no comments?|sem comentários?|nenhum)\W*$",re.I)
df["substantive"]=~df.q.map(lambda s: bool(junk.match(s)) or len(s)<6)
df["grp"]=df.Pol_Tend.map({1:"Left",2:"Left",3:"Center",4:"Right",5:"Right"})
df["C"]=df.Is_Br.map({0:"BR",1:"US"})
print(df.groupby(["grp","C"]).agg(n=("q","size"),subst=("substantive","sum")))
df.to_pickle("q.pkl")
for g in ["Right","Left","Center"]:
    for c in ["US","BR"]:
        s=df[(df.grp==g)&(df.C==c)&df.substantive]
        open(f"q_{g}_{c}.txt","w").write("\n".join(f"[{int(p)}] {t}" for p,t in zip(s.Pol_Tend,s.q)))
        print(g,c,len(s), s.q.str.len().mean().round())
