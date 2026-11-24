"""Rebuild the README figures in docs/img/ and the model-vs-draft-slot tables.

Run from the repo root after 11_temporal_cv.R, 11b_temporal_cv_qb_te.R,
export_model_performance.R and 23_draft_slot_baseline.R:

    python3 docs/make_readme_figures.py

Deployed predictions: WR/RB from website/public/data/model_performance.json
(hurdle + outcome-bucket ensemble, walk-forward OOS); QB/TE from
output/temporal_cv_qb_te/oos_predictions.csv blended with per-fold comps
(QB 0.50/0.50, TE 0.30/0.70 model/comp) exactly as the site does.
"""
import json, sys, numpy as np, pandas as pd, matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
S="output/draft_slot_baseline"; OUT="docs/img"; ROOT="."
b=pd.read_csv(f"{S}/baseline_oos.csv")
d=json.load(open('website/public/data/model_performance.json'))
sc=pd.DataFrame(d['scatter']).rename(columns={'pos':'position','year':'draft_year','pred':'model_pred'})
q=pd.read_csv('output/temporal_cv_qb_te/oos_predictions.csv')
w={'QB':0.5,'TE':0.3}
q['model_pred']=[ (w[p]*e+(1-w[p])*c) if not np.isnan(c) else e for p,e,c in zip(q.position,q.exp_ppg,q.comp_weighted_ppg_honest)]
q=q.rename(columns={'pfr_player_name':'name','ppg':'actual','exp_ppg':'model_only'})
q['hit']=q.made_it==1
allm=pd.concat([sc[['name','position','draft_year','pick','round','model_pred','actual']], q[['name','position','draft_year','pick','round','model_pred','actual','model_only']]])
m=allm.merge(b[['name','position','draft_year','base_pred','actual']].rename(columns={'actual':'actual_b'}),on=['name','position','draft_year'],how='left')
assert m.base_pred.notna().all(), 'players missing from baseline'
assert (abs(m.actual-m.actual_b)<0.01).all(), 'actuals differ between model and baseline'
m.to_csv(f"{S}/merged.csv",index=False)
BG="#141A26"; PAGE="#0B0E13"; INK="#F0F4FF"; INK2="#93A1C4"; MUTED="#5E6B8C"; GRID="#232B3B"
POS={"QB":"#D06BB4","RB":"#2DC98F","WR":"#EFA030","TE":"#4C93F0"}
plt.rcParams.update({"font.family":"Helvetica Neue","font.size":11,"text.color":INK,"axes.labelcolor":INK2,
  "xtick.color":INK2,"ytick.color":INK2,"axes.edgecolor":GRID,"figure.facecolor":BG,"axes.facecolor":BG,"savefig.facecolor":BG})
def style(ax):
    for s in ["top","right"]: ax.spines[s].set_visible(False)
    for s in ["left","bottom"]: ax.spines[s].set_color(GRID)
    ax.tick_params(length=0); ax.grid(True,color=GRID,lw=0.8); ax.set_axisbelow(True)
def title(fig,t,sub):
    fig.text(0.03,0.955,t,fontsize=16,fontweight="bold",color=INK,va="top")
    fig.text(0.03,0.895,sub,fontsize=11,color=INK2,va="top")

order=["QB","RB","WR","TE"]

# ── 1. predicted vs actual small multiples ──────────────────────────────
fig,axes=plt.subplots(1,4,figsize=(14,4.6)); fig.subplots_adjust(left=0.05,right=0.985,top=0.74,bottom=0.15,wspace=0.28)
title(fig,"Predicted vs. actual early-career fantasy points per game",
      "Walk-forward out-of-sample: each draft class 2016–2023 scored by a model trained only on earlier classes. Busts score 0.")
LABELS={"QB":{"Lamar Jackson":(-78,4),"Brock Purdy":(6,4),"Josh Allen":(6,-10)},
        "RB":{"Christian McCaffrey":(-60,6),"Aaron Jones":(-70,4),"Saquon Barkley":(-40,8),"Kyren Williams":(6,2)},
        "WR":{"Tyreek Hill":(6,-3),"Puka Nacua":(6,-3),"Justin Jefferson":(6,-10),"N'Keal Harry":(-30,10)},
        "TE":{"George Kittle":(6,-3),"Kyle Pitts":(6,-3),"Sam LaPorta":(-62,6)}}
for ax,p in zip(axes,order):
    g=m[m.position==p]; lim=max(g.actual.max(),g.model_pred.max())*1.08
    ax.plot([0,lim],[0,lim],color=MUTED,lw=1,ls=(0,(4,3)),zorder=1)
    ax.scatter(g.model_pred,g.actual,s=26,color=POS[p],alpha=0.8,edgecolor=BG,linewidth=0.8,zorder=3)
    for nm,off in LABELS[p].items():
        r=g[g.name==nm]
        if len(r): r=r.iloc[0]; ax.annotate(nm,(r.model_pred,r.actual),xytext=off,textcoords="offset points",fontsize=8.5,color=INK)
    mae=(g.model_pred-g.actual).abs().mean(); rr=g.model_pred.corr(g.actual)
    ax.set_xlim(0,lim); ax.set_ylim(-0.6,lim); style(ax)
    ax.set_title(p,loc="left",fontsize=13,fontweight="bold",color=POS[p])
    ax.set_title(f"n={len(g)} · MAE {mae:.2f} · r {rr:.2f}",loc="right",fontsize=9.5,color=INK2)
    ax.set_xlabel("Predicted PPG")
axes[0].set_ylabel("Actual PPG (first 3 seasons)")
fig.savefig(f"{OUT}/pred_vs_actual.png",dpi=160); plt.close(fig)

# ── 2. model vs draft-slot baseline ─────────────────────────────────────
rng=np.random.default_rng(7); rows=[]
m["rg"]=np.where(m["round"]<=2,"Rounds 1–2","Rounds 3–7")
for p in order:
    for rg in ["Rounds 1–2","Rounds 3–7",None]:
        g=m[(m.position==p)&((m.rg==rg) if rg else True)]
        d=((g.base_pred-g.actual).abs()-(g.model_pred-g.actual).abs()).values
        b=[d[rng.integers(0,len(d),len(d))].mean() for _ in range(4000)]
        rows.append(dict(pos=p,rg=rg or "All rounds",n=len(d),gain=d.mean(),lo=np.percentile(b,2.5),hi=np.percentile(b,97.5)))
R=pd.DataFrame(rows); R.to_csv(f"{S}/gain_table.csv",index=False)
fig,ax=plt.subplots(figsize=(11,6.4)); fig.subplots_adjust(left=0.2,right=0.96,top=0.79,bottom=0.12)
title(fig,"Does the model know more than draft slot?",
      "Change in mean absolute error vs. a draft-slot-only baseline (same folds, same players). Right of zero = model is more accurate.\nLines are 95% bootstrap intervals.  Green = clearly better · red = clearly worse · gray = can't tell apart.")
y=0; ticks=[]; labs=[]
for p in order:
    for rg in ["All rounds","Rounds 1–2","Rounds 3–7"]:
        r=R[(R.pos==p)&(R.rg==rg)].iloc[0]
        sig=r.lo>0 or r.hi<0
        col="#2DC98F" if r.lo>0 else ("#E5484D" if r.hi<0 else MUTED)
        ax.plot([r.lo,r.hi],[y,y],color=col,lw=2.4,solid_capstyle="round")
        ax.scatter([r.gain],[y],s=70 if rg=="All rounds" else 46,color=col if sig else INK2,edgecolor=BG,lw=1.5,zorder=3)
        ax.text(max(r.hi,0)+0.05 if r.gain>=0 or True else r.lo-0.05,y,f"{r.gain:+.2f}  (n={r.n})",va="center",fontsize=9,color=INK2)
        ticks.append(y); labs.append(f"{p} · {rg}" if rg=="All rounds" else rg)
        y-=1
    y-=0.6
ax.axvline(0,color=INK2,lw=1)
ax.set_yticks(ticks); ax.set_yticklabels(labs)
for t,l in zip(ax.get_yticklabels(),labs):
    if "·" in l: t.set_fontweight("bold"); t.set_color(POS[l[:2]])
style(ax); ax.grid(axis="y",visible=False)
ax.set_xlabel("PPG of error removed vs. draft-slot baseline")
ax.set_xlim(-1.4,1.0)
fig.savefig(f"{OUT}/vs_draft_slot.png",dpi=160); plt.close(fig)

# ── 3. calibration of P(hit) ────────────────────────────────────────────
a=pd.read_csv(f"{ROOT}/output/temporal_cv/oos_predictions.csv")[["position","p_made_it","made_it"]]
b=pd.read_csv(f"{ROOT}/output/temporal_cv_qb_te/oos_predictions.csv")[["position","p_made_it","made_it"]]
c=pd.concat([a,b]).dropna()
bins=[0,0.4,0.55,0.7,0.8,0.9,0.95,1.0]
c["bin"]=pd.cut(c.p_made_it,bins,include_lowest=True)
cal=c.groupby("bin",observed=True).agg(pred=("p_made_it","mean"),obs=("made_it","mean"),n=("made_it","size")).reset_index()
cal.to_csv(f"{S}/calibration.csv",index=False)
fig,ax=plt.subplots(figsize=(7.2,6)); fig.subplots_adjust(left=0.13,right=0.95,top=0.8,bottom=0.12)
title(fig,"Is \"probability of hitting\" honest?",f"Out-of-sample, all positions (n={len(c)}). Dot size = players in bin.\nOn the dashed line, a 70% call hits 70% of the time.")
ax.plot([0,1],[0,1],color=MUTED,lw=1,ls=(0,(4,3)))
ax.plot(cal.pred,cal.obs,color="#4C93F0",lw=2,zorder=2)
ax.scatter(cal.pred,cal.obs,s=cal.n*2.2+20,color="#4C93F0",edgecolor=BG,lw=2,zorder=3)
for _,r in cal.iterrows():
    ax.annotate(f"{r.obs:.0%}  (n={r.n})",(r.pred,r.obs),xytext=(-88 if r.pred>0.95 else 10,-4 if r.pred>0.95 else -12),textcoords="offset points",fontsize=9,color=INK2)
ax.set_xlim(0.2,1.02); ax.set_ylim(0.2,1.02); style(ax)
ax.xaxis.set_major_formatter(matplotlib.ticker.PercentFormatter(1)); ax.yaxis.set_major_formatter(matplotlib.ticker.PercentFormatter(1))
ax.set_xlabel("Predicted probability of a qualifying NFL season"); ax.set_ylabel("Observed hit rate")
fig.savefig(f"{OUT}/calibration.png",dpi=160); plt.close(fig)
print(R.round(2).to_string()); print(cal.round(3).to_string())
