# Parses every Appendix A formula in ../main_IJPE.tex and checks it numerically against the two-stage model.
# Requires: pip install sympy antlr4-python3-runtime==4.11 ; run from any directory.
import re, random, sympy as sp
from sympy.parsing.latex import parse_latex
from sympy.core.function import AppliedUndef
tex=open('/Users/johnwu/百度云同步盘/paper/greenwashing/main_IJPE.tex').read()
app=tex[tex.index('Appendix A: Optimal'):tex.index('Appendix B: Proofs')]
a,b,rho,g,gam,lam,th,k,cg=sp.symbols('a b rho g gamma lambda theta k c_g',positive=True)
names={'a':a,'b':b,'rho':rho,'g':g,'gamma':gam,'lambda':lam,'theta':th,'k':k,'c_g':cg,'c_{g}':cg}
def fix(e):
    e=e.replace(lambda x:isinstance(x,AppliedUndef),lambda x:sp.Symbol(x.func.__name__)*x.args[0])
    return e.subs({sp.Symbol(n):v for n,v in names.items()})
exprs={}
for m in re.finditer(r'\$\\?([A-Za-z]+)(?:_(\w))?\^\{(\w+)\}=(.+?)\$(?=[,.]|\s*\\\\|\s*$)',app,re.S):
    var,sub,case,body=m.groups()
    body=body.replace('{}^2','^2').replace('\\left(','(').replace('\\right)',')').replace('[','(').replace(']',')')
    body=re.sub(r'([A-Za-z0-9\)\}])\s*\(',r'\1 \\cdot (',body)
    try: exprs[(var,sub,case)]=fix(parse_latex(body))
    except Exception as ex: print('PARSE FAIL',var,sub,case,str(ex)[:80])
print('parsed',len(exprs),sorted(exprs))
be,wo,wg,po,pg=sp.symbols('beta w_o w_g p_o p_g')
def model(case,h,structure):
    i,fo,fg=case[0],case[1],case[2]
    if i=='N': Dg=(1-rho)*a-pg+b*po+gam*g*(1+be); gw=h*th*be**2; c=0
    else: Dg=(1-rho)*a-pg+b*po+g; gw=0; c=cg
    Do=rho*a-po+b*pg
    pip=((lam*po*Do) if fo=='A' else (po-wo)*Do)+((lam*pg-c)*Dg if fg=='A' else (pg-wg-c)*Dg)
    pio=(1-lam)*po*Do if fo=='A' else wo*Do
    pig=((1-lam)*pg*Dg if fg=='A' else wg*Dg)-gw-k*g**2/2
    pi={'o':pio,'g':pig,'p':pip}
    whole=([('o',wo)] if fo=='R' else [])+([('g',wg)] if fg=='R' else [])
    ag=([('o',po)] if fo=='A' else [])+([('g',pg)] if fg=='A' else [])
    pl=([('p',po)] if fo=='R' else [])+([('p',pg)] if fg=='R' else [])
    B=[('g',be)] if i=='N' else []
    if structure=='2stage': stages=[whole+ag+B,pl]
    else: stages=[B,whole,ag+pl]   # paper text: beta | wholesale | all retail prices
    stages=[s for s in stages if s]
    sub={}; first=None
    if structure!='2stage' and B: first=stages.pop(0)
    for st in reversed(stages):
        eqs=[sp.diff(pi[p].subs(sub),v) for p,v in st]
        sol=sp.solve(eqs,[v for _,v in st],dict=True)[0]
        sub={kk:vv.subs(sol) for kk,vv in sub.items()}; sub.update(sol)
    if first:
        PIg=sp.simplify(pi['g'].subs(sub)); bs=sp.solve(sp.diff(PIg,be),be)[0]
        sub={kk:vv.subs(be,bs) for kk,vv in sub.items()}; sub[be]=bs
    out={'w_o':wo,'w_g':wg,'p_o':po,'p_g':pg,'beta':be}
    res={n:sp.simplify(v.subs(sub)) for n,v in out.items() if v in sub}
    for m_ in 'ogp': res['pi_'+m_]=sp.simplify(pi[m_].subs(sub))
    return res
random.seed(1)
pts=[{a:1,b:sp.Rational(random.randint(5,40),100),rho:sp.Rational(random.randint(20,60),100),g:sp.Rational(random.randint(20,60),100),
      gam:sp.Rational(random.randint(30,90),100),lam:sp.Rational(random.randint(5,60),100),th:sp.Rational(random.randint(40,200),100),
      k:sp.Rational(random.randint(20,120),100),cg:sp.Rational(random.randint(0,5),100)} for _ in range(3)]
for case in ['NRR','NRA','NAR','NAA','BRR','BRA','BAR','BAA']:
    for structure,h in [('2stage',1)]:
        res=model(case,h,structure)
        line=[]
        for (var,sub_,c),e in exprs.items():
            if c!=case: continue
            key=('pi_'+sub_) if var=='pi' else ('beta' if var=='beta' else f'{var}_{sub_}')
            if key not in res: line.append(f'{key}:n/a'); continue
            ok=all(abs(float((e-res[key]).subs(pt)))<1e-9 for pt in pts)
            line.append(f"{key}:{'OK' if ok else 'X'}")
        print(case,structure,'h=',h,' '.join(line))
