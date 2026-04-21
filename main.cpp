#include <bits/stdc++.h>
using namespace std;

struct Term { long long a; int b,c,d; };

struct Poly {
    vector<Term> v;
    void simplify(){
        vector<Term> t; t.reserve(v.size());
        for(const auto &x: v) if(x.a!=0) t.push_back(x);
        sort(t.begin(), t.end(), [](const Term &x, const Term &y){
            if(x.b!=y.b) return x.b>y.b;
            if(x.c!=y.c) return x.c>y.c;
            return x.d>y.d;
        });
        vector<Term> r;
        for(const auto &x: t){
            if(!r.empty() && r.back().b==x.b && r.back().c==x.c && r.back().d==x.d){
                r.back().a += x.a;
                if(r.back().a==0) r.pop_back();
            }else r.push_back(x);
        }
        v.swap(r);
    }
    static Poly fromConst(long long k){ Poly p; if(k!=0) p.v.push_back({k,0,0,0}); return p; }
    Poly operator+(const Poly &o) const { Poly r; r.v.reserve(v.size()+o.v.size()); r.v.insert(r.v.end(), v.begin(), v.end()); r.v.insert(r.v.end(), o.v.begin(), o.v.end()); r.simplify(); return r; }
    Poly operator-(const Poly &o) const { Poly r; r.v.reserve(v.size()+o.v.size()); r.v.insert(r.v.end(), v.begin(), v.end()); for(auto &t: o.v) r.v.push_back({-t.a,t.b,t.c,t.d}); r.simplify(); return r; }
    Poly operator*(const Poly &o) const { Poly r; r.v.reserve(v.size()*o.v.size()); for(const auto &x: v) for(const auto &y: o.v) r.v.push_back({x.a*y.a, x.b+y.b, x.c+y.c, x.d+y.d}); r.simplify(); return r; }
    Poly der() const { Poly r; for(const auto &t: v){ if(t.b>0) r.v.push_back({t.a*t.b, t.b-1, t.c, t.d}); if(t.c>0) r.v.push_back({t.a*t.c, t.b, t.c-1, t.d+1}); if(t.d>0) r.v.push_back({-t.a*t.d, t.b, t.c+1, t.d-1}); } r.simplify(); return r; }
    bool is_zero() const { return v.empty(); }
    bool is_one() const { return v.size()==1 && v[0].a==1 && v[0].b==0 && v[0].c==0 && v[0].d==0; }
    string to_string_poly() const{
        if(v.empty()) return string("0");
        string s;
        for(size_t i=0;i<v.size();++i){
            const auto &t=v[i]; long long a=t.a; bool first=(i==0);
            if(a<0){ s.push_back('-'); a=-a; }
            else if(!first){ s.push_back('+'); }
            bool isConst = (t.b==0 && t.c==0 && t.d==0);
            bool showCoef = isConst || (a!=1);
            if(showCoef) s += std::to_string(a);
            auto append_pow=[&](const char* base, int pow){
                if(pow<=0) return;
                if(string(base)=="x"){
                    s += "x"; if(pow>1){ s += "^"; s += std::to_string(pow);} }
                else if(string(base)=="sin"){
                    s += "sin"; if(pow>1){ s += "^"; s += std::to_string(pow);} s += "x"; }
                else { s += "cos"; if(pow>1){ s += "^"; s += std::to_string(pow);} s += "x"; }
            };
            append_pow("x", t.b); append_pow("sin", t.c); append_pow("cos", t.d);
            if(isConst && !showCoef) s += "1";
        }
        return s;
    }
};

struct Frac{
    Poly p,q;
    static Frac fromPoly(const Poly &pp){ Frac f; f.p=pp; f.q=Poly::fromConst(1); return f; }
    static Frac fromConst(long long k){ return fromPoly(Poly::fromConst(k)); }
    Frac add(const Frac &o) const { Frac r; r.p=p*o.q + o.p*q; r.q=q*o.q; return r; }
    Frac sub(const Frac &o) const { Frac r; r.p=p*o.q - o.p*q; r.q=q*o.q; return r; }
    Frac mul(const Frac &o) const { Frac r; r.p=p*o.p; r.q=q*o.q; return r; }
    Frac divi(const Frac &o) const { Frac r; r.p=p*o.q; r.q=q*o.p; return r; }
    Frac der() const { Frac r; Poly dp=p.der(), dq=q.der(); r.p = dp*q - dq*p; r.q = q*q; return r; }
    string to_string_frac() const{
        if(p.is_zero()) return string("0");
        if(q.is_one()) return p.to_string_poly();
        string s1=p.to_string_poly(), s2=q.to_string_poly();
        bool p_multi = (p.v.size()>1), q_multi=(q.v.size()>1);
        string out; out += (p_multi?"(":""); out += s1; out += (p_multi?")":""); out += "/"; out += (q_multi?"(":""); out += s2; out += (q_multi?")":""); return out;
    }
};

struct Parser{
    const string &s; int n; int i;
    Parser(const string &str): s(str), n((int)str.size()), i(0) {}
    Frac parseExpr(){ Frac lhs=parseTerm(); while(i<n){ if(s[i]=='+'){ ++i; Frac rhs=parseTerm(); lhs=lhs.add(rhs);} else if(s[i]=='-'){ ++i; Frac rhs=parseTerm(); lhs=lhs.sub(rhs);} else break; } return lhs; }
    Frac parseTerm(){ Frac lhs=parseUnary(); while(i<n){ if(s[i]=='*'){ ++i; Frac rhs=parseUnary(); lhs=lhs.mul(rhs);} else if(s[i]=='/'){ ++i; Frac rhs=parseUnary(); lhs=lhs.divi(rhs);} else break; } return lhs; }
    Frac parseUnary(){ int sign=1; while(i<n && (s[i]=='+'||s[i]=='-')){ if(s[i]=='-') sign=-sign; ++i; } Frac f=parseFactor(); if(sign==-1){ Frac neg=Frac::fromConst(-1); f=neg.mul(f);} return f; }
    Frac parseFactor(){ if(i<n && s[i]=='('){ int depth=1; int j=i+1; while(j<n && depth){ if(s[j]=='(') ++depth; else if(s[j]==')') --depth; ++j; } string inner=s.substr(i+1, j-i-2+1); Parser sub(inner); Frac f=sub.parseExpr(); i=j; return f; } else { Poly p=parseMonomial(); return Frac::fromPoly(p);} }
    Poly parseMonomial(){ long long coef=0; bool coef_set=false; int bx=0, cs=0, dc=0; bool any=false; while(i<n && isdigit((unsigned char)s[i])){ coef=coef*10 + (s[i]-'0'); coef_set=true; ++i; } if(!coef_set) coef=1; while(i<n){ char ch=s[i]; if(ch=='x'){ any=true; ++i; int pow=1; if(i<n && s[i]=='^'){ ++i; int val=0; while(i<n && isdigit((unsigned char)s[i])){ val=val*10 + (s[i]-'0'); ++i; } pow=val; } if(pow>0) bx+=pow; }
        else if(ch=='s'){ if(i+2<n && s.substr(i,3)=="sin"){ i+=3; int pow=1; if(i<n && s[i]=='^'){ ++i; int val=0; while(i<n && isdigit((unsigned char)s[i])){ val=val*10 + (s[i]-'0'); ++i; } pow=val; } if(i<n && s[i]=='x') ++i; if(pow>0){ cs+=pow; any=true; } } else break; }
        else if(ch=='c'){ if(i+2<n && s.substr(i,3)=="cos"){ i+=3; int pow=1; if(i<n && s[i]=='^'){ ++i; int val=0; while(i<n && isdigit((unsigned char)s[i])){ val=val*10 + (s[i]-'0'); ++i; } pow=val; } if(i<n && s[i]=='x') ++i; if(pow>0){ dc+=pow; any=true; } } else break; }
        else break; }
        if(!any) return Poly::fromConst(coef);
        Poly p; if(coef!=0) p.v.push_back({coef,bx,cs,dc}); p.simplify(); return p;
    }
};

int main(){ ios::sync_with_stdio(false); cin.tie(nullptr); string expr; if(!getline(cin, expr)) return 0; Parser parser(expr); Frac f=parser.parseExpr(); Frac df=f.der(); cout<<f.to_string_frac()<<"\n"<<df.to_string_frac()<<"\n"; return 0; }
