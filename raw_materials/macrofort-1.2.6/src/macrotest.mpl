init_genfor():

writeto(testout):
interface(quiet=true):

# TESTING ELEMENTARY INSTRUCTIONS

genfor([callf,foo,[a,b[10],c(10)]]);
genfor([closef,10]);
genfor([commentf,`This is a comment`]);
genfor([commonf,roll,[a,b,c[10]]]);
genfor([continuef,label]);
genfor([declaref,real,[a,b[12,3],c[2*n]]]);
genfor([dof,label,i,1,m,-1]);
genfor([dof,label,i,1,m]);
genfor([elsef]);
genfor([endf]);
genfor([endiff]);
genfor([equalf,a,1+x]);
genfor([equalf,[[a,1+sin(t)],[b,cos(t)+sin(t)],[c,1+cos(t)]]]);
optimized:=true:
genfor([equalf,[[a,1+sin(t)],[b,cos(t)+sin(t)],[c,1+cos(t)]]]);
optimized:=false:
genfor([formatf,label,[`2x,'value : ',e14.7`]]);
genfor([functionf,integer,foo,[a,b,c]]);
genfor([gotof,label]);
genfor([if_goto_f,a>2,label]);
genfor([if_then_f,a=1]);
genfor([openf,10,`foo.data`,unknown]);
genfor([parameterf,[n=1,m=2]]);
genfor([programf,foo]);
genfor([readf,10,label,[x,y,z[2]]]);
genfor([returnf]);
genfor([subroutinef,foo,[a,b,c]]);
genfor([writef,12,label,[a,`(b(i),i=1,n)`]]);

# TESTING MISCELLANEOUS FEATURES

# continuation statements
bigexp:=x+2*x**2+3*x**3+4*x**4+5*x**5+6*x**6+7*x**7+8*x**8+
        9*x**9+10*x**10+11*x**11+12*x**12+13*x**13+14*x**14+
        15*x**15+16*x**16+17*x**17+18*x**18+19*x**19+20*x**20:
genfor([equalf,a,bigexp]);
genfor([if_goto_f,bigexp>2,label]);

# labels
genfor([gotof,10]);
genfor([gotof,label1]);
genfor([dof,label1,i,1,m]);
genfor([formatf,label2,[`2x,e14.7`]]);
genfor([gotof,label2]);
genfor([writef,12,label2,[a,`(b(i),i=1,n)`]]);

# conditions
genfor([if_then_f,OR(b<=2,NOT(b<>1))]);
genfor([if_then_f,AND(a<1,AND(b>1,c>=2))]);
genfor([if_then_f,NOT(a=2)]);

# TESTING MACRO INSTRUCTIONS

genfor([dom,i,1,10,[[equalf,a,1],[equalf,b,2]]]);
genfor([dom,i,1,10,2,[[equalf,a,1],[equalf,b,2]]]);
genfor([functionm,integer,foo,[i,j],[[equalf,a,1],[equalf,b,2]]]);
genfor([if_then_else_m,AND(a>b,b>0),[[equalf,a,1],[equalf,b,2]],
        [[equalf,a,1],[equalf,b,2]]]);
genfor([if_then_m,AND(a>b,b>0),[equalf,b,a]]);
mat:=array(1..2,1..2):
for i to 2 do for j to 2 do mat[i,j]:=(x||i)^(j-1) od od:i:='i':j:='j':
genfor([matrixm,m,mat]);
genfor([openm,10,`toto.data`,old,[readm,10,[`i10`],[j]]]);
genfor([programm,foo,[[equalf,a,1],[equalf,b,2]]]);
genfor([readm,input,[`2x,e14.7`],[x,y,z[2]]]);
genfor([subroutinem,foo,[a,b,i],[[equalf,a,1],[equalf,b,2]]]);
genfor([untilm,abs(a)<eps,[equalf,a,big],
         [[equalf,a,a/2.0],[equalf,b,2]],1000]);
genfor([whilem,abs(a)>eps,[equalf,a,big],
         [[equalf,a,a/2.0],[equalf,b,2]],1000]);
genfor([writem,output,[`2x,e14.7`],[x,y,z[2]]]);

# COMMONM and DECLAREM
genfor([programm,foo,
  [[declaref,integer,[i,j]],
   [commonf,toto,[a,b]],
   [equalf,a,1],
   [declarem,real,[c[10]]],
   [commonm,tata,[c]],
   [equalf,b,1]]]);

# nested loops
genfor([dom,i,1,10,[dom,j,1,10,[dom,k,1,10,[equalf,a(i,j,k),i+j+k]]]]);
genfor([whilem,abs(a)>eps,[equalf,a,big],
         [[equalf,a,a/2.0],
          [untilm,abs(a)<eps,[equalf,a,big],
            [[equalf,a,a/2.0],[equalf,b,2]],1000],
          [equalf,b,2]],
         1000]);

# next and break
genfor([dom,i,1,10,
         [[dom,j,1,10,[[equalf,a,1],[nextm],
                      [dom,k,1,10,[breakm]]]],
          [equalf,b,2]]]);
genfor([whilem,abs(a)>eps,[equalf,a,big],
         [[if_then_m,a>1,[nextm]],
          [equalf,a,a/2.0],
          [if_then_m,a<1,[breakm]]],
         1000]);
genfor([untilm,abs(a)>eps,[equalf,a,big],
         [[if_then_m,a>1,[nextm]],
          [equalf,a,a/2.0],
          [if_then_m,a<1,[breakm]]],
         1000]);

# global variables

comment:=false:
genfor([whilem,abs(a)>eps,[equalf,a,big],
         [[equalf,a,a/2.0],
          [untilm,abs(a)<eps,[equalf,a,big],
            [[equalf,a,a/2.0],[equalf,b,2]],1000],
          [equalf,b,2]],
         1000]);

precision:=double:
genfor([equalf,a,1.0+Pi*sinh(exp(1))]);
precision:=single:
genfor([equalf,a,1.0+Pi*sinh(exp(1))]);

optimized:=true:
mat:=array([[1+sin(x),1-sin(x)]]):
genfor([matrixm,m,mat]);
optimized:=false:
mat:=array([[1+sin(x),1-sin(x)]]):
genfor([matrixm,m,mat]);

# fixed bugs (see CHANGES file)

convert((a+b)*c,string);
genfor([dom,i,1,a+b,[]]);
genfor([equalf,a,toto(0)]);
genfor([continuef,1]);
genfor([equalf,a,g()]);
genfor([if_then_f,AND(a>b,OR(b>0,a>0))]);

# miscelleaneous

genfor([equalf,a,30!]);
genfor([equalf,a,Pi+exp(1)*x+gamma*y]);

quit
