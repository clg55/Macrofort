#****************************************************************************
#                     Macrofort: Fortran code generator
#                     Version 1.2.5 (2 September 1998) for Maple V.4
#
#                       Copyright (C) 1990-98 INRIA
#
#                                Claude Gomez
#			email: Claude.Gomez@inria.fr	
#
#				 META2 project
#
#                                    INRIA				    
# 			      Domaine de Voluceau			    
# 			     Rocquencourt - BP 105			    
# 			     78153 Le Chesnay Cedex			    
# 				     FRANCE	
#
#****************************************************************************

#****************************************************************************
#  FUNCTIONS AVAILABLE FOR THE USER IN MACROFORT:
# GENFOR INIT_GENFOR PUSHE
#
#  GLOBAL VARIABLES AVAILABLE FOR THE USER:
# COMMENT INPUT OUTPUT OPTIMIZED PRECISION
#****************************************************************************

#****************************************************************************
#  internal functions directly related to Macrofort : (`macrofort/<name>`)
# callf closef commentf commonf continuef declaref dof dom elsef endf endiff 
# equalf formatf functionf functionm gotof ifgotof ifthenelsem ifthenf 
# ifthenm matrixm openf openm parameterf programf programm readf readm 
# returnf subroutinef subroutinem untilm whilem writef writem
#
#  other internal functions:
# get_flabel get_label list listcom lprint parse_condition space
#
#  internal global variables:
# nflab nflabel nlab nlabel nuntil nwhile
#  internal tables
# flabel label macro
#****************************************************************************

release:=interface(version):
if SearchText(`Release 4`,release) = 0 then
  ERROR(`Maple V Release 4 required`):
fi:

# utilitary functions

# the second argument of PUSHE (the list) must be quoted
pushe := proc(e,l) l:=[op(eval(l)),e] end:

`macrofort/space` := proc(n) 
  if n = 0 then `` else map(cat,` `$n) fi 
end:

`macrofort/get_label` := proc(label)
global `macrofort/label`,`macrofort/nlabel`;
  if type(label,'integer') then 
    cat(`macrofort/space`(4-length(label)),label)
  elif evaln(`macrofort/label`[label])  <> `macrofort/label`[label] then
    `macrofort/label`[label]
  else `macrofort/nlabel`:= `macrofort/nlabel` + 1;
       `macrofort/label`[label] := `macrofort/nlabel` + 1000;
       `macrofort/nlabel` + 1000
  fi
end:

`macrofort/get_flabel` := proc(flabel)
global `macrofort/nflabel`,`macrofort/flabel`;
  if type(flabel,'integer') then 
    cat(`macrofort/space`(4-length(flabel)),flabel)
  elif evaln(`macrofort/flabel`[flabel])  <> `macrofort/flabel`[flabel] then
    `macrofort/flabel`[flabel]
  else `macrofort/nflabel`:= `macrofort/nflabel` + 1;
       `macrofort/flabel`[flabel] := `macrofort/nflabel` + 2000;
       `macrofort/nflabel` + 2000
  fi
end:

`macrofort/list` := proc(l) 
  if l = [] then RETURN(l) fi;
  if type(l[1],'list') then l else [l] fi 
end:

`macrofort/listcomp` := proc(list)
  local i, listc;
  if list = [] then RETURN(``) fi;
  if type(list[1], `=`) then 
    listc:=cat(m_fortrans(op(1,list[1])),`=`,m_fortrans(op(2,list[1])))
  else listc := m_fortrans(list[1]) fi; 
  for i in list[2..nops(list)] do
    if type(i,`=`) then
      listc:=cat(listc,`,`,m_fortrans(op(1,i)),`=`,m_fortrans(op(2,i)))
    else listc := cat(listc,`,`,m_fortrans(i)) fi;
  od;
  listc
end:

`macrofort/listcom` := proc(list)
  local i, listc;
  if list = [] then RETURN(``) fi;
  listc := m_fortrans(list[1]);
  for i in list[2..nops(list)] do
    listc := cat(listc,`,`,m_fortrans(i))
  od;
  listc
end:

`macrofort/parse_condition` := proc(condition)
  local c;
  c:=`macrofort/parse_condition1`(condition);
  if (substring(c,1..1) = `(`) then
    substring(c,2..length(c)-1)
  else c
  fi
end:

`macrofort/parse_condition1` := proc(condition)
  if type(condition,`=`) then
    cat(`(`,m_fortrans(op(1,condition)),`.eq.`,
         m_fortrans(op(2,condition)),`)`)
  elif type(condition,`<`) then
    cat(`(`,m_fortrans(op(1,condition)),`.lt.`,
         m_fortrans(op(2,condition)),`)`)
  elif type(condition,`<=`) then
    cat(`(`,m_fortrans(op(1,condition)),`.le.`,
         m_fortrans(op(2,condition)),`)`)
  elif type(condition,`<>`) then
    cat(`(`,m_fortrans(op(1,condition)),`.ne.`,
         m_fortrans(op(2,condition)),`)`)
  elif type(condition,'function') then
    if op(0,condition) = 'NOT' then
      cat(`(`,`.not.`,`macrofort/parse_condition1`(op(1,condition)),`)`)
    elif op(0,condition) = 'AND' then
      cat(`(`,`macrofort/parse_condition1`(op(1,condition)),`.and.`,
          `macrofort/parse_condition1`(op(2,condition)),`)`)
    elif op(0,condition) = 'OR' then
      cat(`(`,`macrofort/parse_condition1`(op(1,condition)),`.or.`,
          `macrofort/parse_condition1`(op(2,condition)),`)`)
    else m_fortrans(condition)
    fi
  else m_fortrans(condition)
  fi
end:

`macrofort/lprint` := proc(str) 
  local l;
  l := length(str);
  if l <= 72 then printf(`%s\n`,str);
  else printf(`%s\n`,substring(str,1..72));
       `macrofort/lprint`(cat(`     +`,substring(str,73..l)));
  fi;
end:

#****************************************************************************
# GENFOR : function generating Fortran code from its description
#          by a list
#****************************************************************************

genfor := proc(l) 
  local old_Digits, old_quiet, old_labelling, mres;
  old_Digits := Digits;
  if assigned(precision) and 
     not member(precision,['single','double'])
  then ERROR(`bad setting for precision`,precision) fi;
  if precision = 'double' then Digits := 16 else Digits := 7 fi;
  old_quiet := interface(quiet);
  old_labelling := interface(labelling);
  interface(quiet=true);
  interface(labelling=false);
  mres := traperror(`macrofort/genform`(l));
  if mres<>lasterror then 
    if whattype(mres)='exprseq' then mres := [mres] fi;
    if traperror(`macrofort/genfor`(mres,0))=lasterror then
      print(lasterror) 
    fi; 
  else print(lasterror) fi;
  interface(quiet=old_quiet);
  interface(labelling=old_labelling);
  Digits := old_Digits;
  NULL
end:

# macros expansion
`macrofort/genform` := proc(l)
  local oper;
  if l = [] then RETURN([]) fi;
  if type(l,'list') then
      oper:=l[1];
      if oper = 'programm' or oper = 'subroutinem' or oper = 'functionm' then
        `macrofort/`.oper(op(l[2..nops(l)]))
      elif `macrofort/macro`[oper] = true then
        op(`macrofort/genform`(`macrofort/`.oper(op(l[2..nops(l)]))))
        else map(`macrofort/genform`,l)
      fi
    else l
  fi
end:

# macros expansion for COMMONM and DECLAREM
`macrofort/genformdcl` := proc(l,dcl,com)
  local oper;
  if l = [] then RETURN([]) fi;
  if type(l,'list') then
      oper:=l[1];
      if oper = 'declarem' then
        dcl:=[op(eval(dcl)),['declaref',l[2],l[3]]];
        [];
      elif oper = 'commonm' then
        com:=[op(eval(com)),['commonf',l[2],l[3]]];
        []
      elif oper = 'programm' or oper = 'subroutinem' or oper = 'functionm' then
        ERROR(`You cannot nest PROGRAM, FUNCTION or SUBROUTINE`)
      elif `macrofort/macro`[oper] = true then
        op(`macrofort/genformdcl`(`macrofort/`.oper(op(l[2..nops(l)])),
                                 'dcl','com'))
        else map(`macrofort/genformdcl`,l,'dcl','com')
      fi
    else l
  fi
end:

# code generation when all macros have been expanded
`macrofort/genfor` := proc(l,level)
  local e, oper;
  for e in l do 
    if e <> [] then
      if type(e,'name') then RETURN(`macrofort/genfor`([l],level)) fi;
      oper:=e[1];
      if type(oper,'name') then
        if oper = 'nextm' or oper = 'breakm' then
          ERROR(`NEXTM or BREAKM must only appear in a loop`);
          else `macrofort/`.oper(level,op(e[2..nops(e)])); fi;
        else `macrofort/genfor`(e,level+2)
      fi
    fi
  od
end:

#****************************************************************************
# ELEMENTARY INSTRUCTIONS 
#****************************************************************************

# for internal use only
`macrofort/auto_comment` := proc(level,string)
  if comment then
    printf(`%s\n`,cat('c',`macrofort/space`(level+5),string))
  fi
end:

# [EQUALF,VARIABLE,EXPRESSION] 
# [EQUALF,[[VARIABLE,EXPRESSION],...]] 
`macrofort/equalf` := proc(level,variable,expression) 
  local res,res2,l,v;
  res:=NULL; res2:=NULL;
  if type(variable,list) then
    for l in variable do
      res:=res,m_fortrans(l[1])=l[2];
    od;
    if optimized then 
      res:=[readlib(optimize)([res])];
      for l in res do
        res2:=res2,cat(`macrofort/space`(level),op(1,l))=op(2,l);
      od;
      `m_fortran/statement`([res2]);
    else 
      for l in [res] do
        res2:=res2,cat(`macrofort/space`(level),op(1,l))=op(2,l);
      od;
      m_fortran([res2]);
      fi
  else
    v:= cat(`macrofort/space`(level),m_fortrans(variable));
    m_fortran([v=expression]);
  fi
end:

# [RETURNF] 
`macrofort/returnf` := proc(level)
  printf(`%s\n`,cat(`macrofort/space`(level+6),'return'))
end:

# [ENDF] 
`macrofort/endf` := proc(level)
  printf(`%s\n`,cat(`macrofort/space`(level+6),'`end`'))
end:

# [COMMENTF,CHAINE] 
`macrofort/commentf` := proc(level,chaine)
  printf(`%s\n`,cat('c',`macrofort/space`(level+5),chaine))
end:

# [FORMATF,LABEL,LIST] 
`macrofort/formatf` := proc(level,label,list)
  `macrofort/lprint`(cat(` `,`macrofort/get_flabel`(label),
           `macrofort/space`(level+1),'format',`(`,
           `macrofort/listcom`(list),`)`))
end:

# [READF,UNIT,LABEL,LIST]
`macrofort/readf` := proc(level,unit,label,list)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`read(`,unit,`,`,
           `macrofort/get_flabel`(label),`) `,
           `macrofort/listcom`(list)))
end:

# [WRITEF,UNIT,LABEL,LIST]
`macrofort/writef` := proc(level,unit,label,list)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`write(`,unit,`,`,
           `macrofort/get_flabel`(label),`) `,
           `macrofort/listcom`(list)))
end:
# [OPENF,UNIT,FILE,STATUS]
`macrofort/openf` := proc(level,unit,file,status)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`open(unit=`,unit,
           `,file='`,file,`',status='`,status,`')`))
end:

# [CLOSEF,UNIT]
`macrofort/closef` := proc(level,unit)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`close(`,unit,`)`))
end:

# [IF_GOTO_F,CONDITION,LABEL]
`macrofort/if_goto_f` := proc(level,condition,label)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`if (`,
           `macrofort/parse_condition`(condition),`) goto `,
           `macrofort/get_label`(label)))
end:

# [IF_THEN_F,CONDITION]
`macrofort/if_then_f` := proc(level,condition)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`if (`,
           `macrofort/parse_condition`(condition),`) then`))
end:

# [ELSEF]
`macrofort/elsef` := proc(level)
  printf(`%s\n`,cat(`macrofort/space`(level+6),'`else`'))
end:

# [ENDIFF]
`macrofort/endiff` := proc(level)
  printf(`%s\n`,cat(`macrofort/space`(level+6),'endif'))
end:

# [GOTOF,LABEL]
`macrofort/gotof` := proc(level,label)
  printf(`%s\n`,cat(`macrofort/space`(level+6),`goto `,
           `macrofort/get_label`(label)))
end:

# [DOF,LABEL,INDEX,STARTI,ENDI,STEP]
# [DOF,LABEL,INDEX,STARTI,ENDI]
`macrofort/dof` := proc(level,label,index,starti,endi)
   if nargs = 6 then
    `macrofort/lprint`(cat(`macrofort/space`(level+6),`do `,
             `macrofort/get_label`(label),`, `,index,`=`,
             m_fortrans(starti),`,`,
             m_fortrans(endi),`,`,
             m_fortrans(args[nargs])))
  else
    `macrofort/lprint`(cat(`macrofort/space`(level+6),`do `,
             `macrofort/get_label`(label),`, `,index,`=`,
             m_fortrans(starti),`,`,
             m_fortrans(endi)))
  fi;
end:

# [CONTINUEF,LABEL]
`macrofort/continuef` := proc(level,label)
  printf(`%s\n`,cat(` `,`macrofort/get_label`(label),
           `macrofort/space`(level+1),'continue'))
end:

# [PROGRAMF,NAME]
`macrofort/programf` := proc(level,name)
  printf(`%s\n`,cat(`macrofort/space`(level+6),`program `,name))
end:

# [SUBROUTINEF,NAME,LIST]
`macrofort/subroutinef` := proc(level,name,list)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`subroutine `,name,`(`,
           `macrofort/listcom`(list),`)`))
end:

# [FUNCTIONF,TYPE,NAME,LIST]
`macrofort/functionf` := proc(level,type,name,list)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),type,` function `,name,`(`,
           `macrofort/listcom`(list),`)`))
end:

# [CALLF,NAME,LIST]
`macrofort/callf` := proc(level,name,list)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`call `,name,`(`,
           `macrofort/listcom`(list),`)`))
end:

# [COMMONF,NAME,LIST]
`macrofort/commonf` := proc(level,name,list)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`common/`,name,`/`,
           `macrofort/listcom`(list)))
end:

# [DECLAREF,TYPE,LIST]
`macrofort/declaref` := proc(level,type,list)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),type,` `,
           `macrofort/listcom`(list)))
end:

# [PARAMETERF,LIST]
`macrofort/parameterf` := proc(level,list)
  `macrofort/lprint`(cat(`macrofort/space`(level+6),`parameter (`,
           `macrofort/listcomp`(list),`)`))
end:

#*************************************************************************
# MACRO INSTRUCTIONS
#*************************************************************************

# [MATRIXM,VARIABLE,MATRIX]
# MATRIXM is a macro instruction for Fortran but not for Macrofort
# we put "level+1" for the case of level = 0
`macrofort/matrixm` := proc(level,variable,matrix)
  assign(cat(`macrofort/space`(level+1),variable),op(matrix));
  m_fortran(cat(`macrofort/space`(level+1),variable));
end:

# [DOM,INDEX,STARTI,ENDI,STEP,DO_LIST]
# [DOM,INDEX,STARTI,ENDI,DO_LIST]
`macrofort/macro`['dom'] := true:
`macrofort/dom` := proc(index,starti,endi,l)
  global `macrofort/nlab`;
  local breaklabel, dolist, dolabel, isbreak, isnext, res, step;
  if nargs = 5 then
    dolist := args[5];
    step := l;
  else
    dolist := l;
    step := false;
  fi;
  dolabel := `m_label`.`macrofort/nlab`;
  `macrofort/nlab` := `macrofort/nlab` + 1;
  breaklabel := `m_label`.`macrofort/nlab`;
  `macrofort/nlab` := `macrofort/nlab` + 1;
  isbreak := false;
  dolist:=`macrofort/nextbreak`(dolist,'isbreak','isnext',breaklabel,dolabel);
  res := [['auto_comment',``]];
  if step = false then
    res := [res,['dof',dolabel,index,starti,endi]]
  else res := [res,['dof',dolabel,index,starti,endi,step]]
  fi;
  res := [op(res),`macrofort/list`(dolist),['continuef',dolabel]];
  if isbreak then
    res := [op(res),['continuef',breaklabel]];
  else `macrofort/nlab` := `macrofort/nlab` - 1; fi;
  res := [op(res),['auto_comment',``]];
end:

# [NEXTM], [BREAKM]
# generation of break and next into loops
`macrofort/nextbreak`:= proc(l,isbreak,isnext,breaklabel,nextlabel)
  local oper;
  if l = [] then RETURN([]) fi;
  if type(l,'list') then
      oper:=l[1];
      if oper = 'dom' or oper = 'whilem' or oper = untilm then l
      elif oper = 'nextm' then
        isnext := true;
        [gotof,nextlabel]
      elif oper = 'breakm' then
        isbreak := true;
        [gotof,breaklabel]
      else map(`macrofort/nextbreak`,l,'isbreak','isnext',
                                     breaklabel,nextlabel)
      fi
  else l
  fi
end:

# [IF_THEN_ELSE_M,CONDITION,THEN_LIST,ELSE_LIST]
`macrofort/macro`['if_then_else_m'] := true:
`macrofort/if_then_else_m` := proc(condition,then_list,else_list)
  local res;
  res := [['if_then_f',condition],`macrofort/list`(then_list)];
  if else_list <> [] then
    res := [op(res),['elsef'],`macrofort/list`(else_list),['endiff']]
  else res := [op(res),['endiff']]
  fi;
  res
end:

# [IF_THEN_M,CONDITION,THEN_LIST]
`macrofort/macro`['if_then_m'] := true:
`macrofort/if_then_m` := proc(condition,then_list)
  [['if_then_f',condition],`macrofort/list`(then_list),['endiff']]
end:

# [WHILEM,CONDITION,INIT_LIST,WHILE_LIST,MAX_WHILE] 
# [WHILEM,CONDITION,INIT_LIST,WHILE_LIST] 
`macrofort/macro`['whilem'] := true:
`macrofort/whilem` := proc(condition,init_list,while_list)
  global `macrofort/nlab`,`macrofort/nwhile`;
  local  breaklabel,isbreak,isnext,label,slabel,max_while,mxntl,
         nwhile,whf,inun;
  if nargs = 4 then max_while:=args[4]
  else max_while:=false fi;
  slabel:=`m_label`.`macrofort/nlab`;
  `macrofort/nlab`:=`macrofort/nlab` + 1;
  breaklabel := `m_label`.`macrofort/nlab`;
  `macrofort/nlab` := `macrofort/nlab` + 1;
  isbreak := false; isnext:=false;
  if max_while = false then inun:=NULL
  else 
    mxntl:=`maxwhile`.`macrofort/nwhile`;
    nwhile:=`nwhile`.`macrofort/nwhile`;
    inun:=[['equalf',mxntl,max_while],
           ['equalf',nwhile,0]];
  fi;
  whf:=[['auto_comment',` `],
        ['auto_comment',
          cat(`WHILE  (`,convert(condition,string),
            `) DO <WHILE_LIST> (`,`macrofort/nwhile`,`)`)],
        ['auto_comment',` `]];
  if (init_list <> []) or (max_while <> false) then
    whf:=[op(whf),['auto_comment',`WHILE LOOP INITIALIZATION`]];
    if (init_list = []) or type(init_list[1],'list') then
      whf:=[op(whf),[inun,op(init_list)]] 
    else whf:=[op(whf),[inun,init_list]] fi;
  fi;
  whf:=[op(whf),
       ['auto_comment',` `],
       ['auto_comment',`WHILE LOOP BEGINNING`],
       ['continuef',slabel],
       ['auto_comment',` `],
       ['auto_comment',`WHILE LOOP TERMINATION TESTS`],
       ['if_then_f',condition]];
  if max_while <> false then
    whf:=[op(whf),
         [['if_then_f',mxntl >= nwhile],
         [['auto_comment',` `],
          ['auto_comment',`NEW LOOP ITERATION`],
          ['equalf',nwhile,nwhile+1],
          ['auto_comment',` `],
          ['auto_comment',`<WHILE_LIST>`]],
         `macrofort/list`(
           `macrofort/nextbreak`(while_list,'isbreak','isnext',
                                 breaklabel,slabel)),
         ['gotof',slabel],
         ['elsef'],
         [['auto_comment',` `],
          ['auto_comment',`WHILE LOOP TERMINATION :`],
	  ['auto_comment',`BYPASSING THE MAXIMUM ITERATION NUMBER`],
          ['writem',output,[`' `.mxntl.` '`],[]]],
         ['endiff']]];
  else
    whf:=[op(whf),
         [['auto_comment',` `],
          ['auto_comment',`NEW LOOP ITERATION`],
          ['auto_comment',` `],
          ['auto_comment',`<WHILE_LIST>`]],
         `macrofort/list`(
           `macrofort/nextbreak`(while_list,'isbreak','isnext',
                                 breaklabel,slabel)),
         ['gotof',slabel]];
  fi;
  whf:=[op(whf),
       ['auto_comment',` `],
       ['auto_comment',`NORMAL WHILE LOOP TERMINATION`],
       ['endiff']];
  if isbreak then
    whf := [op(whf),['continuef',breaklabel]];
  else `macrofort/nlab` := `macrofort/nlab` - 1; fi;
  whf := [op(whf),
          ['auto_comment',cat(`WHILE LOOP END (`,`macrofort/nwhile`,`)`)]];
  `macrofort/nwhile` := `macrofort/nwhile` + 1;
  whf
end:

# [UNTILM,CONDITION,INIT_LIST,UNTIL_LIST,MAX_UNTIL] 
# [UNTILM,CONDITION,INIT_LIST,UNTIL_LIST] 
`macrofort/macro`['untilm'] := true:
`macrofort/untilm` := proc(condition,init_list,until_list)
  global `macrofort/nlab`,`macrofort/nuntil`;
  local breaklabel,isbreak,isnext,nextlabel,slabel,max_until,mxntl,
        nuntil,unf,inun;
  if nargs = 4 then max_until:=args[4]
  else max_until:=false fi;  
  slabel:=`m_label`.`macrofort/nlab`;
  `macrofort/nlab`:=`macrofort/nlab` + 1;
  breaklabel := `m_label`.`macrofort/nlab`;
  `macrofort/nlab` := `macrofort/nlab` + 1;
  isbreak := false;
  nextlabel := `m_label`.`macrofort/nlab`;
  `macrofort/nlab` := `macrofort/nlab` + 1;
  isnext := false;
  if max_until = false then inun:=NULL
  else
    mxntl:=`maxuntil`.`macrofort/nuntil`;
    nuntil:=`nuntil`.`macrofort/nuntil`;
    inun:=[['equalf',mxntl,max_until],
           ['equalf',nuntil,0]];
  fi;
  unf:=[['auto_comment',` `],
        ['auto_comment',
          cat(`DO <UNTIL_LIST> UNTIL (`,
            convert(condition,string),`) (`,`macrofort/nuntil`,`)`)],
        ['auto_comment',` `]];
  if (init_list <> []) or (max_until <> false) then
    unf:=[op(unf),['auto_comment',`UNTIL LOOP INITIALIZATION`]];
    if (init_list = []) or type(init_list[1],'list') then
      unf:=[op(unf),[inun,op(init_list)]] 
    else unf:=[op(unf),[inun,init_list]] fi;
  fi;
  unf:=[op(unf),
       ['auto_comment',` `],
       ['auto_comment',`UNTIL LOOP BEGINNING`],
       ['continuef',slabel]];
  if max_until <> false then
    unf:=[op(unf),
         ['equalf',nuntil,nuntil+1],
         ['auto_comment',` `],
         ['auto_comment',`<UNTIL_LIST>`],
         `macrofort/list`(
           `macrofort/nextbreak`(until_list,'isbreak','isnext',
                                 breaklabel,nextlabel)),
         ['auto_comment',` `],
         ['auto_comment',`UNTIL LOOP TERMINATION TESTS`]];
    if (isnext) then
      unf:=[op(unf),['continuef',nextlabel]];
    fi;
    unf:=[op(unf),
         ['if_then_f',NOT(condition)],
         [['if_then_f',mxntl >= nuntil],
          [['auto_comment',` `],
           ['auto_comment',`NEW LOOP ITERATION`],
           ['gotof',slabel]],
          ['elsef'],
          [['auto_comment',` `],
           ['auto_comment',`UNTIL LOOP TERMINATION :`],
	   ['auto_comment',`BYPASSING THE MAXIMUM ITERATION NUMBER`],
           ['writem',output,[`' `.mxntl.` '`],[]]],
          ['endiff']]];
  else
    unf:=[op(unf),
         ['auto_comment',` `],
         ['auto_comment',`<UNTIL_LIST>`],
         `macrofort/list`(
           `macrofort/nextbreak`(until_list,'isbreak','isnext',
                                 breaklabel,nextlabel)),
         ['auto_comment',` `],
         ['auto_comment',`UNTIL LOOP TERMINATION TESTS`]];
    if (isnext) then
      unf:=[op(unf),['continuef',nextlabel]];
    fi;
    unf:=[op(unf),
         ['if_then_f',NOT(condition)],
         [['auto_comment',` `],
          ['auto_comment',`NEW LOOP ITERATION`],
          ['gotof',slabel]]];
  fi;
  unf:=[op(unf),
       ['auto_comment',` `],
       ['auto_comment',`NORMAL UNTIL LOOP TERMINATION`],
       ['endiff']];
  if isbreak then
    unf := [op(unf),['continuef',breaklabel]];
  fi;
  unf := [op(unf),
          ['auto_comment',cat(`UNTIL LOOP END (`,`macrofort/nuntil`,`)`)]];
  `macrofort/nuntil` := `macrofort/nuntil` + 1;
  unf
end:

# [PROGRAMM,NAME,BODY_LIST]
`macrofort/macro`['programm'] := true:
`macrofort/programm` := proc(name,body_list)
  local com,dcl,l;
  com:=[];
  dcl:=[];
  l:=`macrofort/genformdcl`(body_list,'dcl','com');
  if whattype(l)='exprseq' then l:=[l] fi;
  [['auto_comment',` `],['auto_comment',cat(`MAIN PROGRAM `,name)],
   ['auto_comment',` `],
   ['programf',name],dcl,com,`macrofort/list`(l),['endf']]
end:

# [SUBROUTINEM,NAME,LIST,BODY_LIST]
`macrofort/macro`['subroutinem'] := true:
`macrofort/subroutinem` := proc(name,list,body_list)
  local com,dcl,l;
  com:=[];
  dcl:=[];
  l:=`macrofort/genformdcl`(body_list,'dcl','com');
  if whattype(l)='exprseq' then l:=[l] fi;
  [['auto_comment',` `],['auto_comment',cat(`SUBROUTINE `,name)],
   ['auto_comment',` `],
   ['subroutinef',name,list],dcl,com,`macrofort/list`(l),['endf']]
end:

# [FUNCTIONM,TYPE,NAME,LIST,BODY_LIST]
`macrofort/macro`['functionm'] := true:
`macrofort/functionm` := proc(type,name,list,body_list)
  local com,dcl,l;
  com:=[];
  dcl:=[];
  l:=`macrofort/genformdcl`(body_list,'dcl','com');
  if whattype(l)='exprseq' then l:=[l] fi;
  [['auto_comment',` `],['auto_comment',cat(`FUNCTION `,name)],
   ['auto_comment',` `],
   ['functionf',type,name,list],dcl,com,`macrofort/list`(l),['endf']]
end:

# [COMMONM,NAME,LIST]
# [DECLAREM,NAME,LIST]

# [READM,UNIT,FORMAT_LIST,VAR_LIST]
`macrofort/macro`['readm'] := true:
`macrofort/readm` := proc(unit,format_list,var_list)
global `macrofort/nflabel`;
  `macrofort/nflabel` := `macrofort/nflabel` + 1;
  [['readf',unit,2000 + `macrofort/nflabel`,var_list],
   ['formatf',2000 + `macrofort/nflabel`,format_list]]
end:

# [WRITEM,UNIT,FORMAT_LIST,VAR_LIST]
`macrofort/macro`['writem'] := true:
`macrofort/writem` := proc(unit,format_list,var_list)
global `macrofort/nflabel`;
  `macrofort/nflabel` := `macrofort/nflabel` + 1;
  [['writef',unit,2000 + `macrofort/nflabel`,var_list],
   ['formatf',2000 + `macrofort/nflabel`,format_list]]
end:

# [OPENM,UNIT,FILE,STATUS,BODY_LIST]
`macrofort/macro`['openm'] := true:
`macrofort/openm` := proc(unit,file,status,body_list)
  [['openf',unit,file,status],body_list,['closef',unit]]
end:

#*************************************************************************
# INIT_GENFOR() used to initialize global variables before every
#    new Fortran code generation
#*************************************************************************

init_genfor := proc()
global `macrofort/nlab`,`macrofort/nflabel`,`macrofort/nlabel`,
       `macrofort/nwhile`,`macrofort/nuntil`,`macrofort/flabel`,
       `macrofort/label`,comment,input,output,optimized,precision;
  `macrofort/nlab` := 0;
  `macrofort/nflabel` := -1;
  `macrofort/nlabel` := -1;
  `macrofort/nwhile` := 1;
  `macrofort/nuntil` := 1;
  `macrofort/flabel` := '`macrofort/flabel`';
  `macrofort/label` := '`macrofort/label`';
  comment := true;
  input := 5;
  output := 6;
  optimized := false;
  precision := 'single';
  NULL
end:

#****************************************************************************
#
#      m_fortran function (modification of MAPLE Fortran function)
#
#****************************************************************************

m_fortran := proc(x) local statseq;
        if type(x,'boolean') then statseq := ['m_boolean'(x)] 
        elif optimized then statseq := [readlib('optimize')(x)]
	elif type(x,'list'('name'='algebraic')) then statseq := x
	elif type(x,'array') and type(x,'name')
	then statseq := [`m_fortran/arrayexpand`(x)]
	elif type(x,'algebraic') then statseq := [x]
	else ERROR(`invalid argument`,x)
	fi;
        `m_fortran/statement`(statseq); 
	RETURN()
end:

m_fortrans := proc(x) local statseq;
global _istr;
  _istr:=true;
        if type(x,'boolean') then statseq := ['m_boolean'(x)] 
	elif type(x,'list'('name'='algebraic')) then statseq := x
	elif type(x,'array') and type(x,'name')
	then statseq := [x]
	elif type(x,'algebraic') then statseq := [x]
	else ERROR(`invalid argument`,x)
	fi;
        `m_fortran/statement`(statseq);
  _istr:='_istr';
  substring(_buffer,6..length(_buffer));
end:

`m_fortran/writeln` := proc()
  if _istr <> true then printf(`%s\n`,_buffer) fi;
end:

`m_fortran/write` := proc(e) local l,d;
global _buffer;
   if _istr = true then
     _buffer:=cat(_buffer,e); 
   else
	l := length(e);
	d := length(_buffer) + l - 72;
	if d <= 0 then _buffer := cat(_buffer,e); RETURN() fi;
	_buffer := cat(_buffer,substring(e,1..l-d));
	printf(`%s\n`,_buffer);
	_buffer := cat(`     +`,substring(e,l-d+1..l))
  fi
end:

`m_fortran/arrayexpand` := proc(A)
option `Copyright 1989 by the University of Waterloo`;
	if not (type(A,'name') and type(A,'array')) then RETURN(A) fi;
	op( map( proc(x,A) local dummy;
		subs( dummy=op(x), 'A[dummy]' ) = A[op(x)] end,
		[indices(A)], A ) )
end:

`m_fortran/statement` := proc(s) local k;
global _buffer;
option `Copyright 1989 by the University of Waterloo`;

	for k in s do
		if _istr <> true then _buffer := `      ` 
                else _buffer:=`ISTR ` fi;
                if type(k,'function') and op(0,k)='m_boolean' then
		  `m_fortran/expression`(op(1,k))
		elif type(k,'algebraic') then `m_fortran/expression`(k)
		elif type(k,'equation') then
			`m_fortran/expression`(op(1,k));
			`m_fortran/write`(` = `); 
			`m_fortran/expression`(op(2,k))
		else ERROR(`not implemented yet`)
		fi;
		`m_fortran/writeln`()
	od

end:

`m_fortran/expression` := proc(e) local k,p,s;
option `Copyright 1989 by the University of Waterloo`;
	
if type(e,'integer') then `m_fortran/write`(`m_fortran/integer`(e))
elif (e = Pi or e = gamma) then `m_fortran/write`(`m_fortran/float`(evalf(e)))
elif type(e,'float') then `m_fortran/write`(`m_fortran/float`(evalf(e)))

elif (e = true) then `m_fortran/write`(`.true.`)
elif (e = false) then `m_fortran/write`(`.false.`)

elif type(e,'string') then `m_fortran/write`(e)

elif type(e,`*`) then

	if op(1,e) + 1 = 0 then
		`m_fortran/write`(`-`);
		p := nops(e) = 2 and `m_fortran/precedence`(op(2,e)) <= 50;
		if p then `m_fortran/write`(`(`) fi;
		`m_fortran/expression`(subsop(1=1,e));
		if p then `m_fortran/write`(`)`) fi;
		RETURN()
	elif type(op(1,e),'fraction') and
		(op(1,op(1,e)) = 1 or op(1,op(1,e)) = -1) then
		if op(1,e) < 0 then `m_fortran/write`(`-`) fi;
		p := nops(e) = 2 and `m_fortran/precedence`(op(2,e)) <= 50;
		if p then `m_fortran/write`(`(`) fi;
		`m_fortran/expression`(subsop(1=1,e));
		if p then `m_fortran/write`(`)`) fi;
		`m_fortran/write`(`/`.(op(2,op(1,e))));
		RETURN()
	fi;

	p := `m_fortran/precedence`(e,1);
	if p then `m_fortran/write`(`(`) fi;
	`m_fortran/expression`(op(1,e));
	if p then `m_fortran/write`(`)`) fi;
	for k from 2 to nops(e) do
		if type(op(k,e),`^`) and type(op(2,op(k,e)),'numeric')
			and sign(op(2,op(k,e))) = -1 
		then s := op(1,op(k,e))^(-op(2,op(k,e))); 
                  `m_fortran/write`(`/`)
		else s := op(k,e); `m_fortran/write`(`*`)
		fi;
		p := `m_fortran/precedence`(s) < `m_fortran/precedence`(e);
		if p then `m_fortran/write`(`(`) fi;
		`m_fortran/expression`(s);
		if p then `m_fortran/write`(`)`) fi;
	od

elif type(e,`^`) then

	if type(op(2,e),'rational') and op(2,e) < 0 then
		p := `m_fortran/precedence`(op(1,e));
		`m_fortran/write`(`1/`);
		if p < 70 then `m_fortran/write`(`(`) fi;
		`m_fortran/expression`(op(1,e)^(-op(2,e)));
		if p < 70 then `m_fortran/write`(`)`) fi;
	elif type(op(1,e),'string') and type(op(2,e),'integer') then
		`m_fortran/write`( cat(op(1,e),`**`,op(2,e)) )
	elif type(op(2,e),'fraction') and op(2,op(2,e))=2 then
		s := op(1,e)^op(1,op(2,e));
		`m_fortran/expression`('sqrt'(s))
	else	p := `m_fortran/precedence`(e,1);
		if p then `m_fortran/write`(`(`) fi;
		`m_fortran/expression`(op(1,e));
		if p then `m_fortran/write`(`)`) fi;
		`m_fortran/write`(`**`);
		p := `m_fortran/precedence`(e,2);
		if p then `m_fortran/write`(`(`) fi;
		`m_fortran/expression`(op(2,e));
		if p then `m_fortran/write`(`)`) fi;
	fi

elif type(e,`+`) then
	p := `m_fortran/precedence`(e,1);
	if p then `m_fortran/write`(`(`) fi;
	`m_fortran/expression`(op(1,e));
	if p then `m_fortran/write`(`)`) fi;
	for k from 2 to nops(e) do
		if not (type(op(k,e),'numeric') and op(k,e) < 0 or
			type(op(k,e),`*`) and type(op(1,op(k,e)),'numeric')
			and sign(op(1,op(k,e))) = -1)
		then `m_fortran/write`(`+`) fi;
		p := `m_fortran/precedence`(e,k);
		if p then `m_fortran/write`(`(`) fi;
		`m_fortran/expression`(op(k,e));
		if p then `m_fortran/write`(`)`) fi;
	od

elif type(e,'function') then
        `m_fortran/function`(e);

elif type(e,'indexed') then
	`m_fortran/expression`(op(0,e));
	`m_fortran/write`(`(`);
	`m_fortran/expression`(op(1,e));
	for k from 2 to nops(e) do
		`m_fortran/write`(`,`);
		`m_fortran/expression`(op(k,e))
	od;
	`m_fortran/write`(`)`)

elif type(e,'fraction') then
	`m_fortran/expression`(evalf(op(1,e)));
	`m_fortran/write`('`/`');
	`m_fortran/expression`(evalf(op(2,e)));

elif type(e,`not`) then
	`m_fortran/write`(` .not. `);
	p := `m_fortran/precedence`(e,1);
	if p then `m_fortran/write`(`(`) fi;
	`m_fortran/expression`(op(1,e));
	if p then `m_fortran/write`(`)`) fi

elif type(e,'boolean') then
	p := `m_fortran/precedence`(e,1);
	if p then `m_fortran/write`(`(`) fi;
	`m_fortran/expression`(op(1,e));
	if p then `m_fortran/write`(`)`) fi;
	if type(e,`or`) then `m_fortran/write`(` .or. `)
	elif type(e,`and`) then `m_fortran/write`(` .and. `)
	elif type(e,`=`) then `m_fortran/write`(` .eq. `)
	elif type(e,`<`) then `m_fortran/write`(` .lt. `)
	elif type(e,`<=`) then `m_fortran/write`(` .le. `)
	elif type(e,`<>`) then `m_fortran/write`(` .ne. `)
	fi;
	p := `m_fortran/precedence`(e,2);
	if p then `m_fortran/write`(`(`) fi;
	`m_fortran/expression`(op(2,e));
	if p then `m_fortran/write`(`)`) fi

else ERROR(`unable to translate`)
fi

end:


`m_fortran/precedence` := proc(e,k)
option `Copyright 1989 by the University of Waterloo`;

if nargs = 1 then
	if type(e,'string') then 99
	elif type(e,'integer') and e > 0 then 99
	elif type(e,`*`) then 70
	elif type(e,`+`) then 50
	elif type(e,`^`) then 80
	elif type(e,'float') and e > 0 then 99
	elif type(e,'function') or type(e,'indexed') then 99
	elif type(e,'rational') then 70
	elif type(e,'integer') then 60
	elif type(e,'float') then 60
	elif type(e,`=`) or type(e,`<`) or type(e,`<=`) or type(e,`<>`) then 40
	elif type(e,`not`) then 30
	elif type(e,`and`) then 20
	elif type(e,`or`) then 10
	else 0
	fi
elif type(e,`*`) and k = 1 and type(op(1,e),'numeric') then false
elif type(e,`^`) and k = 1 and type(op(1,e),`^`) then true
else `m_fortran/precedence`(e) > `m_fortran/precedence`(op(k,e))
fi

end:


`m_fortran/float` := proc(f) local mantissa,exponent,letter,quotient,prefix;
option `Copyright 1989 by the University of Waterloo`;
	if f = 0
	then mantissa := 0; exponent := 0
	else mantissa := op(1,f); exponent := op(2,f) fi;
	if exponent = 0 and mantissa < 1000000 then
		if precision <> 'double' then 
                  RETURN( cat(mantissa,`.0`) ) fi;
		RETURN( cat(mantissa,`.D0`) )
	fi;
	if precision = 'double' then letter := 'D' 
        else letter := 'E' fi;
	if mantissa < 0 then
		prefix := `-0.`;
		mantissa := -mantissa
	else prefix := `0.`
	fi;
	while irem(mantissa,10,'quotient') = 0 do
		mantissa := quotient;
		exponent := exponent+1
	od;
	exponent := exponent+length(mantissa);
	cat(prefix,mantissa,letter,exponent)
end:

`m_fortran/integer` := proc(e)
  if length(e) > 8 then
    `m_fortran/float`(evalf(e))
  else ``.e
  fi
end:

`m_fortran/function` := proc(e)
  local f,k,n;
  f:=op(0,e); n:=nops(e);
  if f = 'abs' then 
    if n <> 1 then ERROR(f,`function must have one and only one argument`); 
    fi;
    `m_fortran/function_IR1`(`m_fortran/function_name`[f],op(1,e));
  elif member(f,{'arccos','arcsin','cos','cosh','exp','ln','sin','sinh',
                 'sqrt','tan','tanh'}) then
    if n <> 1 then ERROR(f,`function must have one and only one argument`); 
    fi;
    `m_fortran/function_R1`(`m_fortran/function_name`[f],op(1,e));
  elif f = 'arctan' then
    if n = 1 then `m_fortran/function_R1`('atan',op(1,e));
    elif n = 2 then `m_fortran/function_R2`('atan2',op(1,e),op(2,e));
    else ERROR(f,`function must have one or two arguments`);
    fi
  else
    `m_fortran/expression`(f);
    `m_fortran/write`(`(`);
     if nops(e) > 0 then
       `m_fortran/expression`(op(1,e));
	for k from 2 to nops(e) do
	  `m_fortran/write`(`,`);
	  `m_fortran/expression`(op(k,e));
	od;
     fi;
     `m_fortran/write`(`)`);
  fi;
end:

`m_fortran/function_IR1` := proc(f,a)
# 1 integer or real argument
  `m_fortran/write`(f);
  `m_fortran/write`(`(`);
  `m_fortran/expression`(a);
  `m_fortran/write`(`)`);
end:

`m_fortran/function_R1` := proc(f,a)
# 1 real argument
  local va;
  `m_fortran/write`(f);
  `m_fortran/write`(`(`);
   va:=a;
   if type(a,'numeric') then
    if a = 0 then 
      if precision = 'single' then va:=`0.0E0` else va:=`0.0D0` fi;
    else va:=evalf(a) fi;
  fi;
  `m_fortran/expression`(va);
  `m_fortran/write`(`)`);
end:

`m_fortran/function_R2` := proc(f,a,b)
# 2 real arguments
  local v;
  `m_fortran/write`(f);
  `m_fortran/write`(`(`);
  v:=a;
  if type(a,'numeric') then
    if a = 0 then 
      if precision = 'single' then v:=`0.0E0` else v:=`0.0D0` fi;
    else v:=evalf(a) fi;
  fi;
  `m_fortran/expression`(v);
  `m_fortran/write`(`,`);
  v:=b;
  if type(b,numeric) then
    if b = 0 then 
      if precision = 'single' then v:=`0.0E0` else v:=`0.0D0` fi;
    else v:=evalf(b) fi;
  fi;
  `m_fortran/expression`(v);
  `m_fortran/write`(`)`);
end:

`m_fortran/function_name`['abs']:='abs':
`m_fortran/function_name`['arccos']:='acos':
`m_fortran/function_name`['arcsin']:='asin':
`m_fortran/function_name`['cos']:='cos':
`m_fortran/function_name`['cosh']:='cosh':
`m_fortran/function_name`['exp']:='exp':
`m_fortran/function_name`['ln']:='log':
`m_fortran/function_name`['sin']:='sin':
`m_fortran/function_name`['sinh']:='sinh':
`m_fortran/function_name`['sqrt']:='sqrt':
`m_fortran/function_name`['tan']:='tan':
`m_fortran/function_name`['tanh']:='tanh':

#****************************************************************************
#
#          CONVERT(.,STRING) new convertion (better than NAME)
#
#***************************************************************************

`convert/string` := proc(x)
if nargs=0 then ``
elif nargs>1 then ``.(`convert/string`(x)).`,`.(`convert/string`(
	args[2..nargs] ) )
else
	if type(x,'string') then x
	elif type(x,'integer') then ``.x
	elif type(x,'rational') then
		``.(`convert/string`(op(1,x))).`/`.(`convert/string`(op(2,x)))
	elif type(x,'float') then
        ``.(_convertnamefloat(x))
	elif type(x,`^`) then `_convertstring^`(x)
	elif type(x,`*`) then `_convertstring*`(x)
	elif type(x,`+`) then `_convertstring+`(x)
	elif type(x,'list') then `[`.(`convert/string`(op(x))).`]`
	elif type(x,'set') then `{`.(`convert/string`(op(x))).`}`
	elif type(x,'indexed') then ``.(`convert/string`(op(0,x))).
		`[`.(`convert/string`(op(x))).`]`
	elif type(x,'function') then ``.(op(0,x)).`(`.(`convert/string`(op(x))).`)`
	elif type(x,`=`) then ``.(`convert/string`(op(1,x))).`=`.
		(`convert/string`(op(2,x)))
	elif type(x,`..`) then ``.(`convert/string`(op(1,x))).`..`.
		(`convert/string`(op(2,x)))
	elif type(x,`<`) then ``.(`convert/string`(op(1,x))).`<`.
		(`convert/string`(op(2,x)))
	elif type(x,`<=`) then ``.(`convert/string`(op(1,x))).`<=`.
		(`convert/string`(op(2,x)))
	elif type(x,`<>`) then ``.(`convert/string`(op(1,x))).`<>`.
		(`convert/string`(op(2,x)))
	elif type(x,`and`) then `(`.(`convert/string`(op(1,x))).` and `.
		(`convert/string`(op(2,x))).`)`
	elif type(x,`or`) then `(`.(`convert/string`(op(1,x))).` or `.
		(`convert/string`(op(2,x))).`)`
	elif type(x,`not`) then ` not `.(`convert/string`(op(1,x)))
	elif type(x,`.`) then ``.(`convert/string`(op(1,x))).`.`.
		(`convert/string`(op(2,x)))
	else
		ERROR(`unable to convert to string`)
	fi
fi
end:
_convertnamefloat := proc( f )
local old, s;
option `Copyright 1989 by the University of Waterloo`;
old := Digits;
Digits := max( old, length( trunc(f) ) + 1 );
if f < 0 then
	s := `-`.(trunc(-f)).`.`.(_convertnamefrac(frac(-f)));
else
	s := ``.(trunc(f)).`.`.(_convertnamefrac(frac(f)));
fi;
Digits := old;
RETURN( s );
end:
_convertnamefrac := proc(r)
option `Copyright 1989 by the University of Waterloo`;
if r = 0 then ``
elif r >= 0.1 then ``.(trunc(10*r)).(_convertnamefrac(10*r-trunc(10*r)))
else `0`. (_convertnamefrac(10*r)) fi
end:
`_convertstring^` := proc(x)
	local p;
	p := op(2,x);
	if type(p,'string') or (type(p,'integer') and p>0) then
	  ``.(`convert/string`(op(1,x))).`^`.p
	else ``.(`convert/string`(op(1,x))).`^(`.(`convert/string`(p)).`)`
	fi
end:
`_convertstring*` := proc(x)
	local i,t,p;
	t := `(`.(`convert/string`(op(1,x)));
	if nops(x) = 2 then
		p := op(2,x);
		if type(p,`^`) and op(2,p) = -1 then
		  ``.(t).`/`.(`convert/string`(op(1,p))).`)`
		else ``.(t).`*`.(`convert/string`(p)).`)`
		fi;
	else for i from 2 to nops(x) do
		t := ``.(t).`*`.(`convert/string`(op(i,x)))
	      od;
	      ``.t.`)`
	fi
end:
`_convertstring+` := proc(x)
	local i,t;
	t := `(`.(`convert/string`(op(1,x)));
	for i from 2 to nops(x) do
		t := ``.(t).`+`.(`convert/string`(op(i,x)))
	od;
	``.t.`)`
end:

#****************************************************************************

`help/text/macrofort` := TEXT(
`PACKAGE: Macrofort - A system for the complete generation of Fortran 77 code`,
`                     (programs, subroutines and functions) without dealing`,
`                     with label numbering.`,
`   `,
`FUNCTIONS:   `,
`   init_genfor - initialization routine`,
`   genfor      - Fortran 77 code generation routine`,
`   `,
`CALLING SEQUENCES:`,
`   init_genfor()`,
`   genfor(flist)`,
`   `,
`PARAMETERS:`,
`   flist - a list describing the Fortran program`,
`   `,
`SYNOPSIS:   `,
`- The init_genfor procedure initializes the generator. The genfor procedure`,
`  generates the Fortran program. The input flist is a list describing the`,
`  Fortran program in the following way: each element of the list corresponds `,
`  to either a single Fortran statement or several Fortran statements.`,
`  The syntax of a Fortran statement or a macro Fortran statement in `,
`  Macrofort is a list where the first element is a keyword describing`,
`  the statement, and the remaining elements are the relevant arguments.`,
`  A keyword is made from the Fortran instruction name (when it exists)`,
`  with a f at its end for a single Fortran statement or a m at its end for `,
`  a macro Fortran statement. These keywords correspond to what we call a `,
`  Macrofort single instruction or a Macrofort macro instruction. In fact it`,
`  is much more efficient to use macro instructions when it is possible (the`,
`  labels are automatically generated).`,
`   `,
`Macrofort single instructions:              Fortran code generated:`,
`==============================================================================`,
`[callf,<name>,<list>]     generates        call <name>(<list>)`,
`[closef,<unit>]           generates        close(<unit>)`,
`[commentf,<string>]       generates  c     <string>`,
`[commonf,<name>,<list>]   generates        common /<name>/ <list>`,
`[continuef,<lbl>]         generates  <lbl> continue`,
`[declaref,<type>,<list>]  generates        <type> <list>`,
`[dof,<lbl>,<idx>,<start>,`,
`     <end>]               generates        do <lbl>,<idx>=<start>,<end>`,
`[dof,<lbl>,<idx>,<start>,`,
`     <end>,<step>]        generates        do <lbl>,<idx>=<start>,<end>,<step>`,
`[elsef]                   generates        else`,
`[endf]                    generates        end`,
`[endiff]                  generates        endif`,
`[equalf,<var>,<expr>]     generates        <var>=<expr>`,
`[equalf,[[<var1>,<expr1>],[<var2>,<expr2>],...]]`,
`                          generates        <var1>=<expr1>`,
`                                           <var2>=<expr2>`,
`                                            ...`,
`[formatf,<lbl>,<list>]     generates  <lbl> format(<list>)`,
`[functionf,<type>,<name>,<list>] `,
`                           generates        <type> function <name>(<list>)`,
`[gotof,<lbl>]              generates        goto <lbl>`,
`[if_goto_f,<cond>,<lbl>]   generates        if (<cond>) goto <lbl>`,
`[if_then_f,<cond>]         generates        if (<cond>) then`,
`[openf,<unit>,<file>,<status>]`,
`                           generates        open (unit=<unit>,file=,'<file>',`,
`                                                  status=,'<status>')`,
`[parameterf,<list>]        generates        parameter(<list>)`,
`[programf,<name>]          generates        program <name>`,
`[readf,<file>,<lbl>,<list>] `,
`                           generates        read(<file>,<lbl>) <list>`,
`[returnf]                  generates        return`,
`[subroutinef,<name>,<list>] `,
`                           generates        subroutine <name>(<list>)`,
`[writef,<file>,<lbl>,<list>] `,
`                           generates        write(<file>,<lbl>) <list> `,
`   `,
`Macrofort macro instructions:               Fortran code generated:`,
`===============================================================================`,
`[dom,<idx>,<start>,<end>,`,
`     <step>,<do_list>]     generates        do <lbl>,<idx>=<start>,<end>,<step>`,
`                                              <do_list>`,
`                                      <lbl> continue`,
`[dom,<idx>,<start>,<end>,`,
`     <do_list>]            generates        do <lbl>,<idx>=<start>,<end>`,
`                                              <do_list>`,
`                                      <lbl> continue`,
`[functionm,<type>,<name>,<list>,`,
`           <body_list>]    generates        <type> function <name>(<list>)`,
`                                              <body_list>`,
`                                            end`,
`[if_then_else_m,<cond>,<then_list>,`,
`       <else_list>]        generates        if <cond> then`,
`                                              <then_list>`,
`                                            else`,
`                                              <else_list>`,
`                                            endif`,
`[if_then_m,<cond>,<then_list>]`,
`                           generates        if <cond> then`,
`                                              <then_list>`,
`                                            endif`,
`[programm,<name>,<body_list>] `,
`                           generates        program <name>`,
`                                              <body_list>`,
`                                            end`,
`[openm,<unit>,<file>,<status>,`,
`       <body_list>]        generates        open(unit=<unit>,file=,'<file>',`,
`                                                 status=,'<status>')`,
`                                              <body_list>`,
`                                            close(<unit>)`,
`[readm,<file>,<format_list>,`,
`       <list>]             generates        read(<file>,<lbl>)<list>`,
`                                      <lbl> format(<format_list>)`,
`[subroutinem,<name>,<list>,`,
`       <body_list>]        generates        subroutine <name>(<list>)`,
`                                              <body_list>`,
`                                            end`,
`[writem,<file>,<format_list>,`,
`        <list>]            generates        write(<file>,<lbl>)<list>`,
`                                      <lbl> format(<format_list>)`,
`   `,
`[breakm] and [nextm] generate BREAK and NEXT statements in loops.`,
`   `,
`  The macro instructions commonm and declarem are the same as commonf`,
`  and declaref, but you can put them anywhere in the list describing the`,
`  program.   `,
`   `,
`  The two following macro instructions correspond to while and until loops:`,
`  [whilem,<cond>,<init_list>,<while_list>,<while_max>]`,
`  [untilm,<cond>,<init_list>,<until_list>,<until_max>]`,
`  where <while_max> and <until_max> denote the maximum number of iterations`,
`  the loop will execute. They are optional.`,
`   `,
`  The macro instruction [matrixm,<variable>,<matrix>] makes assignment of`,
`  the elements of a matrix (see examples).`,
`   `,
`  <cond> is a condition you have to write in MAPLE syntax, but when you`,
`  introduce the logical operators not, and and or into the condition, you`,
`  have to use the names NOT, AND and OR in a functional notation, for instance`,
`  [if_then_f,OR(a=b,NOT(c<d))].`,
`   `,
`  <do_list>, <then_list>, <else_list>, <body_list>, <init_list>, <until_list> `,
`  and <while_list> must be list describing Fortran statements with Macrofort`,
`  syntax.   `,
`    `,
`- By default the output is sent to standard output. To direct the output to`,
`  the file foo, use writefile(foo) before generating the code.`,
`   `,
`- If the global variable optimized is set to true (false by default), common `,
`  subexpression optimization is performed. The result is a sequence of `,
`  assignment statements in which temporary values are stored in local `,
`  variables beginning with the letter t. The global names t0, t1, t2, ... are `,
`  reserved for this purpose.`,
`   `,
`- The global variable precision can be assigned either single or double`,
`  (single by default) for single or double precision respectively.`,
`   `,
`- The global variable input is bound to the logical unit number of standard`,
`  input (5 by default) and the global variable output is bound to the logical `,
`  unit number of standard output (6 by default).`,
`   `,
`- If the global variable comment is set to false (true by default), Macrofort`,
`  no longer generates automatically Fortran comments.`,
`   `,
`- A complete documentation can be found as a LaTeX file macrofort.tex in the`,
`  share directory.`,
`   `,
`EXAMPLES:   `,
`> genfor([equalf,a,expand((1+sin(x))^10)]);`,
`      a = 1+10*sin(x)+45*sin(x)**2+120*sin(x)**3+210*sin(x)**4+252*sin(x`,
`     +)**5+210*sin(x)**6+120*sin(x)**7+45*sin(x)**8+10*sin(x)**9+sin(x)*`,
`     +*10   `,
`> l:=[[equalf,n,i+j],[commonm,toto,[ia]],[equalf,foo,ia^n]]:`,
`> genfor([functionm,integer,foo,[i,j],l]);`,
`c   `,
`c     FUNCTION foo`,
`c   `,
`      integer function foo(i,j)`,
`        common/toto/ia`,
`        n = i+j`,
`        foo = ia**n`,
`      end   `,
`> mat:=array(1..2,1..2):`,
`> for i to 2 do for j to 2 do mat[i,j]:=(x.i)^(j-1) od od:`,
`> genfor([matrixm,m,mat]);`,
`      m(1,1) = 1`,
`      m(1,2) = x1`,
`      m(2,1) = 1`,
`      m(2,2) = x2`,
`> genfor([readm,input,[``2x,e14.7``],[x,y,z(2)]]);`,
`      read(5,2000) x,y,z(2.0)`,
` 2000 format(2x,e14.7)`,
`> genfor([whilem,abs(a)>eps,[equalf,a,big],[equalf,a,a/2.0],1000]);`,
`c   `,
`c     WHILE  (eps<abs(a)) DO <WHILE_LIST> (1)`,
`c   `,
`c     WHILE LOOP INITIALIZATION`,
`          maxwhile1 = 1000`,
`          nwhile1 = 0`,
`        a = big`,
`c   `,
`c     WHILE LOOP BEGINNING`,
` 1000 continue`,
`c   `,
`c     WHILE LOOP TERMINATION TESTS`,
`      if (eps.lt.abs(a)) then`,
`        if (nwhile1.le.maxwhile1) then`,
`c   `,
`c         NEW LOOP ITERATION`,
`          nwhile1 = nwhile1+1`,
`c   `,
`c         <WHILE_LIST>`,
`          a = 0.5E0*a`,
`        goto 1000`,
`        else`,
`c   `,
`c         WHILE LOOP TERMINATION :`,
`c         BYPASSING THE MAXIMUM ITERATION NUMBER`,
`          write(6,2000)`,
` 2000     format(' maxwhile1 ')`,
`        endif`,
`c   `,
`c     NORMAL WHILE LOOP TERMINATION`,
`      endif`,
`c     WHILE LOOP END (1)`,
`   `,
`SEE ALSO:  fortran, optimize`
):

init_genfor():
#save `macrofor.m`:
#quit
