-module(semantic_forms).
-author('Chanrotha Sisowath').
-behaviour(application).

-export([start/0, start/1, start/2, stop/1, new/2, stepwizard/3, translatable_strings/1, q/1]).
-export([init/2,validation/1,init_fields/2]).

-export([         
         linked_select/6,
         select/6, 
         search_select/6,          
         multi_select/6,          
         multi_search_select/6,          
         dynamic/6,
         date/6,date_range/6,
         number/6,hidden/6,
         currency/6,integer/6,
         float/6,
         price/6,
         file/6,         
         textarea/6,
         checkbox/6, 
         radio_checkbox/6,
         string/6,readonly/6,
         mobile/6,
         password/6,
         repassword/6,confirm_password/6,
         email/6,
         search_menu/6,
         render_options/3,
         prompt/1,
         set_values/2,
         set_values/3,
         set_dropdown_values/2,
         set_dropdown_value/3,
         formBtns/1
         ]).

-include_lib("nitro/include/nitro.hrl").
-include_lib("n2o/include/wf.hrl").
-include("meta.hrl").
-define(SELECT, [select,search_select,linked_select,multi_select,multi_search_select]).

start()    -> ok.
start(_)   -> ok.
start(_,_) -> {ok,self()}.
stop(_)    -> ok.

q(#document{name=Name,fields=Fields,sections=Sections}=Doc) -> 
    Secs = [{S#sec.id,S#sec.m}||S<-Sections],
    {Attrs, Count} = lists:foldl(fun(#field{}=X,{Acc,Idx}) -> 
                       Field =wf:atom([Name, X#field.name]), 
                       Val = wf:q(Field),
                        case proplists:get_value(X#field.sec, Secs) of
                          undefined -> {Acc#{X#field.name => Val}, Idx};
                          M         -> {add_prop(M, Acc, [{X#field.name, Val}]), Idx}
                        end;
                  (#cat{}=Cat,{Acc,Idx}) -> 
                       Cats = lists:foldl(fun(#field{}=Y,CatAcc) -> 
                                                  CatField =wf:atom([Name, Y#field.name]), 
                                                  CatVal = wf:q(CatField),
                                                  CatAcc#{ n(Cat#cat.name,Idx,Y#field.name) => CatVal} end, #{}, Cat#cat.fields),
                       case proplists:get_value(Cat#cat.sec, Secs) of
                          undefined -> {Acc#{Cat#cat.name => Cats},Idx+1};
                          M         -> {add_prop(M, Acc, [{Cat#cat.name, maps:to_list(Cats)}]), Idx+1}
                       end
               end, {#{},1}, Fields), Attrs.

%%FIXME
add_prop(M, Maps, Prop) ->
  case maps:get(M, Maps, undefined) of undefined -> Maps#{M => Prop};
    Old -> New = Old ++ Prop, Maps#{M => New} end.


n(C,I,FN) -> 
 A = string:tokens(atom_to_list(C),"_") ++ [integer_to_list(I)],
 B = string:tokens(atom_to_list(FN),"_"),
 wf:atom(B -- A).


% n(N,I,F) -> n(N,F,n1(N,F)).
% n(_,F,{}) -> F;
% n(_,_,F) -> F.

% n1(Cat,Idx,Field) when is_atom(Name)->n1(atom_to_list(Name),Field);
% n1(Cat,Idx,Field) when is_atom(Field)->n1(Name,atom_to_list(Field));
% n1(N,F)->
%  case string:tokens(F, "_") of
%    [N,_|R] -> wf:atom(R);
%    _ -> {} end.

new(Document,Object) ->  
    StepWizard = Document#document.steps,         
    Sections   = Document#document.sections,
    Fields     = Document#document.fields,
    Name       = Document#document.name,
    Buttons    = Document#document.buttons,
    Lang       = Document#document.lang, 
    Default    = Document#document.default,
    Class      = Document#document.class,
    FormClass  = Document#document.fClass,
    Action     = case Document#document.action of undefined -> wf:atom(Name); A -> A end,
    %Style      = Document#document.style,
    Translator = proplists:get_value(translator, Default, fun(_,X) -> X end),
    InitActions= ?MODULE:init(Document, Object),             

    #form{class=FormClass, id=wf:atom([Name]), method=post, action=Action, actions=InitActions, body=[
      stepwizard(Name, StepWizard, {Translator, Lang}),
      render_fields(Name, Fields, Sections, Object, proplists:get_value(fields, Default), {Translator, Lang}),
      case proplists:get_value(render_buttons, Default, true) of true ->
      formBtns(Name,Fields,Buttons,Translator, Lang); _ -> [] end]}.


formBtns(Document) ->
      Name       = Document#document.name,
      Buttons    = Document#document.buttons,
      Default    = Document#document.default,
      Translator = proplists:get_value(translator, Default, fun(_,X) -> X end),
      Fields     = Document#document.fields,
      Lang       = Document#document.lang, 
      formBtns(Name, Fields, Buttons, Translator, Lang).

formBtns(Name,Fields, Buttons,Translator, Lang) ->
      lists:foldr(fun(#but{}=But,Acc) ->
                      case But#but.sources of
                          [fields] -> 
                              Sources = sources(Name,Fields),
                              [#button{id=But#but.id,class=But#but.class,postback=But#but.postback,body=Translator(Lang,But#but.title),
                                        source=Sources}|Acc];
                          _ -> [#button{id=But#but.id,class=But#but.class,postback=But#but.postback,body=Translator(Lang,But#but.title),
                                        source=[wf:atom([Name,S])||S<-But#but.sources],onclick=But#but.onclick}|Acc] 
                      end end,[],Buttons).

set_values(#document{}=Document, Maps) ->
  Name = Document#document.name,

  Fields = [Y||Y<-lists:flatten([begin case X of 
                #field{} -> X;
                #cat{} ->X#cat.fields
              end end || X<-Document#document.fields]), not lists:member(Y#field.type, [select,multi_select,multi_search_select])],

  New = lists:foldr(fun(X,Acc) -> 
          Key = wf:atom([Name, X#field.name]), Val = maps:get(Key,Maps), Acc#{Key => Val} end, #{}, Fields),
  
  Values = jsone:encode(maps:to_list(New)),
  Action = wf:f("$('#~s').form('set values', ~s);", [Name, Values]),
  Action1 = case catch set_dropdown_values(Document, Maps) of {'EXIT', Err} -> io:format("Error ~p~n", [Err]), []; Else -> Else end,
  io:format("set values:~p~n", [Action]),
  io:format("set dropdown values:~p~n", [Action1]),
  wf:wire(Action++Action1).

set_values(#document{}=Document, Key, Value) ->
  Name = Document#document.name,
  Action = wf:f("$('#~s').form('set value', ~s, \"~s\");", [Name, Key, Value]),
  wf:wire(Action).

set_dropdown_values(#document{}=D,#{}=Maps) ->
  Name = D#document.name,

  Fields = [Y||Y<-lists:flatten([begin case X of 
                #field{} -> X;
                #cat{} ->X#cat.fields
              end end || X<-D#document.fields]), lists:member(Y#field.type, [select,multi_select,multi_search_select])],

  lists:foldr(fun(X,Acc) -> 
          Field =wf:atom([Name, X#field.name]), 
          Val = maps:get(Field,Maps),
          wf:info(?MODULE, "Field ~p:~p~n",[Field,Val]),
          Acc++set_dropdown_value(X#field.type,Field,Val) end, "", Fields).

set_dropdown_value(Type, Key, Value) when is_atom(Key)->
 set_dropdown_value(Type, atom_to_list(Key), Value);

set_dropdown_value(multi_search_select, Key, Value) -> set_dropdown_value(multi_select, Key, Value);
set_dropdown_value(multi_select, Key, Value) ->wf:f("$('#~s').dropdown('set selected', \"~s\");",[Key,Value]);
set_dropdown_value(select, Key, Value) ->wf:f("$('#~s_dropdown').dropdown('set selected', \"~s\");",[Key,Value]).

sources(Name, Fields) ->
    sources(Name, Fields, [], []).

sources(_,[],[],Acc) -> lists:reverse(lists:flatten(Acc));
sources(Name, [#cat{}=C|T], AccCat, Acc) ->
    sources(Name, T, [C|AccCat], Acc);

sources(Name, [#field{}=X|T], [], Acc) ->
    FieldName  = wf:atom([Name,X#field.name]),
    sources(Name, T, [], [FieldName|Acc]);

sources(Name, [#field{}=X|T]=Fields, AccCat, Acc) ->
    Cats = [sources(Name,C#cat.fields)||C<-AccCat],
    sources(Name, Fields, [], [lists:flatten(lists:reverse(Cats))|Acc]).

%%VALIDATION SEMANTIC FORMS
validation(#document{default=Default}=D) ->
   Fields = lists:flatten([begin case X of 
                #field{} -> X;
                #cat{} ->X#cat.fields
              end end || X<-D#document.fields]),    
   %Name = D#document.name,
   Options = proplists:get_value(options, Default, []),
   %%Opts = ",on:'blur'",
   %Opts = jsone:encode([{inline, true},{on, 'blur'}]).
   Form = validation(#document{name=Name}=D, Fields, length(Fields), ""),   
   wf:f("$('#~s').form({ fields:{~s}~s});",[Name,Form,Options]).



% options(O) -> case length(O) >= 1 of true -> options(O, length(O), ","); _ -> "".
% options([], 0, Acc) -> Acc;

% options([{K,V}|T], Idx, Acc) when Idx >1 ->
%   options(T, Acc ++ wf:f("~s:~s,",[k,v]));

% options([{K,V}|T], Idx, Acc) when Idx >1 ->
%   options(T, Acc ++ wf:f("~s:~s,",[k,v]));
  

validation(_D, [], 0, Acc) -> Acc;

validation(#document{name=Name}=Doc, [#field{}=X|T], Idx, Acc) when Idx > 1 ->
  N = wf:atom([Name,X#field.name]),
  Rules = rules(Doc, N, X#field.rules),
  case X#field.optional of true ->  
  V = case Rules of [] ->  wf:f("~s:{identifier:'~s',optional:true},", [N,N]);
    _-> wf:f("~s:{identifier:'~s',optional:true,rules:[~s]},", [N,N, Rules]) end,
      wf:info(?MODULE,"~p:~s~n",[Idx,V]), validation(Doc, T, Idx-1, Acc ++ V);
  false -> 
    V = case Rules of [] ->  wf:f("~s:{identifier:'~s'},", [N,N]);
    _-> wf:f("~s:{identifier:'~s',rules:[~s]},", [N,N, Rules]) end,
      wf:info(?MODULE,"~p:~s~n",[Idx,V]), validation(Doc, T, Idx-1, Acc ++ V)
  end;


validation(#document{name=Name}=D, [#field{}=X|T], Idx, Acc) when Idx == 1 ->
  N = wf:atom([Name,X#field.name]),
  Rules = rules(D, N, X#field.rules),
  case X#field.optional of false -> 
  V = case Rules of [] ->  wf:f("~s:{identifier:'~s'}", [N,N]);
    _-> wf:f("~s:{identifier:'~s', rules:[~s]}", [N,N, Rules]) end,
  wf:info(?MODULE,"~p:~s~n",[Idx,V]),
  validation(D, T, Idx-1, Acc ++ V);
  true ->
   V = case Rules of [] ->  wf:f("~s:{identifier:'~s',optional:true}", [N,N]);
    _-> wf:f("~s:{identifier:'~s',optional:true,rules:[~s]}", [N,N, Rules]) end,
  wf:info(?MODULE,"~p:~s~n",[Idx,V]),
  validation(D, T, Idx-1, Acc ++ V) end.

rules(Doc, N, Rules) ->
  rules(Doc, N, Rules, length(Rules), []).

rules(Doc, N, [], 0, Acc) -> Acc;
rules(Doc, N, [X|T], Idx, Acc) when Idx > 1 -> 
  R = case catch rule(Doc, N, X) of
       {'EXIT', Err} -> io:format("rule EXIT ~p",[Err]), [];
       Else -> Else end,
  rules(Doc, N, T, Idx-1, Acc ++ R ++ ",");

rules(Doc, N, [X|T], Idx, Acc) when Idx == 1 -> 
  R = case catch rule(Doc, N, X) of
       {'EXIT', Err} -> io:format("rule EXIT ~p",[Err]), [];
       Else -> Else end,
  rules(Doc, N, T, Idx-1, Acc ++ R).

rule(_,_,[]) -> "";
rule(#document{name=DocName,lang=Lang}=Doc,Field,#rule{name=RuleName,prompt=P}=R) ->
   Translator = proplists:get_value(translator, Doc#document.default, fun(_,X) -> X end),
   Prompt = case P of [] -> Translator(Lang, prompt(RuleName)); _ -> P end,
   rule1(DocName, Field, R#rule{prompt=Prompt}).

rule1(_,_,#rule{name=email,   prompt=Prompt}) -> wf:f("{type:'email',prompt:\"~s\"}",[Prompt]);
rule1(_,_,#rule{name=url,     prompt=Prompt}) -> wf:f("{type:'url',prompt:\"~s\"}",[Prompt]);
rule1(_,_,#rule{name=number,  prompt=Prompt}) -> wf:f("{type:'number',prompt:\"~s\"}",[Prompt]);
rule1(_,_,#rule{name=decimal, prompt=Prompt}) -> wf:f("{type:'decimal',prompt:\"~s\"}",[Prompt]);
rule1(_,_,#rule{name=required,prompt=Prompt}) -> wf:f("{type:'empty',prompt:\"~s\"}",[Prompt]);
rule1(_,_,#rule{name=empty,   prompt=Prompt}) -> wf:f("{type:'empty',prompt:\"~s\"}",[Prompt]);
rule1(_,_,#rule{name=checked, prompt=Prompt}) -> wf:f("{type:'checked',prompt:\"~s\"}",[Prompt]);
rule1(_,_,#rule{name=regExp,  prompt=Prompt, 
                              pattern=Pattern}) ->  wf:f("{type:'regExp[~s]',prompt:\"~s\"}",[Pattern,Prompt]);
%%matching Field
rule1(N,_,#rule{name=match,   prompt=Prompt, 
                              target=Field}) ->
              NField = wf:atom([N,Field]), wf:f("{type:'match[~s]',prompt:\"~s\"}",[NField,Prompt]);
rule1(N,_,#rule{name=different,prompt=Prompt,
                             target=Field}) ->
              NField = wf:atom([N,Field]), wf:f("{type:'different[~s]',prompt:\"~s\"}",[NField,Prompt]);

%%payment: visa,amex,mastercard,discover,unionpay,jcb,dinersClub,maestro,laser,visaElectron
rule1(_,_,#rule{name=creditcard,prompt=Prompt,
                                pattern=Pattern}) -> 
  case Pattern of [] -> wf:f("{type:'creditCard',prompt:\"~s\"}",[Pattern,Prompt]);
                   _ -> wf:f("{type:'creditCard[~s]',prompt:\"~s\"}",[Pattern,Prompt]) end;

%%special content
rule1(_,_,#rule{name=contains, prompt=Prompt, pattern=Pattern}) -> wf:f("{type:'contains[~s]',prompt:\"~s\"}",[Pattern,Prompt]);
rule1(_,_,#rule{name=containsExactly, prompt=Prompt, pattern=Pattern}) -> wf:f("{type:'containsExactly[~s]',prompt:\"~s\"}",[Pattern,Prompt]);
rule1(_,_,#rule{name=doesntContain, prompt=Prompt, pattern=Pattern}) -> wf:f("{type:'doesntContain[~s]',prompt:\"~s\"}",[Pattern,Prompt]);
rule1(_,_,#rule{name=doesntContainExactly, prompt=Prompt, pattern=Pattern}) -> wf:f("{type:'doesntContainExactly[~s]',prompt:\"~s\"}",[Pattern,Prompt]);
rule1(_,_,#rule{name=is, prompt=Prompt, pattern=Pattern}) -> wf:f("{type:'is[~s]',prompt:\"~s\"}",[Pattern,Prompt]);
rule1(_,_,#rule{name=isExactly, prompt=Prompt, pattern=Pattern}) -> wf:f("{type:'isExactly[~s]',prompt:\"~s\"}",[Pattern,Prompt]);
rule1(_,_,#rule{name='not', prompt=Prompt, pattern=Pattern}) -> wf:f("{type:'not[~s]',prompt:\"~s\"}",[Pattern,Prompt]);
rule1(_,_,#rule{name=notExactly, prompt=Prompt, pattern=Pattern}) -> wf:f("{type:'notExactly[~s]',prompt:\"~s\"}",[Pattern,Prompt]);

%%lenth
rule1(_,_,#rule{name=minLength, prompt=Prompt, 
                                pattern=Pattern}) -> S = integer_to_list(Pattern), wf:f("{type:'minLength[~s]',prompt:\"~s\"}",[S,Prompt]);
rule1(_,_,#rule{name=exacLength, prompt=Prompt, 
                                 pattern=Pattern}) -> S = integer_to_list(Pattern),wf:f("{type:'exacLength[~s]',prompt:\"~s\"}",[S,Prompt]);
rule1(_,_,#rule{name=maxLength, prompt=Prompt,
                                pattern=Pattern}) -> S = integer_to_list(Pattern), wf:f("{type:'maxLength[~s]',prompt:\"~s\"}",[S,Prompt]);

%%SELEC COUNT
rule1(_,_,#rule{name=minCount, prompt=Prompt, pattern=Pattern}) -> S = integer_to_list(Pattern),wf:f("{type:'minCount[~s]',prompt:\"~s\"}",[S,Prompt]);
rule1(_,_,#rule{name=exactCount, prompt=Prompt, pattern=Pattern}) -> S = integer_to_list(Pattern),wf:f("{type:'exactCount[~s]',prompt:\"~s\"}",[S,Prompt]);
rule1(_,_,#rule{name=maxCount, prompt=Prompt, pattern=Pattern}) -> S = integer_to_list(Pattern),wf:f("{type:'maxCount[~s]',prompt:\"~s\"}",[S,Prompt]).


prompt(required)             -> "{name} must have a value";
prompt(empty)                -> "{name} must have a value";
prompt(checked)              -> "{name} must be checked";
prompt(email)                -> "{name} must be a valid e-mail";
prompt(url)                  -> "{name} must be a valid url";
prompt(regExp)               -> "{name} is not formatted correctly";
prompt(integer)              -> "{name} must be an integer";
prompt(decimal)              -> "{name} must be a decimal number";
prompt(number)               -> "{name} must be set to a number";
prompt(is)                   -> "{name} must be '{ruleValue}'";
prompt(isExactly)            -> "{name} must be exactly '{ruleValue}'";
prompt('not')                -> "{name} cannot be set to '{ruleValue}'";
prompt(notExactly)           -> "{name} cannot be set to exactly '{ruleValue}'";
prompt(contain)              -> "{name} cannot contain '{ruleValue}'";
prompt(containExactly)       -> "{name} cannot contain exactly '{ruleValue}'";
prompt(doesntContain)        -> "{name} must contain '{ruleValue}'";
prompt(doesntContainExactly) -> "{name} must contain exactly '{ruleValue}'";
prompt(minLength)            -> "{name} must be at least {ruleValue} characters";
prompt(length)               -> "{name} must be at least {ruleValue} characters";
prompt(exactLength)          -> "{name} must be exactly {ruleValue} characters";
prompt(maxLength)            -> "{name} cannot be longer than {ruleValue} characters";
prompt(match)                -> "{name} must match {ruleValue} field";
prompt(different)            -> "{name} must have a different value than {ruleValue} field";
prompt(creditCard)           -> "{name} must be a valid credit card number";
prompt(minCount)             -> "{name} must have at least {ruleValue} choices";
prompt(exactCount)           -> "{name} must have exactly {ruleValue} choices";
prompt(maxCount)             -> "{name} must have {ruleValue} or less choices".

init_fields(#document{}=D,Obj) ->
   Fields = [begin case X of 
                #field{}=X -> X;
                #cat{}=X ->X#cat.fields
              end end || X<-D#document.fields], 
   Name = D#document.name,
   lists:flatten(
    [begin 
        N = wf:atom([Name,X#field.name]),        
        case X#field.type of
          checkbox -> init_checkbox(X,N,Obj);
          multi_select -> init_multi(X,N,Obj);
          multi_search_select -> init_multi(X,N,Obj);          
          _ ->  init_dropdown(X,N,Obj)
        end end
    || X <- lists:flatten(Fields), lists:member(X#field.type,?SELECT ++ [checkbox])]).

init_multi(X,N,Obj) ->
  V = maps:get(N, Obj, <<>>),
  wf:f(
       "$('#~s').dropdown();"
       "$('#~s').parent().dropdown({"
                                  "onNoResults:function(v){ws.send(enc(tuple(atom('client'),tuple(bin('set_unknow_value'),bin('~s'),bin(v)))));}"
                                  ",onAdd:function(v,t,$i){ws.send(enc(tuple(atom('client'),tuple(bin('add_value'),bin('~s'),bin(v)))));}"
                                  ",onRemove:function(v,t,$i){ws.send(enc(tuple(atom('client'),tuple(bin('del_value'),bin('~s'),bin(v)))));}});"
       ,
       [N,N,X#field.name,X#field.name,X#field.name
       ]).

init_dropdown(X,N,Obj) ->
  V = maps:get(N, Obj, <<>>),
  wf:f("$('#~s_dropdown').dropdown();"
       "$('#~s_dropdown').dropdown({onChange:function(v,t,$i){$('#~s').val(v);ws.send(enc(tuple(atom('client'),tuple(bin('set_value'),bin('~s'),bin(v)))));}});"
       "$('#~s_dropdown').dropdown('set selected', '~s');",
       [N,N,N,X#field.name,N,V]).

%onNoResults(searchValue)

init_checkbox(X,N,Obj) ->
  V = maps:get(N, Obj, <<>>),
  wf:f("$('#~s_checkbox').checkbox();"
       "$('#~s_checkbox').checkbox({onChecked:function(v,t,$i){if(v==undefined){$('#~s').val(true);}else{$('#~s').val(v);};ws.send(enc(tuple(atom('client'),tuple(bin('set_value'),bin('~s'),bin(v)))));},"
                                  " onUnchecked:function(v,t,$i){if(v==undefined){$('#~s').val(false);}else{$('#~s').val(v);};ws.send(enc(tuple(atom('client'),tuple(bin('del_value'),bin('~s'),bin(v)))));}"
                                  "});",
       [N,
        N,N,N,X#field.name,
        N,N,X#field.name]).


init(#document{}=D,Obj) ->
  InitFields = init_fields(D,Obj),
  Validation = validation(D),  
  init(#document{}=D,Obj, Validation++InitFields).

init(#document{}=D,Obj, []) ->
  Default = D#document.default,
  case proplists:get_value(init, Default, undefined) of
    undefined -> skip; Init -> 
    wf:wire(Init) end;

init(#document{}=D,Obj, Init) ->
  Default = D#document.default,
  case proplists:get_value(init, Default, undefined) of
    undefined -> wf:wire(Init); 
    Init0 -> 
    wf:info(?MODULE,"init:~p",[Init++Init0]),
    wf:wire(Init ++ Init0) end.

render_options({Taxonomy, List}, Lookup, Lang) ->
 [begin 
  Items = [begin T=case O#opt.title of [] -> Lookup(Lang,O#opt.name); _-> Lookup(Lang,O#opt.title) end,
            #link{class=[item],data_fields=[{'data-value',O#opt.name},{'data-text',T}]} 
           end|| O <- onthology:Taxonomy(Cat)],
        #panel{class=[column], body=[
         #h4{class=[ui,header], body=[Cat]},
         #panel{class=[ui,link,list], body=[Items]}
        ]} end || Cat <- List];

render_options(Opts, Lookup, Lang) ->
  [begin 
    T=case O#opt.title of [] -> Lookup(Lang,O#opt.name); _-> Lookup(Lang,O#opt.title) end,
    B=case O#opt.icon of [] -> Lookup(Lang,T);
    Icon ->[#i{class=Icon},Lookup(Lang,T)]end,
            #panel{class=[item],data_fields=[{'data-value',O#opt.name},{'data-text',T}], 
             body=B} end||O<-Opts].

linked_select(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N = wf:atom([Name,X#field.name]),
  Options = render_options([], Lookup, Lang),
  #panel{id=wf:atom([N, dropdown]), class=[ui,dropdown,selection], body=[
   #panel{class=[ui,left,icon,input], body=[
    #input{ id=N, name=N, type=hidden, class=Class}
    %,case X#field.icon of [] -> []; _ -> #i{class=X#field.icon} end
   ]},    
   #panel{class=[default,text], body=Lookup(Lang,X#field.ph)},
   #i{class=[dropdown,icon]},
   #panel{class=[menu], body=Options}   
  ]}.

multi_search_select(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  Options = [begin 
              T=case O#opt.title of [] -> Lookup(Lang,O#opt.name); _-> Lookup(Lang,O#opt.title) end,
              #option{value=O#opt.name, body=T}
             end||O<-X#field.options],  
  N = wf:atom([Name,X#field.name]),
  #select{id=N, name=N, class=[ui,fluid,search,dropdown], data_fields=[{multiple,<<>>}], body=Options}.

multi_select(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  Options = [begin 
              T=case O#opt.title of [] -> Lookup(Lang,O#opt.name); _-> Lookup(Lang,O#opt.title) end,
              #option{value=O#opt.name, body=T}
             end||O<-X#field.options],  
  N = wf:atom([Name,X#field.name]),
  #select{id=N, name=N, class=[ui,fluid,normal,dropdown], data_fields=[{multiple,<<>>}], body=Options}.


search_select(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  Options = render_options(X#field.options, Lookup, Lang),
  N = wf:atom([Name,X#field.name]),
  #panel{id=wf:atom([N, dropdown]), class=[ui,search,selection,dropdown], body=[
   #panel{class=[ui,left,icon,input], body=[
    #input{ id=N, name=N, type=hidden, class=Class}
    %,case X#field.icon of [] -> []; _ -> #i{class=X#field.icon} end
   ]},    
   #panel{class=[default,text], body=Lookup(Lang,X#field.ph)},
   #i{class=[dropdown,icon]},
   #panel{class=[menu], body=Options}   
  ]}.

select(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  Options = render_options(X#field.options, Lookup, Lang),
  N = wf:atom([Name,X#field.name]),
  #panel{id=wf:atom([N, dropdown]), class=[ui,dropdown,selection], body=[
   #panel{class=[ui,left,icon,input], body=[
    #input{ name=N, id=N, type=hidden, class=Class}
    %,case X#field.icon of [] -> []; _ -> #i{class=X#field.icon} end
   ]},    
   #panel{class=[default,text], body=Lookup(Lang,X#field.ph)},
   #i{class=[dropdown,icon]},
   #panel{class=[menu], body=Options}   
  ]}.

dynamic(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  string(Name, X, ErrMsg, Class, Object, {Lookup, Lang}).
number(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  string(Name, X, ErrMsg, Class, Object, {Lookup, Lang}).
integer(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  string(Name, X, ErrMsg, Class, Object, {Lookup, Lang}).
float(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  string(Name, X, ErrMsg, Class, Object, {Lookup, Lang}).
currency(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  string(Name, X, ErrMsg, Class, Object, {Lookup, Lang}).

%FIXME
date_range(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  Fmt  = "YYYY/MM/DD h:mm A",
  Incr = 60, 
  Actions = wf:f("$('#~s').daterangepicker({"
                "timePicker: true,"
                "format: '~s',"
                "timePickerIncrement: ~s"
            "}, function(start, end, label) {"
                "console.log(start.toISOString(), end.toISOString(), label);"
            "});",[N,Fmt,integer_to_list(Incr)]),
  #input{ id=N, name=N, class=Class,
          actions=Actions,
          value=maps:get(X#field.name, Object, <<>>), 
          placeholder=Lookup(Lang,X#field.ph)
        }.

%FIXME
date(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  Fmt = "YYYY/MM/DD h:mm A",
  Actions = wf:f("$('#~s').daterangepicker({"
                "singleDatePicker: true,"
                "timePicker: true,"
                "format: '~s'"
            "}, function(start, end, label) {"
                "console.log(start.toISOString(), end.toISOString(), label);"
            "});",[N,Fmt]),
  #input{ id=N, name=N, class=Class,
          actions=Actions,
          value=maps:get(X#field.name, Object, <<>>), 
          placeholder=Lookup(Lang,X#field.ph)
        }.

checkbox(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N = wf:atom([Name,X#field.name]),
  #panel{ id=wf:atom([N,checkbox]), class=[ui,checkbox], body=[
    #input{id=N, name=N, class=Class, type=checkbox, tabindex=0
          ,value=maps:get(X#field.name, Object, <<>>)
          ,placeholder=Lookup(Lang,X#field.ph)
          },
    #label{body=[Lookup(Lang,X#field.label)]}
  ]}.

radio_checkbox(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  Options = [
           begin 
             N=wf:atom([Name,X#field.name]),
             T=case O#opt.title of [] -> Lookup(Lang,O#opt.name); _-> Lookup(Lang,O#opt.title) end,
             #panel{class=[field],body=[
              #panel{class=[ui,radio,checkbox], body=[
               #input{id=N, class=[hidden], tabindex=0, type=radio, value=maps:get(N, Object, <<>>)},
               #label{class=X#field.labelClass,style=X#field.styleRadio,body=Lookup(Lang,T)}
              ]}
             ]}
           end || O<-X#field.options],
  [
   #label{class=X#field.labelClass,style=X#field.styleLabel,body=[#i{class=X#field.icon},Lookup(Lang,X#field.label)]},
   Options
  ].

textarea(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  #textarea{ id=N, name=N, class=Class,
          value=maps:get(X#field.name, Object, <<>>),
          placeholder=Lookup(Lang,X#field.ph)
        }.


price(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  #panel{class=[ui,left,icon,labeled,input],body=[    
    #input{ id=N, name=N, class=Class,
            value=maps:get(X#field.name, Object, <<>>),
            placeholder=Lookup(Lang,X#field.ph)
          },
    case X#field.icon of [] -> []; _ -> #i{class=X#field.icon} end,
    #panel{class=[ui,basic,label],body=[<<"元"/utf8>>]}          
  ]}.

search_menu(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
 N=wf:atom([Name,X#field.name]),

 {Taxonomy, List} = X#field.options, 
 Body = render_options(X#field.options, Lookup, Lang),

 #panel{class=[ui,dropdown,browse], body=[
  #i{class=[filter,icon]},
  #span{class=[text], body=Lookup(Lang,"Filter category")},

  #panel{class=[menu], body=[
   #panel{class=[ui,icon,search,input], body=[
    #i{class=[search,icon]},
    #input{name=N,class=Class,placeholder=Lookup(Lang,"Search category..."), type=text}
   ]},
   #panel{class=[divider]},
   #panel{class=[ui,fluid,popup,bottom,left,transition,hidden], body=[
    #panel{class=[ui,four,column,relaxed,equal,height,divided,grid], body=Body}
   ]}
  ]}
 ]}.


file(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  Uid=wf:atom([Name,X#field.name]),
  bind(ftp_open,  click,  wf:f("qi('~s').click(); e.preventDefault();",[Uid])),
  bind(ftp_start, click, "ftp.start();"),
  bind(ftp_stop,  click, "ftp.stop();"),
  bind(ftp_cancel, click, wf:f("ftp.cancel();$('#~s').val('');",[Uid])),
  Actions = wf:f("$('input:text, #ftp_open.ui.button', '.ui.action.input')"
                 ".on('click',function(e){$('input:file', $(e.target).parents()).click();});"
                 "$('input:file', '.ui.action.input').on('change', function(e) {"
                 "var name = e.target.files[0].name;"
                 "$('input:text', $(e.target).parent()).val(name);"
                 "ftp.init(this.files[0],1,$('input:text').val());"
                 "$('#ftp_status').parent().show();"
                 "});"
                ,[]),
  #panel{id=Uid, class=[ui,fluid,action,input], actions=Actions, body=[
    #input{type=text, data_fields=[{readonly, <<>>}]}
    ,#input{type= file, style=["display:none;"]}
    ,#button{ class=[ui,green,icon,button], style=["display:none;width:5em;"], body=[#span{id=ftp_status,body=[]}]}
    ,#button{id = ftp_open,class=[ui,green,icon,button], body=[#i{class=[file,icon]}]}
    ,#button{id = ftp_start,class=[ui,green,icon,button], body=[#i{class=[upload,icon]}]}
    ,#button{id = ftp_stop,class=[ui,green,icon,button], body=[#i{class=[stop,icon]}]}
    ,#button{id = ftp_cancel,class=[ui,green,icon,button], body=[#i{class=[cancel,icon]}]}       
  ]}.

bind(Control,Event,Code) ->
    wf:wire(#bind{target=Control,type=Event,postback=Code}).  


hidden(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  #input{ id=N, name=N, class=Class,type=hidden,
          value=maps:get(X#field.name, Object, <<>>), 
          placeholder=Lookup(Lang,X#field.ph)
        }.

readonly(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  #input{ id=N, name=N, class=Class, 
          value=maps:get(X#field.name, Object, <<>>), 
          data_fields=[{readonly,<<>>}],
          placeholder=Lookup(Lang,X#field.ph)
        }.

string(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  #input{ id=N, name=N, class=Class,
          value=maps:get(X#field.name, Object, <<>>), 
          placeholder=Lookup(Lang,X#field.ph)
        }.

mobile(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  #input{ id=N, name=N, class=Class,
          value=maps:get(X#field.name, Object, <<>>), 
          placeholder=Lookup(Lang,X#field.ph)
        }.

password(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  N=wf:atom([Name,X#field.name]),
  #input{ id=N, name=N, class=Class, type=password,
          value=maps:get(X#field.name, Object, <<>>), 
          placeholder=Lookup(Lang,X#field.ph)
        }. 

confirm_password(Name, X, ErrMsg, Class, Object, {Lookup, Lang}) ->
  repassword(Name, X, ErrMsg, Class, Object, {Lookup, Lang}).

repassword(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->        
  N=wf:atom([Name,X#field.name]),
  #input{ id=N, name=N, class=Class, type=password,
          value=maps:get(X#field.name, Object, <<>>), 
          placeholder=Lookup(Lang,X#field.ph)
        }.

email(Name, #field{}=X, ErrMsg, Class, Object, {Lookup, Lang}) ->   
  N=wf:atom([Name,X#field.name]),
  #input{ id=N, name=N, class=Class, type=text,
          value=maps:get(X#field.name, Object, <<>>), 
          placeholder=Lookup(Lang,X#field.ph)
        }.

translatable_strings(Documents) when is_list(Documents)->
     lists:foldl(fun(X, Acc) -> translatable_strings(X) ++ Acc end,[], Documents);
     
translatable_strings(#document{}=D) ->
     [D#document.name]
     ++ trans_strings(D#document.steps)
     ++ trans_strings(D#document.sections) 
     ++ trans_strings(D#document.fields).

trans_strings(#step_wizard{steps=Steps}) -> trans_strings(Steps);
trans_strings(L) when is_list(L) -> trans_strings(L, []).

trans_strings([], Acc) -> Acc;

trans_strings([#sec{name=N, desc=undefined}|T], Acc) -> 
    trans_strings(T, [N|Acc]);
trans_strings([#sec{name=N, desc=Desc}|T], Acc) -> 
    trans_strings(T, [N,Desc|Acc]);
trans_strings([#step{title=Title, description=Desc}|T], Acc) -> 
    trans_strings(T, [Title, Desc | Acc]);
trans_strings([#field{title=[], ph=Ph}|T], Acc) -> 
    trans_strings(T, [Ph|Acc]);
trans_strings([#field{title=Title, ph=Ph, label=[]}|T], Acc) -> 
    trans_strings(T, [Title,Ph|Acc]);
trans_strings([#field{title=Title, ph=Ph}|T], Acc) -> 
    trans_strings(T, [Title,Ph|Acc]). 

stepwizard(_,[],_) -> [];
stepwizard(Name, #step_wizard{}=W, {Lookup, Lang}) ->
     case W#step_wizard.steps of [] -> []; _ -> N=sem(length(W#step_wizard.steps)),      
     FunStepW = fun(#step{id=Id, title=Title, description=Desc, icon=Icon}, Acc) -> 
        case Id == W#step_wizard.current_step of
                        true -> 
                            [#panel{id=wf:atom([Name,step,Id]), class=[active,step], body=[
                              #i{class=Icon},
                                #panel{class=[content],body=[
                                 #panel{class=[title],body=[Lookup(Lang,Title)]},
                                 #panel{class=[description],body=Lookup(Lang,Desc)}
                                ]}
                            ]}|Acc];
                        false ->
                            case Id < W#step_wizard.current_step of
                                true ->
                            [#panel{id=wf:atom([Name,step,Id]),class=[completed,step], body=[
                              #i{class=Icon},
                                #panel{class=[content],body=[
                                 #panel{class=[title],body=Lookup(Lang,Title)},
                                 #panel{class=[description],body=Lookup(Lang,Desc)}
                                ]}
                            ]}|Acc];
                                false ->
                            [#panel{id=wf:atom([Name,step,Id]),class=[disable,step,hidden], body=[
                              #i{class=Icon},
                                #panel{class=[content],body=[
                                 #panel{class=[title],body=Lookup(Lang,Title)},
                                 #panel{class=[description],body=Lookup(Lang,Desc)}
                                ]}
                            ]}|Acc] end                                       
                    end end,
     Steps = lists:foldr(FunStepW,[],W#step_wizard.steps),
     #panel{class=[ui, N, steps], body=Steps} end.

render_buttons(Name, GrpClass, {Lookup, Lang}, Buttons) ->
    Body = lists:foldr(fun(#but{}=But,Acc) ->
                    [#button{id=But#but.id, 
                           class=But#but.class, 
                           postback=But#but.postback,
                           body=[#i{class=But#but.icon},Lookup(Lang, #but.title)],
                           source=[wf:atom([Name,S])||S<-But#but.sources]
                           }|Acc] 
                       end,[],Buttons),
    #panel{class=GrpClass, body=Body}.


%% RENDER FIELDS
render_fields(Name, Fields, Sections, Object, Default, {Lookup, Lang}) ->
  rr_fields(Name, Fields, Sections, Object, Default, {Lookup, Lang}).

rr_fields(Name, Fields, [], Object, Default, {Lookup, Lang}) ->
  rr_fields(Name, Fields, [], undefined, {1,[]}, Object, Default, {Lookup, Lang}, []);                  

rr_fields(Name, [X|_] = Fields, [H|SecTail], Object, Default, {Lookup, Lang}) ->
    %case X#field.sec /= H#sec.id of true ->
      E = case H#sec.buttons of [] -> 
             #h3{class=H#sec.nameClass, body=[Lookup(Lang,H#sec.name)]};
            B ->[render_buttons(Name, H#sec.groupClass, {Lookup,Lang}, B),#h3{class=H#sec.nameClass, body=Lookup(Lang,H#sec.name)}]
          end, 
      rr_fields(Name, Fields, SecTail, H#sec.id, {1,[]}, Object, Default, {Lookup, Lang}, [E]).%;                  
     %_ ->         
     % rr_fields(Name, Fields, SecTail, H#sec.id, {1,[]}, Object, Default, {Lookup, Lang}, []) end.

rr_fields(_, [], _, _, {_,[]}, _, _, _, Acc) -> lists:reverse(Acc);

rr_fields(Name, [#cat{}|_]=L, [H|TailSections]=Sections, PrevSec, {PrevGrp,AccGrp}, Object, Default, {Lookup,Lang}, Acc1) ->
    %flush
    N=length(AccGrp),
    GG = case N of
         0 -> [];      
         1 -> r_fields(Name, AccGrp, Object, Default, {Lookup,Lang});
         N when N > 1 -> #panel{class=[sem(N),fields], body=[r_fields(Name, AccGrp, Object, Default, {Lookup,Lang})]}
        end,

    E = case H#sec.buttons of [] -> 
           #h3{class=H#sec.nameClass, body=[Lookup(Lang,H#sec.name)]};
            B ->[render_buttons(Name, H#sec.groupClass, {Lookup,Lang}, B),#h3{class=H#sec.nameClass, body=Lookup(Lang,H#sec.name)}]
        end,
            
    {NewFields, NewSections, NewPrevSec, NewAcc1} = render_cat(Name, L, Sections, PrevSec, Object, Default, {Lookup,Lang}, [E,GG|Acc1], []),
    rr_fields(Name, NewFields, NewSections, NewPrevSec, {PrevGrp,[]}, Object, Default, {Lookup,Lang}, NewAcc1);


rr_fields(Name, [#field{g=CurrentGrp}=X|T], Sections, PrevSec, {CurrentGrp,AccGrp}, Object, Default, {Lookup, Lang}, Acc) ->
    rr_fields(Name, T, Sections, PrevSec, {CurrentGrp,[X|AccGrp]}, Object, Default, {Lookup, Lang}, Acc);


rr_fields(Name, [], Sections, PrevSec, {PrevGrp,AccGrp}, Object, Default, {Lookup, Lang}, Acc) ->
    N=length(AccGrp),
    GG = case N of
         0 -> [];
         1 -> r_fields(Name, AccGrp, Object, Default, {Lookup, Lang});
         N when N > 1-> #panel{class=[sem(N),fields], body=[r_fields(Name, AccGrp, Object, Default, {Lookup,Lang})]}
        end,  
    rr_fields(Name, [], Sections, PrevSec, {PrevGrp,[]}, Object, Default, {Lookup, Lang}, [GG|Acc]);


rr_fields(Name, [#field{g=Current}=X|T], [], PrevSec, {PrevGrp,AccGrp}, Object, Default, {Lookup,Lang}, Acc) when Current > PrevGrp->
    N=length(AccGrp),
    GG = case N of
         0 -> [];
         1 -> r_fields(Name, AccGrp, Object, Default, {Lookup,Lang});
         N when N > 1 -> #panel{class=[sem(N),fields], body=[r_fields(Name, AccGrp, Object, Default, {Lookup,Lang})]}
        end,  
    rr_fields(Name, T, [], X#field.sec, {Current,[X]}, Object, Default, {Lookup,Lang}, [GG|Acc]);

rr_fields(Name, [#field{g=Current}=X|T], [H|SecTail]=Sections, PrevSec, {PrevGrp,AccGrp}, Object, Default, {Lookup,Lang}, Acc) when Current > PrevGrp->
    N=length(AccGrp),
    GG = case N of
         0 -> [];
         1 -> r_fields(Name, AccGrp, Object, Default, {Lookup,Lang});
         N when N > 1 -> #panel{class=[sem(N),fields], body=[r_fields(Name, AccGrp, Object, Default, {Lookup,Lang})]}
        end,

    case X#field.sec > PrevSec of
        true ->
            E = case H#sec.buttons of [] -> #h3{class=H#sec.nameClass, body=[Lookup(Lang,H#sec.name)]}; 
                   B ->[render_buttons(Name, H#sec.groupClass, {Lookup,Lang}, B),#h3{class=H#sec.nameClass, body=Lookup(Lang,H#sec.name)}]
                end,
            rr_fields(Name, T, SecTail, X#field.sec, {Current,[X]}, Object, Default, {Lookup,Lang}, [E,GG|Acc]);
        false ->
           rr_fields(Name, T, Sections, PrevSec, {Current,[X]}, Object, Default, {Lookup,Lang}, [GG|Acc])
    end.

%% RENDER CAT
render_cat(Name, [#cat{}=X|T], Sections, PrevSec, Object, Default, I8n, Acc1, AccCat) ->
  render_cat(Name, T, Sections, PrevSec, Object, Default, I8n, Acc1, AccCat++[X]);

render_cat(Name, [#field{}=X|_]=L, [H|Tail]=Sections, PrevSec, Object, Default, {Lookup,Lang}, Acc1, AccCat) ->
  [F|Rest] = AccCat,
  First = begin 
               FBody = render_fields(Name, F#cat.fields, [], Object, Default, {Lookup, Lang}) ,
               [#panel{class=[active,title,active],body=[#i{class=[dropdown,icon]},Lookup(Lang,F#cat.title)]},
                #panel{class=[content,field,active], body=FBody}
               ] end,
  Result = First ++ [begin 
               Body = render_fields(Name, C#cat.fields, [], Object, Default, {Lookup, Lang}) ,
               [#panel{class=[active,title],body=[#i{class=[dropdown,icon]},Lookup(Lang,C#cat.title)]},
                #panel{class=[content,field], body=Body}
               ] end || C<- Rest],
  Accordion = #panel{id=wf:atom([H#sec.name]),class=H#sec.class,body=Result},
  {L, Tail, PrevSec, [Accordion|Acc1]}.


r_fields(Name, Fields, Object, Default, {Lookup,Lang}) ->
    lists:foldl(
    fun (#field{}=X,Acc) ->

            IClass = case X#field.iClass of [] -> proplists:get_value(iClass, Default, []); _-> X#field.iClass end,
            FClass = case X#field.fClass of [] -> proplists:get_value(fClass, Default, [field]); _-> X#field.fClass end,
            Class = case X#field.class of [] -> proplists:get_value(class, Default, []); _-> X#field.class end,
            ErrMsg = case X#field.errMsg of [] -> proplists:get_value(errMsg, Default, []); _-> X#field.errMsg end,

            Label = case lists:member(X#field.type,[radio_checkbox,checkbox]) of true ->[]; 
                        false ->case X#field.label of [] -> []; _ -> #label{class=X#field.labelClass,body=Lookup(Lang,X#field.label)} end end,
            Input = ?MODULE:(X#field.type)(Name, X, ErrMsg, Class, Object, {Lookup,Lang}),
            Icon = case X#field.icon of [] -> []; _ -> #i{class=X#field.icon} end,

            case lists:member(X#field.type, [files,file,price,multi_search_select,
                                             multi_select,search_select,select,multi_search_select,
                                             textarea,checkbox,radio_checkbox,search_menu]) of 
                true -> 
                    [#panel { class=FClass, body=[Label,Input]}|Acc]; 
                false ->
                    case Icon of [] -> [#panel { class=FClass, body =[Label,Input]} | Acc];
                    _ -> [#panel { class=FClass, body =[Label,#panel{class=Class, body=[Input,Icon]}]} | Acc] end
            end
end,
[], Fields).

sem( 0) -> zero;
sem( 1) -> one;
sem( 2) -> two;
sem( 3) -> three;
sem( 4) -> four;
sem( 5) -> five;
sem( 6) -> six;
sem( 7) -> seven;
sem( 8) -> height;
sem( 9) -> nine;
sem(10) -> ten;
sem(11) -> eleven;
sem(12) -> twelve;
sem(13) -> thirteen;
sem(14) -> fourteen;
sem(15) -> fithteen;
sem(16) -> sixteen;
sem(17) -> seventeen;
sem(18) -> eightteen;
sem(19) -> nineteen;
sem(20) -> twenty.



