-ifndef(SEMENTIC_META_HRL).
-define(SEMENTIC_META_HRL, "semantic_meta_hrl").

-include_lib("kvs/include/kvs.hrl").

-record(step_wizard,{current_step=1, steps=[]}).
-record(step,       {id, title, description, icon, sections}).
-record(document,   {?ITERATOR(feed), style=[],action, lang=en, name=[], base=[], sections=[], fields=[], buttons=[], access=[], steps=[], validations=[], default=[], class=[ui,container], fClass=[ui,form,ui,piled,segment]}).
-record(validation, {name, type, msg, extract = fun(X) -> X end, options=[], function, field={record,pos} }).
-record(sec,        {id, name=[], m, desc=[], nameClass=[], descClass=small, class=[], buttons=[], groupClass=[ui,right,floated, small,basic,icon,buttons]}).
-record(but,        {id, href, target, postback, name, title, sources=[], class , icon, onclick}).
-record(opt,        {id, name, title=[], postback, checked=false, disabled=false, noRadioButton=false, icon=[] }).
-record(cap,        {id, name, title, postback, checked=false, disabled=false, noRadioButton=false }). %%captcha
-record(sel,        {id, name, title, postback }).
-record(cat,        {id, name, title, tClass=[], class=[], icon=[], fields=[], sec=1}).
-record(field,      {id, e, sec=1, name, pos, g, c=[], title=[], styleLabel=[], styleRadio=[], icon=[], layout, visible=true, disabled=false, format="~w", curr="",
                     postfun=[], desc, type=string, etc, label=[], labelClass=[], class=[], fClass=[], iClass=[], boxClass=box, ph="",
                     access, tooltips=[], options=[], postback, errMsg=[], rules=[], optional=false}).
-record(rule, {name, min, max, exac, pattern=[], target, prompt=[]}).

-record(drp,{
          singleDatePicker=false, 
          timePicker=false, 
          timePickerIncrement, 
          format="YYYY/MM/DD h:mm A", 
          startDate,
          endDate,
          minDate,
          dateLimit,
          showDropdowns,
          showWeekNumbers,
          timePicker12Hour,
          ranges,
          opens='left',
          buttonClasses = "ui compact button",
          cancelClass,
          separator,
          locale
          }).

-record(drpl,{
   applyLabel = "Submit",
   cancelLabel= "Clear",
   fromLabel  = "From",
   toLable    = "to",
   customRangeLabel = "Custom",
   daysOfWeek = ["Su", "Mo", "Tu", "We", "Th", "Fr", "Sa"],
   monthNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
   firstDay = 1
  }).
  
-endif.
