pro main_mvn_load,days=days,sta=sta,swi=swi,swe=swe,tplot=tplot
   
   st0 = '2014-11-05'
   
   for i=0,days-1 do begin
     st = time_double(st0)
     st = st + 3600d*24d*i
     et = st + 3600d*24d
     timespan,time_string([st,et])
     ;mvn_swia_load_l2_data,/loadall,tplot=tplot
     if keyword_set(sta) then mvn_sta_l2_load
     if keyword_set(swe) then mvn_swe_load_l2,[st,et],/spec
   
   endfor 

end