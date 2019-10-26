pro auto_mvn_emplot,date,orbit=orbit
    
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    del_data,'*'
    
    ;if keyword_set(date) then begin
    ;  timespan,date
    ;  save_path = '/maven/png/emplot/'
    ;endif
    if keyword_set(orbit) then begin
      timespan,mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
      png_path = '/maven/png/emplot/orbit/'+string(orbit,format='(i05)')
    endif
    
    mvn_sta_l2_load,sta_apid=['c6','d0']
    mvn_sta_l2_tplot
    get_data,'mvn_sta_d0_M',data=d   
    
    if size(d,/type) ne 8 then begin
      dprint,'No d0 data is found'
      return
    endif
    time = d.x
    ndat = n_elements(time)
    
;    tspan = time[ndat-1]-time[0]
;    ndat_d0 = fix(tspan/256d)
      
    for it=0,ndat-1 do begin
        tspan = [ time[it]-64d, time[it]+64d ]
     ;  tspan = [ time[0]+256d*it, time[1]+256d + 256d * it ]
       mvn_sta_emplot,tspan,/def
       date_s = strmid(time_string(time[it]),0,4)+strmid(time_string(time[it]),5,2)+strmid(time_string(time[it]),8,2)+ $
                strmid(time_string(time[it]),11,2)+strmid(time_string(time[it]),14,2)+strmid(time_string(time[it]),17,2)
       if file_test(SAVE_LOC+png_path) eq 0 then file_mkdir,SAVE_LOC+png_path
       makepng, SAVE_LOC+png_path+'/emplot_'+date_s
      
    endfor
   
end