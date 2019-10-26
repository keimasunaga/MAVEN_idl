pro tplot_mvn_sw_temp,sd=sd,ndays=ndays,noplot=noplot,save=save

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    time_Ti_arr= 0d
    time_Te_arr = 0d
    Ti_arr = 0d
    Te_arr = 0d
    e = 1.602e-19
    k = 8.617e-5 ;;[eV/K]
    ;k = 1.38e-23 ;;J/K
    
    sd = '20141127'
    ndays=460
    
    for i=0d,ndays-1 do begin
       sd_dbl = time_double(sd) + 3600d*24d*i
       sd_string = time_string(sd_dbl)
       yyyy = strmid(sd_string,0,4)
       mm = strmid(sd_string,5,2)
       dd = strmid(sd_string,8,2)
       filename_swe = SAVE_LOC + '/maven/sav/moment/swe/daily/swe_mom_'+yyyy+mm+dd+'.sav' 
       filename_swi = SAVE_LOC + '/maven/sav/moment/swi/daily/mom_'+yyyy+mm+dd+'.sav' 
       print,filename_swe
       print,filename_swi
       ft_swe = file_test(filename_swe)
       ft_swi = file_test(filename_swi)
       if ft_swe eq 0 then goto, noswe
       restore,filename_swe
       time_Te_arr = [time_Te_arr,mom_elec.swe_temp.x]
       Te = mom_elec.swe_temp.y/k  
       Te_arr = [Te_arr, Te]
       noswe:
       if ft_swi eq 0 then goto, noswi
       restore,filename_swi
       time_Ti_arr = [time_Ti_arr,mom.tproton.x]
       Ti = 1./3. * (mom.tproton.y[*,0]+mom.tproton.y[*,1]+mom.tproton.y[*,2])/k
       Ti_arr = [Ti_arr, Ti]
       noswi:
    endfor
    time_Te_arr = time_Te_arr[1:*]
    time_Ti_arr = time_Ti_arr[1:*]
    Te_arr = Te_arr[1:*]
    Ti_arr = Ti_arr[1:*]
    
    idx = where(Te_arr ge 0)
    time_Te_arr = time_Te_arr[idx]
    Te_arr = Te_arr[idx]
        
    timespan,sd,ndays
    store_data,'Te',data={x:time_Te_arr,y:Te_arr}
    store_data,'Ti',data={x:time_Ti_arr,y:Ti_arr}
    if ~keyword_set(noplot) then tplot,['Te','Ti']
    if keyword_set(save) then tplot_save,['Te','Ti'],file=SAVE_LOC+'/maven/tplot_save/mom/sw_temp'
end