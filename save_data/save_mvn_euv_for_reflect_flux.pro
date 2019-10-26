pro save_mvn_euv_for_reflect_flux

   env = init_env()
   SAVE_LOC = env.SAVE_LOC   
   orbit_arr = [317+findgen(2500)]

   ;tplot_saved_mvn_euv
   ;get_data,'EUVM',data=euvdat
   
   for j=0,n_elements(orbit_arr)-1 do begin
       trange = mvn_orbit_num(orbnum = [orbit_arr[j]-0.5,orbit_arr[j]+0.5])
       timespan,trange[0],trange[1]-trange[0],/sec
       
       mvn_euv_load,trange=trange,/all
       get_data,'mvn_euv_data',data=euvdat
       get_data,'mvn_euv_flag',data=euvflg
       idx_flg0 = where(euvflg.y eq 0)
       if idx_flg0[0] eq -1 then goto, nodata       
       euv_time = euvdat.x[idx_flg0]       
       euv_flux = euvdat.y[idx_flg0,*]
       
       time_arr = 0d
       euv_arr = dblarr(3)
       file_flx = SAVE_LOC + '/maven/sav/flux/deflect_ion_rmv_ring/d0/flux_vdf_'+string(orbit_arr[j],format='(i05)')+'.sav'
       ft_flx = file_test(file_flx)
       if ft_flx eq 0 then goto, nodata
       restore,file_flx
            
       for n=0,n_elements(dat_sav_arr.stime)-1 do begin
         stime = dat_sav_arr[n].stime
         etime = dat_sav_arr[n].etime
         mtime = mean([stime, etime],/double)             
         idx = where(euv_time ge stime and euv_time lt etime,ndat)         
         if ndat lt 5 then begin
          euv = [!values.D_NaN,!values.D_NaN,!values.D_NaN]
          goto, noavg 
         endif        
         if idx[0] ne -1 then euv = mean(euv_flux[idx,*],dimension=1,/nan) else euv = [!values.D_NaN,!values.D_NaN,!values.D_NaN]
         noavg:
         time_arr = [time_arr,mtime]
         euv_arr = [[euv_arr], [euv]]     
       endfor
       time_arr = time_arr[1:*]
       euv_arr = transpose(euv_arr)
       euv_arr = euv_arr[1:*,*]
       euv_dat = {time:time_arr, euv_avg:euv_arr}
      
       save,euv_dat,file=SAVE_LOC+'/maven/sav/euv/avg_with_sta_d0_flux/euv_avg_d0_'+string(orbit_arr[j],format='(i05)')+'.sav'
       nodata:
   endfor
    
end