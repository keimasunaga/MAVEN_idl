pro check_mag_stability,b=b,ok=ok_mag


   


end


pro judge_near_bs,t,dt=dt,orbit=orbit,ok=ok
    
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    filename = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit,format='(i05)')+'.sav'
    ft = file_test(filename)
    if ft eq 0 then begin
      print,'No data: '+filename
      goto,nodata
    endif
    restore,filename  
    t_bs = dat_sw.time
    if ~keyword_set(dt) then dt = 300d
    if (t le t_bs[0] and t ge t_bs[0]-dt) or (t ge t_bs[1] and t le t_bs[1]+dt) then ok = 1 else ok = 0
    nodata:
end
  
pro plot_sta_vdf_avg_bk,orbit_arr,vperp_range=vperp_range,Btrange=Btrange,vperb=vperb,$
                     select_region=select_region,_extra=extra,near_bs=near_bs,day=day,night=night,Ehemisphere=Ehemisphere,$
                     gyro_scl=gyro_scl,Large_Rg=Large_Rg,Small_Rg=Small_Rg
   
   orbit_arr =400+indgen(50);indgen(20)+425;[336,337,338,339,340,341,342,343,344,345,346,347];949];428,
   
   env = init_env()
   SAVE_LOC = env.SAVE_LOC


   nbin_acc = 300 & vres_sw = 0.25
   im_xy_avg = fltarr(nbin_acc+1,nbin_acc+1) & im_yz_avg = fltarr(nbin_acc+1,nbin_acc+1) & im_xz_avg = fltarr(nbin_acc+1,nbin_acc+1)  & im_paraperp_avg = fltarr(nbin_acc+1,nbin_acc/2.+1)
   im_xy_acc = fltarr(nbin_acc+1,nbin_acc+1) & im_yz_acc = fltarr(nbin_acc+1,nbin_acc+1) & im_xz_acc = fltarr(nbin_acc+1,nbin_acc+1) & im_paraperp_acc = fltarr(nbin_acc+1,nbin_acc/2.+1)
   N_im_xy_acc = fltarr(nbin_acc+1,nbin_acc+1) & N_im_yz_acc = fltarr(nbin_acc+1,nbin_acc+1) & N_im_xz_acc = fltarr(nbin_acc+1,nbin_acc+1) & N_im_paraperp_acc = fltarr(nbin_acc+1,nbin_acc/2.+1)
   
   
   for hh=0,n_elements(orbit_arr)-1 do begin
    
     filename = SAVE_LOC + '/maven/sav/vdf/orbit/'+string(orbit_arr[hh],'(i05)')+'/vdf_*.sav'
     fs = file_search(filename)
    
    
    for ii=0,n_elements(fs)-1 do begin
   
      restore,fs[ii]
      time = dat_sav.time & ;mtime = (time[0] + time[1])/2d
      nbin = dat_sav.nbin & vres = dat_sav.vres
      v_arr = (findgen(nbin+1)-nbin/2)*vres
      im_xy = dat_sav.im_xy & N_im_xy = dat_sav.N_im_xy & idx_exist_xy = where(N_im_xy ne 0)
      im_yz = dat_sav.im_yz & N_im_yz = dat_sav.N_im_yz & idx_exist_yz = where(N_im_yz ne 0)
      im_xz = dat_sav.im_xz & N_im_xz = dat_sav.N_im_xz & idx_exist_xz = where(N_im_xz ne 0)   
      im_paraperp = dat_sav.im_paraperp & N_im_paraperp = dat_sav.N_im_paraperp & idx_exist_paraperp = where(N_im_paraperp ne 0) 
     
      if idx_exist_xy[0] eq -1 and idx_exist_yz[0] eq -1 and idx_exist_xz[0] eq -1 then goto, noadd
        
      Vmso = dat_sav.Vmso ;& dVmso = dat_sav.Vmso_std
      Bmso = dat_sav.Bmso ;& dBmso = dat_sav.Bmso_std
      Emso = crossp(Bmso,Vmso)
      Vmso_tot = total(Vmso^2)^.5
      Vpara = inpro(Vmso,Bmso)/total(Bmso^2)^.5 & Vperp = sqrt(Vmso_tot^2 - Vpara^2)
      Vperp_tot = total(Vperp^2)^.5     
      pos = dat_sav.pos_mso
     
      if finite(vperp_tot) ne 1 then goto, noadd
      if vperp_tot le 50. then goto, noadd
      
      if keyword_set(gyro_scl) then begin
         vbratio = Vperp_tot/total(Bmso^2)^.5
         Rg = 166.8*vbratio
         if gyro_scl eq 1 then gyro_scl = 3389.9d
         if keyword_set(Large_Rg) then if Rg lt gyro_scl then goto,noadd
         if keyword_set(Small_Rg) then if Rg gt gyro_scl then goto,noadd 
      endif
      
      if keyword_set(select_region) then begin
        region = mvn_judge_region(pos[0],pos[1],pos[2])
        if region ne select_region then goto,noadd
      endif     
      
      if keyword_set(near_bs) then begin
        judge_near_bs,time,orbit=orbit_arr[hh],ok=ok
        print,orbit_arr[hh],ok
        if ok eq 0 then goto, noadd
      endif
    
      if keyword_set(Ehemisphere) then begin
        rot_angle = get_rot_angle(Vmso,Bmso)
        pos_mse = mso2mse(pos[0],pos[1],pos[2],rot_angle)
        if Ehemisphere eq 'plus' then if pos_mse[2] lt 0 then goto,noadd
        if Ehemisphere eq 'minus' then if pos_mse[2] ge 0 then goto,noadd       
      endif
      
      if keyword_set(day) then if pos[0] lt 0 then goto,noadd
      if keyword_set(night) then if pos[0] ge 0 then goto,noadd
          
      ix=round(v_arr/vperp_tot/vres_sw)+nbin_acc/2.
      iy=round(v_arr/vperp_tot/vres_sw)+nbin_acc/2. 
      iz=round(v_arr/vperp_tot/vres_sw)+nbin_acc/2.
      ipara=round(v_arr/vperp_tot/vres_sw)+nbin_acc/2.
      iperp = round(v_arr[nbin/2.:*]/vperp_tot/vres_sw)
      print,fs[ii]
        for jj=0,nbin do begin
          for kk=0,nbin do begin
           
           im_xy_acc[ix[jj],iy[kk]] = im_xy_acc[ix[jj],iy[kk]] + im_xy[jj,kk]
           N_im_xy_acc[ix[jj],iy[kk]] = N_im_xy_acc[ix[jj],iy[kk]] + N_im_xy[jj,kk]
           im_yz_acc[iy[jj],iz[kk]] = im_yz_acc[iy[jj],iz[kk]] + im_yz[jj,kk]
           N_im_yz_acc[iy[jj],iz[kk]] = N_im_yz_acc[iy[jj],iz[kk]] + N_im_yz[jj,kk]
           im_xz_acc[ix[jj],iz[kk]] = im_xz_acc[ix[jj],iz[kk]] + im_xz[jj,kk]
           N_im_xz_acc[ix[jj],iz[kk]] = N_im_xz_acc[ix[jj],iz[kk]] + N_im_xz[jj,kk]
           
           
           if kk le nbin/2. then im_paraperp_acc[ipara[jj],iperp[kk]] = im_paraperp_acc[ipara[jj],iperp[kk]] + im_paraperp[jj,kk]
           if kk le nbin/2. then N_im_paraperp_acc[ipara[jj],iperp[kk]] = N_im_paraperp_acc[ipara[jj],iperp[kk]] + N_im_paraperp[jj,kk]
             
           
           
          endfor  
          
        endfor 
      
;        idx_yz = where(N_im_yz_acc ne 0)
;        idx_yz_zero = where(N_im_yz_acc eq 0)
;        im_yz_avg[idx_yz] = im_yz_acc[idx_yz]/N_im_yz_acc[idx_yz]
;        im_yz_avg[idx_yz_zero] = !values.F_NaN
;        N_im_yz_acc[idx_yz_zero] = !values.F_NaN
;        plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,im_yz_avg,/zlog,xrange=[-5,5],yrange=[-5,5],zrange=[1e-5,1e7],xtit='Vexb',ytit='Ve',charsize=charsize
;        plots,circle_v2(1,0,1),color=1
;        plots,circle_v2(sqrt(2.),0,sqrt(2.)),color=1
;        plots,circle_v2(sqrt(44./16.),0,sqrt(44./16.)),color=1
;        plots,circle_v2(1,0,3),color=1
;      
;        im_yz_acc[*]=0. & im_yz_avg[*]=0. & N_im_yz_acc[*] = 0.

    noadd:
    
    endfor
   endfor
       
       loadct2,39
       wi,1,/new
       !p.multi=[4,2]
       ;;plot averaged vdf
       charsize=1.5
       idx_xy = where(N_im_xy_acc ne 0)
       idx_xy_zero = where(N_im_xy_acc eq 0)
       im_xy_avg[idx_xy] = im_xy_acc[idx_xy]/N_im_xy_acc[idx_xy]      
       im_xy_avg[idx_xy_zero] = !values.F_NaN
       N_im_xy_acc[idx_xy_zero] = !values.F_NaN
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,im_xy_avg,/zlog,xrange=[-5,5],yrange=[-5,5],zrange=[1e4,1e7],mult='4,2',xtit='Vb',ytit='Vexb',charsize=charsize
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,N_im_xy_acc,/zlog,xrange=[-5,5],yrange=[-5,5],zrange=[1,1e6],/add,xtit='Vb',ytit='Vexb',charsize=charsize
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       
       
       idx_yz = where(N_im_yz_acc ne 0)
       idx_yz_zero = where(N_im_yz_acc eq 0)
       im_yz_avg[idx_yz] = im_yz_acc[idx_yz]/N_im_yz_acc[idx_yz]       
       im_yz_avg[idx_yz_zero] = !values.F_NaN
       N_im_yz_acc[idx_yz_zero] = !values.F_NaN
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,im_yz_avg,/zlog,xrange=[-5,5],yrange=[-5,5],zrange=[1e3,1e6],/add,xtit='Vexb',ytit='Ve',charsize=charsize
       plots,circle_v2(1,0,1),color=1
       plots,circle_v2(sqrt(2.),0,sqrt(2.)),color=1
       plots,circle_v2(sqrt(44./16.),0,sqrt(44./16.)),color=1
       plots,circle_v2(1,0,3),color=1
       ;plots,circle_v2(sqrt(2.),0,2*sqrt(2.)),color=1
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,N_im_yz_acc,/zlog,xrange=[-5,5],yrange=[-5,5],zrange=[1,1e6],/add,xtit='Vexb',ytit='Ve',charsize=charsize
       plots,circle_v2(1,0,1),color=1
       plots,circle_v2(sqrt(2.),0,sqrt(2.)),color=1
       plots,circle_v2(sqrt(44./16.),0,sqrt(44./16.)),color=1
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       
       
       idx_xz = where(N_im_xz_acc ne 0)
       idx_xz_zero = where(N_im_xz_acc eq 0)
       im_xz_avg[idx_xz] = im_xz_acc[idx_xz]/N_im_xz_acc[idx_xz]
       im_xz_avg[idx_xz_zero] = !values.F_NaN
       N_im_xz_acc[idx_xz_zero] = !values.F_NaN
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,im_xz_avg,/zlog,xrange=[-5,5],yrange=[-5,5],zrange=[1e4,1e7],/add,xtit='Vb',ytit='Ve',charsize=charsize
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,N_im_xz_acc,/zlog,xrange=[-5,5],yrange=[-5,5],zrange=[1,1e6],/add,xtit='Vb',ytit='Ve',charsize=charsize
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2


       idx_paraperp = where(N_im_paraperp_acc ne 0)
       idx_paraperp_zero = where(N_im_paraperp_acc eq 0)
       im_paraperp_avg[idx_paraperp] = im_paraperp_acc[idx_paraperp]/N_im_paraperp_acc[idx_paraperp]
       im_paraperp_avg[idx_paraperp_zero] = !values.F_NaN
       N_im_paraperp_acc[idx_paraperp_zero] = !values.F_NaN
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc/2.+1))*vres_sw,im_paraperp_avg,/zlog,xrange=[-5,5],yrange=[0,5],zrange=[1e4,1e7],/add,xtit='Vb',ytit='Ve',charsize=charsize
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc/2.+1))*vres_sw,N_im_paraperp_acc,/zlog,xrange=[-5,5],yrange=[0,5],zrange=[1,1e6],/add,xtit='Vb',ytit='Ve',charsize=charsize
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2

end

