pro check_mag_stability,b=b,std=std,ok=ok_mag
 
 
 
   


end



  
pro plot_sta_vdf_event,orbit_arr,stime=stime,vperp_range=vperp_range,Btrange=Btrange,vperb=vperb,$
                     select_region=select_region,_extra=extra,near_bs=near_bs,dt=dt,day=day,night=night,Ehemisphere=Ehemisphere,sza_range=sza_range,$
                     gyro_scl=gyro_scl,Large_Rg=Large_Rg,Small_Rg=Small_Rg,$
                     ymse_range=ymse_range,herium=herium,$
                     _extra=_extra
   
   orbit_arr = 465 ;[336,337,338,339,340,341,342,343,344,345,441,442,443,444,445,446,447,448,449,450,559,560,561,562,563,564,565,566,567,568,569,570];,1075,1076,1077,1078];1075;[441,442,443,444,445,446,447,448,449,450];339;826;[698,339];;503;[339,469];339;;[261,249,250,260];+indgen(5);[561,562,563,564];+indgen(50);indgen(20)+425;[336,337,338,339,340,341,342,343,344,345,346,347];949];428,
   
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   SAVE_LOC_HEAVY = env.SAVE_LOC_HEAVY
   if keyword_set(stime) then date_s = strmid(stime,0,4)+strmid(stime,5,2)+strmid(stime,8,2)+ $
                                       strmid(stime,11,2)+strmid(stime,14,2)+strmid(stime,17,2)
   filename = SAVE_LOC_HEAVY + '/maven/sav/vdf/event/'+string(orbit_arr,'(i05)')+'/vdf_'+date_s+'.sav'                                    

   nbin_acc = 800 & vres_sw = 0.1
   im_xy_avg = fltarr(nbin_acc+1,nbin_acc+1) & im_yz_avg = fltarr(nbin_acc+1,nbin_acc+1) & im_xz_avg = fltarr(nbin_acc+1,nbin_acc+1)  & im_paraperp_avg = fltarr(nbin_acc+1,nbin_acc/2.+1)
   im_xy_acc = fltarr(nbin_acc+1,nbin_acc+1) & im_yz_acc = fltarr(nbin_acc+1,nbin_acc+1) & im_xz_acc = fltarr(nbin_acc+1,nbin_acc+1) & im_paraperp_acc = fltarr(nbin_acc+1,nbin_acc/2.+1)
   N_im_xy_acc = fltarr(nbin_acc+1,nbin_acc+1) & N_im_yz_acc = fltarr(nbin_acc+1,nbin_acc+1) & N_im_xz_acc = fltarr(nbin_acc+1,nbin_acc+1) & N_im_paraperp_acc = fltarr(nbin_acc+1,nbin_acc/2.+1)
   xsc_arr = 0. & ysc_arr = 0. & zsc_arr = 0.
   
  
     
    ; filename = SAVE_LOC_HEAVY + '/maven/sav/vdf/event/'+string(orbit_arr[hh],'(i05)')+'/vdf_*.sav'
   
      restore,filename
      time = dat_sav.time & mtime = (time[0] + time[1])/2d
      nbin = dat_sav.nbin & vres = dat_sav.vres
      v_arr = (findgen(nbin+1)-nbin/2)*vres
      im_xy = dat_sav.im_xy & N_im_xy = dat_sav.N_im_xy & idx_exist_xy = where(N_im_xy ne 0)
      im_yz = dat_sav.im_yz & N_im_yz = dat_sav.N_im_yz & idx_exist_yz = where(N_im_yz ne 0)
      im_xz = dat_sav.im_xz & N_im_xz = dat_sav.N_im_xz & idx_exist_xz = where(N_im_xz ne 0)   
      im_paraperp = dat_sav.im_paraperp & N_im_paraperp = dat_sav.N_im_paraperp & idx_exist_paraperp = where(N_im_paraperp ne 0) 
     
      if idx_exist_xy[0] eq -1 and idx_exist_yz[0] eq -1 and idx_exist_xz[0] eq -1 then goto, noadd
     
      Vmso = dat_sav.Vmso 
      th_v = dat_sav.theta_v & th_v_std = dat_sav.theta_v_std & ph_v = dat_sav.phi_v & R_v = dat_sav.R_v ;& dVmso = dat_sav.Vmso_std
      Bmso = dat_sav.Bmso 
      th_b = dat_sav.theta_b & th_b_std = dat_sav.theta_b_std & ph_b = dat_sav.phi_b & R_b = dat_sav.R_b ;& dBmso = dat_sav.Bmso_std
      Emso = crossp(Bmso,Vmso) * 1e-6
      Vmso_tot = total(Vmso^2)^.5
      Vpara = inpro(Vmso,Bmso)/total(Bmso^2)^.5 & Vperp = sqrt(Vmso_tot^2 - Vpara^2)
      Vperp_tot = total(Vperp^2)^.5     
      pos = dat_sav.pos_mso
      rot_angle = get_rot_angle(Vmso,Bmso)
      pos_mse = mso2mse(pos[0],pos[1],pos[2],rot_angle)
      Bmse = mso2mse(Bmso[0],Bmso[1],Bmso[2],rot_angle)/total(Bmso^2)^.5
      Vmse = mso2mse(Vmso[0],Vmso[1],Vmso[2],rot_angle)/total(Vmso^2)^.5
      Emse = mso2mse(Emso[0],Emso[1],Emso[2],rot_angle)/total(Emso^2)^.5
      ExBmse = crossp(Emse,Bmse)
      Rm = 3389.9D
      if not keyword_set(mass) then mass = 16.
      sza = acos(pos[0]/total(pos^2)^.5)*!radeg
   
   
      if ~keyword_set(ymse_range) then ymse_range = [-1,1]
      ;if th_v_std*!radeg gt 20. or th_b_std*!radeg gt 20. or R_v lt 0.9 or R_b lt 0.9 then goto,noadd
      
      if finite(vperp_tot) ne 1 then goto, noadd
      if vperp_tot le 50. then goto, noadd
     
      if keyword_set(gyro_scl) then begin
         vbratio = Vperp_tot/total(Bmso^2)^.5       
         Rg = 166.8*vbratio
         ;print,vperp_tot,total(Bmso^2)^.5, vbratio,Rg
         if gyro_scl eq 1 then gyro_scl = 3389.9d
         if keyword_set(Large_Rg) then if Rg lt gyro_scl then goto,noadd
         if keyword_set(Small_Rg) then if Rg gt gyro_scl then goto,noadd 
      endif
      
      if keyword_set(select_region) then begin
        ;region = mvn_judge_region(pos[0],pos[1],pos[2])
        region = judge_mvn_boundary(mtime,orbit=orbit_arr[hh])
        if region ne select_region then goto,noadd
      endif     
      
      if keyword_set(near_bs) then begin
        judge_near_bs,mtime,dt=dt,orbit=orbit_arr[hh],ok=ok
        print,orbit_arr[hh],ok
        if ok eq 0 then goto, noadd
      endif
    
      if keyword_set(Ehemisphere) then begin
        ;print,Vmso,Bmso
        if Ehemisphere eq 'plus' then if pos_mse[2] lt 0 then goto,noadd
        if Ehemisphere eq 'minus' then if pos_mse[2] ge 0 then goto,noadd       
      endif
      
      if keyword_set(ymse_range) then begin
        if pos_mse[1] lt ymse_range[0] or pos_mse[1] gt ymse_range[1] then goto,noadd
      endif
      
      if keyword_set(day) then if pos[0] lt 0 then goto,noadd
      if keyword_set(night) then if pos[0] ge 0 then goto,noadd
      
      if keyword_set(sza_range) then begin
        if sza lt sza_range[0] or sza gt sza_range[1] then goto,noadd  
      endif
      
      
      xsc_arr = [xsc_arr,pos_mse[0]]
      ysc_arr = [ysc_arr,pos_mse[1]]
      zsc_arr = [zsc_arr,pos_mse[2]]
          
      ix=round(v_arr/vperp_tot/vres_sw)+nbin_acc/2.
      iy=round(v_arr/vperp_tot/vres_sw)+nbin_acc/2. 
      iz=round(v_arr/vperp_tot/vres_sw)+nbin_acc/2.
      ipara=round(v_arr/vperp_tot/vres_sw)+nbin_acc/2.
      iperp = round(v_arr[nbin/2.:*]/vperp_tot/vres_sw)
      ;print,fs[ii]
       ; for jj=0,nbin-1 do begin
       ;   for kk=0,nbin-1 do begin
           
           im_xy_acc = im_xy_acc + im_xy
           N_im_xy_acc = N_im_xy_acc + N_im_xy
           im_yz_acc = im_yz_acc + im_yz
           N_im_yz_acc = N_im_yz_acc + N_im_yz
           im_xz_acc = im_xz_acc + im_xz
           N_im_xz_acc = N_im_xz_acc + N_im_xz
           
          
           im_paraperp_acc = im_paraperp_acc + im_paraperp
           N_im_paraperp_acc = N_im_paraperp_acc + N_im_paraperp
             
           
           
         ; endfor  
          
        ;endfor 
      
;        idx_yz = where(N_im_yz_acc ne 0)
;        idx_yz_zero = where(N_im_yz_acc eq 0)
;        im_yz_avg[idx_yz] = im_yz_acc[idx_yz]/N_im_yz_acc[idx_yz]
;        im_yz_avg[idx_yz_zero] = !values.F_NaN
;        N_im_yz_acc[idx_yz_zero] = !values.F_NaN
;        plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,(findgen(nbin_acc+1)-nbin_acc/2.)*vres_sw,im_yz_avg,/zlog,xrange=[-5,5],yrange=[-5,5],zrange=[1e-5,1e7],xtit='Vexb',ytit='Ve',charsize=charsize
;        plots,circle_v2(1,0,1),color=1
;        plots,circle_v2(1,0,2),color=1
;      
;        im_yz_acc[*]=0. & im_yz_avg[*]=0. & N_im_yz_acc[*] = 0.
;stop
    noadd:
    
  
       

       
       
       loadct2,39
       erase
       wi,1
      !p.multi=[0,4,3]
      charsize=2
       ;;plot observed positions     
       xsc_arr = xsc_arr[1:*]
       ysc_arr = ysc_arr[1:*]
       zsc_arr = zsc_arr[1:*]
       Ryz_arr = sqrt(ysc_arr^2+zsc_arr^2)
 
       plot_mvn_orbit,/xy,/only_map,charsize=charsize,title='orbit: '+string(orbit_arr,format='(i05)')
       oplot,xsc_arr,ysc_arr,psym=1;,xrange=[-5,5],yrange=[-5,5],xtit='X',Ytit='Y',psym=1
       arrow,xsc_arr,ysc_arr,xsc_arr+Emse[0],ysc_arr+Emse[1],color=4,/data,hsize=.5
       arrow,xsc_arr,ysc_arr,xsc_arr+Bmse[0],ysc_arr+Bmse[1],color=2,/data,hsize=.5
       arrow,xsc_arr,ysc_arr,xsc_arr+Vmse[0],ysc_arr+Vmse[1],color=6,/data,hsize=.5
       arrow,xsc_arr,ysc_arr,xsc_arr+ExBmse[0],ysc_arr+ExBmse[1],color=210,/data,hsize=.5
       
       p1 = convert_coord([0, 0], /data, /to_normal)
       p2 = convert_coord([9, 9], /data, /to_normal)
       plot_mvn_orbit,/yz,/only_map,charsize=charsize,title='Pos_mse='+'('+string(pos_mse[0],format='(f5.2)')+','+string(pos_mse[1],format='(f5.2)')+','+string(pos_mse[2],format='(f5.2)')+')'
       oplot,ysc_arr,zsc_arr,psym=1;,xrange=[-5,5],yrange=[-5,5],xtit='Y',Ytit='Z',psym=1
       arrow,ysc_arr,zsc_arr,ysc_arr+Emse[1],zsc_arr+Emse[2],color=4,/data,hsize=.5
       arrow,ysc_arr,zsc_arr,ysc_arr+Bmse[1],zsc_arr+Bmse[2],color=2,/data,hsize=.5
       arrow,ysc_arr,zsc_arr,ysc_arr+Vmse[1],zsc_arr+Vmse[2],color=6,/data,hsize=.5
       arrow,ysc_arr,zsc_arr,ysc_arr+ExBmse[1],zsc_arr+ExBmse[2],color=210,/data,hsize=.5
       p3 = convert_coord([0, 0], /data, /to_normal)
       p4 = convert_coord([9, 9], /data, /to_normal)
       plot_mvn_orbit,/xz,/only_map,charsize=charsize,title='Alt='+string((sqrt((pos_mse[0]^2+pos_mse[1]^2+pos_mse[2]^2))-1.)*Rm,format='(f9.2)')+'km'
       oplot,xsc_arr,zsc_arr,psym=1;,xrange=[-5,5],yrange=[-5,5],xtit='X',Ytit='Z',psym=1
       arrow,xsc_arr,zsc_arr,xsc_arr+Emse[0],zsc_arr+Emse[2],color=4,/data,hsize=.5
       arrow,xsc_arr,zsc_arr,xsc_arr+Bmse[0],zsc_arr+Bmse[2],color=2,/data,hsize=.5
       arrow,xsc_arr,zsc_arr,xsc_arr+Vmse[0],zsc_arr+Vmse[2],color=6,/data,hsize=.5
       arrow,xsc_arr,zsc_arr,xsc_arr+ExBmse[0],zsc_arr+ExBmse[2],color=210,/data,hsize=.5
       p5 = convert_coord([0, 0], /data, /to_normal)
       p6 = convert_coord([9, 9], /data, /to_normal)
       plot_mvn_orbit,/cyl,/only_map,charsize=charsize,title='SZA: '+strtrim(string(round(sza)),2)+' deg'
       oplot,xsc_arr,Ryz_arr,psym=1;,xrange=[-5,5],yrange=[-5,5],xtit='X',Ytit='Ryz',psym=1
       p7 = convert_coord([0, 0], /data, /to_normal)
       p8 = convert_coord([9, 9], /data, /to_normal)
       
       
       
       ;;plot averaged vdf
       mult = '4,3'
       mpanel_imxy = '0,1'
       mpanel_Nxy = '1,1'
       mpanel_imyz = '2,1'
       mpanel_Nyz = '3,1'
       mpanel_imxz = '0,2'
       mpanel_Nxz = '1,2'
       mpanel_imparaperp = '2,2'
       mpanel_Nparaperp = '3,2'
       
       ;;calculate the sun direction in the bve frame
       sundir = spice_body_pos('SUN','MAVEN',utc=time_string(mtime),frame='MAVEN_MSO');;[1.,0.,0.]
       sundir_bve = mso2bve(sundir,Vmso,Bmso)
       sundir_bv = [sundir_bve[0],sundir_bve[1]]/total(sundir_bve[0]^2+sundir_bve[1]^2)^.5
       sundir_ve = [sundir_bve[1],sundir_bve[2]]/total(sundir_bve[1]^2+sundir_bve[2]^2)^.5
       sundir_be = [sundir_bve[0],sundir_bve[2]]/total(sundir_bve[0]^2+sundir_bve[2]^2)^.5

       
       
       ;;xy plane
       idx_xy = where(N_im_xy_acc ne 0)
       idx_xy_zero = where(N_im_xy_acc eq 0)
       im_xy_avg[idx_xy] = im_xy_acc[idx_xy]/N_im_xy_acc[idx_xy]      
       im_xy_avg[idx_xy_zero] = !values.F_NaN
       N_im_xy_acc[idx_xy_zero] = !values.F_NaN
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*2,(findgen(nbin_acc+1)-nbin_acc/2.)*2,im_xy_avg,/zlog,xrange=[-800,800],yrange=[-800,800],zrange=[1e3,1e6],mult=mult,mpanel=mpanel_imxy,xtit='Vb',ytit='Vexb',charsize=charsize,tit=time_string(time[0])+'-'+strmid(time_string(time[1]),11,8)
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       loadct2,0
       plots,circle_v2(0,0,mvn_ev2km(10.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(100.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(1000.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(10000.,mass)),color=180
       loadct2,39
       arrow,sundir_bv[0]*650,sundir_bv[1]*650,sundir_bv[0]*750,sundir_bv[1]*750,color=210,/data,hsize=3
       
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*2,(findgen(nbin_acc+1)-nbin_acc/2.)*2,N_im_xy_acc,xrange=[-800,800],yrange=[-800,800],zrange=[0,10],/add,mpanel=mpanel_Nxy,xtit='Vb',ytit='Vexb',charsize=charsize,tit=time_string(time[0])+'-'+strmid(time_string(time[1]),11,8)
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       loadct2,0
       plots,circle_v2(0,0,mvn_ev2km(10.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(100.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(1000.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(10000.,mass)),color=180
       loadct2,39
       
       
       ;;yz plane
       idx_yz = where(N_im_yz_acc ne 0)
       idx_yz_zero = where(N_im_yz_acc eq 0)
       im_yz_avg[idx_yz] = im_yz_acc[idx_yz]/N_im_yz_acc[idx_yz]       
       im_yz_avg[idx_yz_zero] = !values.F_NaN
       N_im_yz_acc[idx_yz_zero] = !values.F_NaN
       
       
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*2,(findgen(nbin_acc+1)-nbin_acc/2.)*2,im_yz_avg,/zlog,xrange=[-800,800],yrange=[-800,800],zrange=[1e3,1e7],/add,mpanel=mpanel_imyz,xtit='Vexb',ytit='Ve',charsize=charsize,tit=time_string(time[0])+'-'+strmid(time_string(time[1]),11,8)
       plots,circle_v2(vperp_tot,0,vperp_tot),color=230,line=2
       loadct2,0
       plots,circle_v2(0,0,mvn_ev2km(10.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(100.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(1000.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(10000.,mass)),color=180
       loadct2,39
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       surf_angle = get_imb_surf_angle(pos,Vmso,Bmso,IMB_pos_mso=IMB_pos_mso)
       idx_nan = where(finite(surf_angle) eq 1, ndat)
       if ndat ne 0 then begin
         dist_sc_imb_z = abs(pos[2] - IMB_pos_mso[2,*])
         idx = where(dist_sc_imb_z eq min(dist_sc_imb_z))
         zsurf = [surf_angle[idx]*(-800d),surf_angle[idx]*800d]
         oplot,[-800,800],zsurf,color=80,line=2
       endif
       arrow,sundir_ve[0]*650,sundir_ve[1]*650,sundir_ve[0]*750,sundir_ve[1]*750,color=210,/data,hsize=3
       
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*2,(findgen(nbin_acc+1)-nbin_acc/2.)*2,N_im_yz_acc,xrange=[-800,800],yrange=[-800,800],zrange=[0,10],/add,mpanel=mpanel_Nyz,xtit='Vexb',ytit='Ve',charsize=charsize,tit=time_string(time[0])+'-'+strmid(time_string(time[1]),11,8)
       plots,circle_v2(1,0,1),color=1
       ;plots,circle_v2(sqrt(2.),0,sqrt(2.)),color=1
       ;plots,circle_v2(sqrt(44./16.),0,sqrt(44./16.)),color=1
       plots,circle_v2(1,0,2),color=1
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       loadct2,0
       plots,circle_v2(0,0,mvn_ev2km(10.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(100.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(1000.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(10000.,mass)),color=180
       loadct2,39
       
       
       ;;xz plane
       idx_xz = where(N_im_xz_acc ne 0)
       idx_xz_zero = where(N_im_xz_acc eq 0)
       im_xz_avg[idx_xz] = im_xz_acc[idx_xz]/N_im_xz_acc[idx_xz]
       im_xz_avg[idx_xz_zero] = !values.F_NaN
       N_im_xz_acc[idx_xz_zero] = !values.F_NaN
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*2,(findgen(nbin_acc+1)-nbin_acc/2.)*2,im_xz_avg,/zlog,xrange=[-800,800],yrange=[-800,800],zrange=[1e3,1e7],/add,mpanel=mpanel_imxz,xtit='Vb',ytit='Ve',charsize=charsize,tit=time_string(time[0])+'-'+strmid(time_string(time[1]),11,8)
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       loadct2,0
       plots,circle_v2(0,0,mvn_ev2km(10.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(100.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(1000.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(10000.,mass)),color=180
       loadct2,39
       arrow,sundir_be[0]*650,sundir_be[1]*650,sundir_be[0]*750,sundir_be[1]*750,color=210,/data,hsize=3
       
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*2,(findgen(nbin_acc+1)-nbin_acc/2.)*2,N_im_xz_acc,xrange=[-800,800],yrange=[-800,800],zrange=[0,10],/add,mpanel=mpanel_Nxz,xtit='Vb',ytit='Ve',charsize=charsize,tit=time_string(time[0])+'-'+strmid(time_string(time[1]),11,8)
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       loadct2,0
       plots,circle_v2(0,0,mvn_ev2km(10.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(100.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(1000.,mass)),color=180
       plots,circle_v2(0,0,mvn_ev2km(10000.,mass)),color=180
       loadct2,39

       idx_paraperp = where(N_im_paraperp_acc ne 0)
       idx_paraperp_zero = where(N_im_paraperp_acc eq 0)
       im_paraperp_avg[idx_paraperp] = im_paraperp_acc[idx_paraperp]/N_im_paraperp_acc[idx_paraperp]
       im_paraperp_avg[idx_paraperp_zero] = !values.F_NaN
       N_im_paraperp_acc[idx_paraperp_zero] = !values.F_NaN
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*2,(findgen(nbin_acc/2.+1))*2,im_paraperp_avg,/zlog,xrange=[-800,800],yrange=[0,800],zrange=[1e3,1e7],/add,mpanel=mpanel_imparaperp,xtit='Vb',ytit='Ve',charsize=charsize,tit=time_string(time[0])+'-'+strmid(time_string(time[1]),11,8)
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       loadct2,0
       oplot,mvn_ev2km(10.,mass)*cos(findgen(180)*!DTOR),mvn_ev2km(10.,mass)*sin(findgen(180)*!DTOR),color=180
       oplot,mvn_ev2km(100.,mass)*cos(findgen(180)*!DTOR),mvn_ev2km(100.,mass)*sin(findgen(180)*!DTOR),color=180
       oplot,mvn_ev2km(1000.,mass)*cos(findgen(180)*!DTOR),mvn_ev2km(1000.,mass)*sin(findgen(180)*!DTOR),color=180
       oplot,mvn_ev2km(10000.,mass)*cos(findgen(180)*!DTOR),mvn_ev2km(10000.,mass)*sin(findgen(180)*!DTOR),color=180
       loadct2,39
       plotxyz,(findgen(nbin_acc+1)-nbin_acc/2.)*2,(findgen(nbin_acc/2.+1))*2,N_im_paraperp_acc,xrange=[-800,800],yrange=[0,800],zrange=[0,100],/add,mpanel=mpanel_Nparaperp,xtit='Vb',ytit='Ve',charsize=charsize,tit=time_string(time[0])+'-'+strmid(time_string(time[1]),11,8)
       oplot,!x.crange,[0,0],line=2
       oplot,[0,0],!y.crange,line=2
       loadct2,0
       oplot,mvn_ev2km(10.,mass)*cos(findgen(180)*!DTOR),mvn_ev2km(10.,mass)*sin(findgen(180)*!DTOR),color=180
       oplot,mvn_ev2km(100.,mass)*cos(findgen(180)*!DTOR),mvn_ev2km(100.,mass)*sin(findgen(180)*!DTOR),color=180
       oplot,mvn_ev2km(1000.,mass)*cos(findgen(180)*!DTOR),mvn_ev2km(1000.,mass)*sin(findgen(180)*!DTOR),color=180
       oplot,mvn_ev2km(10000.,mass)*cos(findgen(180)*!DTOR),mvn_ev2km(10000.,mass)*sin(findgen(180)*!DTOR),color=180
       loadct2,39

end

