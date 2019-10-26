pro plot_fmap,each_axis,f_xy,f_xz,f_yz,n_xy,n_xz,n_yz,tit=tit,energy=energy,frange=frange,nrange=nrange

  plotxyz,each_axis,each_axis,alog10(f_xy),mult='2,3',zrange=frange,xrange=[4,-4],yrange=[-4,4],tit='Flux ('+Energy+')',xtit='X',ytit='Y',ztit='flux [cm-2s-1]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/xz
  plotxyz,each_axis,each_axis,alog10(n_xy),/add,zrange=nrange,xrange=[4,-4],yrange=[-4,4],xtit='X',ytit='Y',ztit='measurements [#]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/xz

  plotxyz,each_axis,each_axis,alog10(f_xz),/add,zrange=frange,xrange=[4,-4],yrange=[-4,4],tit='Flux ('+Energy+')',xtit='X',ytit='Z',ztit='flux [cm-2s-1]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/xz
  plotxyz,each_axis,each_axis,alog10(n_xz),/add,zrange=nrange,xrange=[4,-4],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/xz

  plotxyz,each_axis,each_axis,alog10(f_yz),/add,zrange=frange,xrange=[-4,4],yrange=[-4,4],tit='Flux ('+Energy+')',xtit='Y',ytit='Z',ztit='flux [cm-2s-1]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/yz
  plotxyz,each_axis,each_axis,alog10(n_yz),/add,zrange=nrange,xrange=[-4,4],yrange=[-4,4],xtit='Y',ytit='Z',ztit='measurements [#]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/yz


end




pro plot_reflect_ion_map3,energy
   
   
   
 


    orbit_arr = 336+findgen(472);346;,347,348,349,350] ;+ findgen(472) 

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    
    nr = 16.
    nt = 18.
    
    
    res = 0.25
    range = 4.
    nbin = 2.*range/res     
    each_axis = (findgen(nbin+1) - nbin/2.)*res

;;difine xy bin
    f_xy_t = fltarr(nbin+1,nbin+1)
    f_xy_t2 = fltarr(nbin+1,nbin+1)
    f_xy_x = fltarr(nbin+1,nbin+1)
    f_xy_x2 = fltarr(nbin+1,nbin+1)
    f_xy_y = fltarr(nbin+1,nbin+1)
    f_xy_y2 = fltarr(nbin+1,nbin+1)
    f_xy_z = fltarr(nbin+1,nbin+1)
    f_xy_z2 = fltarr(nbin+1,nbin+1)

    v_xy_x = fltarr(nbin+1,nbin+1)
    v_xy_x2 = fltarr(nbin+1,nbin+1)
    v_xy_y = fltarr(nbin+1,nbin+1)
    v_xy_y2 = fltarr(nbin+1,nbin+1)
    v_xy_z = fltarr(nbin+1,nbin+1)
    v_xy_z2 = fltarr(nbin+1,nbin+1)
    n_xy = fltarr(nbin+1,nbin+1)

;;difine xz bin
    f_xz_t = fltarr(nbin+1,nbin+1)
    f_xz_t2 = fltarr(nbin+1,nbin+1)
    f_xz_x = fltarr(nbin+1,nbin+1)
    f_xz_x2 = fltarr(nbin+1,nbin+1)
    f_xz_y = fltarr(nbin+1,nbin+1)
    f_xz_y2 = fltarr(nbin+1,nbin+1)
    f_xz_z = fltarr(nbin+1,nbin+1)
    f_xz_z2 = fltarr(nbin+1,nbin+1) 
    
    v_xz_x = fltarr(nbin+1,nbin+1)
    v_xz_x2 = fltarr(nbin+1,nbin+1)
    v_xz_y = fltarr(nbin+1,nbin+1)
    v_xz_y2 = fltarr(nbin+1,nbin+1)
    v_xz_z = fltarr(nbin+1,nbin+1)
    v_xz_z2 = fltarr(nbin+1,nbin+1)  
    n_xz = fltarr(nbin+1,nbin+1)
 
;;difine yz bin   
    f_yz_t = fltarr(nbin+1,nbin+1)
    f_yz_t2 = fltarr(nbin+1,nbin+1)
    f_yz_x = fltarr(nbin+1,nbin+1)
    f_yz_x2 = fltarr(nbin+1,nbin+1)
    f_yz_y = fltarr(nbin+1,nbin+1)
    f_yz_y2 = fltarr(nbin+1,nbin+1)
    f_yz_z = fltarr(nbin+1,nbin+1)
    f_yz_z2 = fltarr(nbin+1,nbin+1)

    v_yz_x = fltarr(nbin+1,nbin+1)
    v_yz_x2 = fltarr(nbin+1,nbin+1)
    v_yz_y = fltarr(nbin+1,nbin+1)
    v_yz_y2 = fltarr(nbin+1,nbin+1)
    v_yz_z = fltarr(nbin+1,nbin+1)
    v_yz_z2 = fltarr(nbin+1,nbin+1)
    n_yz = fltarr(nbin+1,nbin+1)
   
    
    F_yz_map = fltarr(nbin+1,nbin+1)
    
    
    
    
    
    
    
  
    Rm = 3389.9D * 1e3 * 1e2 ;[cm]
    rflct_number = fltarr(18,72)
   
    for in=0,n_elements(orbit_arr)-1 do begin  
      
      
      filename = SAVE_LOC + '/maven/sav/flux/deflect_ion_151018_Vx/d0/'+string(orbit_arr[in],format='(i05)')+'/flux_vdf_*.sav'   
      fs = file_search(filename)
      if fs[0] eq '' then goto, no_data
       
      file_sw = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit_arr[in],format='(i05)')+'.sav'
      ft_sw = file_test(file_sw)
      if ft_sw eq 0 then goto, no_data
      restore,file_sw
      time_bs = dat_sw.time
      Vsw = avg(dat_sw.vel,1)
      Bsw = avg(dat_sw.mag_mso,1)
      fnt_vsw = finite(Vsw)
      fnt_bsw = finite(Bsw)
      
      for ii=0,n_elements(fs)-1 do begin
          
          restore,fs[ii]
          time = dat_sav.time
          mtime = total(time)/2d
          if mtime gt time_bs[0] and mtime lt time_bs[1] then goto, skip
          ;if mtime lt time_bs[0] or mtime gt time_bs[1] then goto,skip       
        ;  Vmso = dat_sav.Vmso
        ;  Bmso = dat_sav.Bmso
        ;  fnt_v = finite(Vmso)
        ;  fnt_b = finite(Bmso)
        ;  if fnt_v[0] eq 0 or fnt_v[1] eq 0 or fnt_v[2] eq 0 then goto, skip
        ;  if fnt_b[0] eq 0 or fnt_b[1] eq 0 or fnt_b[2] eq 0 then goto, skip
          ;rot = get_rot_angle(Vmso,Bmso)
          
          if fnt_vsw[0] eq 0 or fnt_vsw[1] eq 0 or fnt_vsw[2] eq 0 then goto, skip
          if fnt_bsw[0] eq 0 or fnt_bsw[1] eq 0 or fnt_bsw[2] eq 0 then goto, skip  
          rot = get_rot_angle(Vsw,Bsw) 
          ;rot = 0d
        ;; reflected ion density and velocity  
          Odens_20keV = dat_sav.Odens_20keV
          Ovel_mso_20keV = dat_sav.Ovel_mso_20keV
          Ovel_mse_20keV = mso2mse(Ovel_mso_20keV[0],Ovel_mso_20keV[1],Ovel_mso_20keV[2],rot)
          Oflux_mso_20keV = Odens_20keV * Ovel_mso_20keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_20keV = mso2mse(Oflux_mso_20keV[0],Oflux_mso_20keV[1],Oflux_mso_20keV[2],rot)
          
          
          Odens_10keV = dat_sav.Odens_10keV
          Ovel_mso_10keV = dat_sav.Ovel_mso_10keV   
          Ovel_mse_10keV = mso2mse(Ovel_mso_10keV[0],Ovel_mso_10keV[1],Ovel_mso_10keV[2],rot)   
          Oflux_mso_10keV = Odens_10keV * Ovel_mso_10keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_10keV = mso2mse(Oflux_mso_10keV[0],Oflux_mso_10keV[1],Oflux_mso_10keV[2],rot)       
          
          
          Odens_5keV = dat_sav.Odens_5keV
          Ovel_mso_5keV = dat_sav.Ovel_mso_5keV
          Ovel_mse_5keV = mso2mse(Ovel_mso_5keV[0],Ovel_mso_5keV[1],Ovel_mso_5keV[2],rot)
          Oflux_mso_5keV = Odens_5keV * Ovel_mso_5keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_5keV = mso2mse(Oflux_mso_5keV[0],Oflux_mso_5keV[1],Oflux_mso_5keV[2],rot)
          
          
          Odens_1keV = dat_sav.Odens_1keV
          Ovel_mso_1keV = dat_sav.Ovel_mso_1keV
          Ovel_mse_1keV = mso2mse(Ovel_mso_1keV[0],Ovel_mso_1keV[1],Ovel_mso_1keV[2],rot)
          Oflux_mso_1keV = Odens_1keV * Ovel_mso_1keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_1keV = mso2mse(Oflux_mso_1keV[0],Oflux_mso_1keV[1],Oflux_mso_1keV[2],rot)
          
          
          Odens_100eV = dat_sav.Odens_100eV
          Ovel_mso_100eV = dat_sav.Ovel_mso_100eV
          Ovel_mse_100eV = mso2mse(Ovel_mso_100eV[0],Ovel_mso_100eV[1],Ovel_mso_100eV[2],rot)
          Oflux_mso_100eV = Odens_100eV * Ovel_mso_100eV * 1e5 ;[#/cm2/sec]
          Oflux_mse_100eV = mso2mse(Oflux_mso_100eV[0],Oflux_mso_100eV[1],Oflux_mso_100eV[2],rot)
          
          
          Odens_all = dat_sav.Odens_all
          Ovel_mso_all = dat_sav.Ovel_mso_all
          Ovel_mse_all = mso2mse(Ovel_mso_all[0],Ovel_mso_all[1],Ovel_mso_all[2],rot)
          Oflux_mso_all = Odens_all * Ovel_mso_all * 1e5 ;[#/cm2/sec]
          Oflux_mse_all = mso2mse(Oflux_mso_all[0],Oflux_mso_all[1],Oflux_mso_all[2],rot)
          
          
          Odens_gt10keV = dat_sav.Odens_gt10keV
          Ovel_mso_gt10keV = dat_sav.Ovel_mso_gt10keV
          Ovel_mse_gt10keV = mso2mse(Ovel_mso_gt10keV[0],Ovel_mso_gt10keV[1],Ovel_mso_gt10keV[2],rot)
          Oflux_mso_gt10keV = Odens_gt10keV * Ovel_mso_gt10keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt10keV = mso2mse(Oflux_mso_gt10keV[0],Oflux_mso_gt10keV[1],Oflux_mso_gt10keV[2],rot)
             
          
          Odens_gt1keV = dat_sav.Odens_gt1keV
          Ovel_mso_gt1keV = dat_sav.Ovel_mso_gt1keV
          Ovel_mse_gt1keV = mso2mse(Ovel_mso_gt1keV[0],Ovel_mso_gt1keV[1],Ovel_mso_gt1keV[2],rot)
          Oflux_mso_gt1keV = Odens_gt1keV * Ovel_mso_gt1keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt1keV = mso2mse(Oflux_mso_gt1keV[0],Oflux_mso_gt1keV[1],Oflux_mso_gt1keV[2],rot)
              
          
          ;; precipitating ion density and velocity
          Odens_20keV_in = dat_sav.Odens_20keV_in
          Ovel_mso_20keV_in = dat_sav.Ovel_mso_20keV_in
          Ovel_mse_20keV_in = mso2mse(Ovel_mso_20keV_in[0],Ovel_mso_20keV_in[1],Ovel_mso_20keV_in[2],rot)
          Oflux_mso_20keV_in = Odens_20keV_in * Ovel_mso_20keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_20keV_in = mso2mse(Oflux_mso_20keV_in[0],Oflux_mso_20keV_in[1],Oflux_mso_20keV_in[2],rot)


          Odens_10keV_in = dat_sav.Odens_10keV_in
          Ovel_mso_10keV_in = dat_sav.Ovel_mso_10keV_in
          Ovel_mse_10keV_in = mso2mse(Ovel_mso_10keV_in[0],Ovel_mso_10keV_in[1],Ovel_mso_10keV_in[2],rot)
          Oflux_mso_10keV_in = Odens_10keV_in * Ovel_mso_10keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_10keV_in = mso2mse(Oflux_mso_10keV_in[0],Oflux_mso_10keV_in[1],Oflux_mso_10keV_in[2],rot)


          Odens_5keV_in = dat_sav.Odens_5keV_in
          Ovel_mso_5keV_in = dat_sav.Ovel_mso_5keV_in
          Ovel_mse_5keV_in = mso2mse(Ovel_mso_5keV_in[0],Ovel_mso_5keV_in[1],Ovel_mso_5keV_in[2],rot)
          Oflux_mso_5keV_in = Odens_5keV_in * Ovel_mso_5keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_5keV_in = mso2mse(Oflux_mso_5keV_in[0],Oflux_mso_5keV_in[1],Oflux_mso_5keV_in[2],rot)


          Odens_1keV_in = dat_sav.Odens_1keV_in
          Ovel_mso_1keV_in = dat_sav.Ovel_mso_1keV_in
          Ovel_mse_1keV_in = mso2mse(Ovel_mso_1keV_in[0],Ovel_mso_1keV_in[1],Ovel_mso_1keV_in[2],rot)
          Oflux_mso_1keV_in = Odens_1keV_in * Ovel_mso_1keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_1keV_in = mso2mse(Oflux_mso_1keV_in[0],Oflux_mso_1keV_in[1],Oflux_mso_1keV_in[2],rot)


          Odens_100eV_in = dat_sav.Odens_100eV_in
          Ovel_mso_100eV_in = dat_sav.Ovel_mso_100eV_in
          Ovel_mse_100eV_in = mso2mse(Ovel_mso_100eV_in[0],Ovel_mso_100eV_in[1],Ovel_mso_100eV_in[2],rot)
          Oflux_mso_100eV_in = Odens_100eV_in * Ovel_mso_100eV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_100eV_in = mso2mse(Oflux_mso_100eV_in[0],Oflux_mso_100eV_in[1],Oflux_mso_100eV_in[2],rot)


          Odens_all_in = dat_sav.Odens_all_in
          Ovel_mso_all_in = dat_sav.Ovel_mso_all_in
          Ovel_mse_all_in = mso2mse(Ovel_mso_all_in[0],Ovel_mso_all_in[1],Ovel_mso_all_in[2],rot)
          Oflux_mso_all_in = Odens_all_in * Ovel_mso_all_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_all_in = mso2mse(Oflux_mso_all_in[0],Oflux_mso_all_in[1],Oflux_mso_all_in[2],rot)


          Odens_gt10keV_in = dat_sav.Odens_gt10keV_in
          Ovel_mso_gt10keV_in = dat_sav.Ovel_mso_gt10keV_in
          Ovel_mse_gt10keV_in = mso2mse(Ovel_mso_gt10keV_in[0],Ovel_mso_gt10keV_in[1],Ovel_mso_gt10keV_in[2],rot)
          Oflux_mso_gt10keV_in = Odens_gt10keV_in * Ovel_mso_gt10keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt10keV_in = mso2mse(Oflux_mso_gt10keV_in[0],Oflux_mso_gt10keV_in[1],Oflux_mso_gt10keV_in[2],rot)


          Odens_gt1keV_in = dat_sav.Odens_gt1keV_in
          Ovel_mso_gt1keV_in = dat_sav.Ovel_mso_gt1keV_in
          Ovel_mse_gt1keV_in = mso2mse(Ovel_mso_gt1keV_in[0],Ovel_mso_gt1keV_in[1],Ovel_mso_gt1keV_in[2],rot)
          Oflux_mso_gt1keV_in = Odens_gt1keV_in * Ovel_mso_gt1keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt1keV_in = mso2mse(Oflux_mso_gt1keV_in[0],Oflux_mso_gt1keV_in[1],Oflux_mso_gt1keV_in[2],rot)
          
          
          pos_mso = dat_sav.pos_mso
          pos_mse = mso2mse(pos_mso[0],pos_mso[1],pos_mso[2],rot)
          pos_r = total(pos_mse^2)^.5
          pos_th = acos(pos_mse[0]/pos_r)
          if pos_mse[2] ge 0 then pos_ph = acos(pos_mse[1]/pos_r/sin(pos_th)) $
                             else pos_ph = -acos(pos_mse[1]/pos_r/sin(pos_th)) 
          
          if pos_mse[0] lt 1.8 or pos_mse[0] gt 2.2 then goto, skip
          if pos_mse[0] lt 0 then goto, skip
          
;      
                   
          i = round(pos_mse[0]/res) + nbin/2.
          j = round(pos_mse[1]/res) + nbin/2.
          k = round(pos_mse[2]/res) + nbin/2.
          
          
         if Energy eq '20keV' then begin
            Oflux_mse = Oflux_mse_20keV
            Ovel_mse = Ovel_mse_20keV
         endif
         
         if Energy eq '10keV' then begin
           Oflux_mse = Oflux_mse_10keV
           Ovel_mse = Ovel_mse_10keV
         endif
         
         if Energy eq '5keV' then begin
           Oflux_mse = Oflux_mse_5keV
           Ovel_mse = Ovel_mse_5keV
         endif
         
         if Energy eq '1keV' then begin
           Oflux_mse = Oflux_mse_1keV
           Ovel_mse = Ovel_mse_1keV
         endif
         
         if Energy eq '100eV' then begin
           Oflux_mse = Oflux_mse_100eV
           Ovel_mse = Ovel_mse_100eV
         endif
         
         if Energy eq 'all' then begin
           Oflux_mse = Oflux_mse_all
           Ovel_mse = Ovel_mse_all
         endif

         if Energy eq 'gt10keV' then begin
           Oflux_mse = Oflux_mse_gt10keV
           Ovel_mse = Ovel_mse_gt10keV
         endif
                
         if Energy eq 'gt1keV' then begin
           Oflux_mse = Oflux_mse_gt1keV
           Ovel_mse = Ovel_mse_gt1keV
         endif
         
         

         if Energy eq '20keV_in' then begin
           Oflux_mse = Oflux_mse_20keV_in
           Ovel_mse = Ovel_mse_20keV_in
         endif

         if Energy eq '10keV_in' then begin
           Oflux_mse = Oflux_mse_10keV_in
           Ovel_mse = Ovel_mse_10keV_in
         endif

         if Energy eq '5keV_in' then begin
           Oflux_mse = Oflux_mse_5keV_in
           Ovel_mse = Ovel_mse_5keV_in
         endif

         if Energy eq '1keV_in' then begin
           Oflux_mse = Oflux_mse_1keV_in
           Ovel_mse = Ovel_mse_1keV_in
         endif

         if Energy eq '100eV_in' then begin
           Oflux_mse = Oflux_mse_100eV_in
           Ovel_mse = Ovel_mse_100eV_in
         endif
         
         if Energy eq 'all_in' then begin
           Oflux_mse = Oflux_mse_all_in
           Ovel_mse = Ovel_mse_all_in
         endif

         if Energy eq 'gt10keV_in' then begin
           Oflux_mse = Oflux_mse_gt10keV_in
           Ovel_mse = Ovel_mse_gt10keV_in
         endif

         if Energy eq 'gt1keV_in' then begin
           Oflux_mse = Oflux_mse_gt1keV_in
           Ovel_mse = Ovel_mse_gt1keV_in
         endif


         
          
          ;f_xy_t[i,j] = f_xy_t[i,j] + total(Oflux_mse^2)^.5
          f_xy_x[i,j] = f_xy_x[i,j] + Oflux_mse[0]
          ;f_xy_y[i,j] = f_xy_y[i,j] + Oflux_mse[1]
          ;f_xy_z[i,j] = f_xy_z[i,j] + Oflux_mse[2]
          v_xy_x[i,j] = v_xy_x[i,j] + Ovel_mse[0]
          ;v_xy_y[i,j] = v_xy_y[i,j] + Ovel_mse[1]
          ;v_xy_z[i,j] = v_xy_z[i,j] + Ovel_mse[2]
          n_xy[i,j] = n_xy[i,j] + 1.
                   
          
          ;f_xz_t[i,k] = f_xz_t[i,k] + total(Oflux_mse^2)^.5
          f_xz_x[i,k] = f_xz_x[i,k] + Oflux_mse[0]
          ;f_xz_y[i,k] = f_xz_y[i,k] + Oflux_mse[1]
          ;f_xz_z[i,k] = f_xz_z[i,k] + Oflux_mse[2]
          v_xz_x[i,k] = v_xz_x[i,k] + Ovel_mse[0]
          ;v_xz_y[i,k] = v_xz_y[i,k] + Ovel_mse[1]
          ;v_xz_z[i,k] = v_xz_z[i,k] + Ovel_mse[2]
          n_xz[i,k] = n_xz[i,k] + 1.
          
          ;f_yz_t[j,k] = f_yz_t[j,k] + total(Oflux_mse^2)^.5 
          f_yz_x[j,k] = f_yz_x[j,k] + Oflux_mse[0]     
          ;f_yz_y[j,k] = f_yz_y[j,k] + Oflux_mse[1]
          ;f_yz_z[j,k] = f_yz_z[j,k] + Oflux_mse[2]
          v_yz_x[j,k] = v_yz_x[j,k] + Ovel_mse[0]
          ;v_yz_y[j,k] = v_yz_y[j,k] + Ovel_mse[1]
          ;v_yz_z[j,k] = v_yz_z[j,k] + Ovel_mse[2]
          n_yz[j,k] = n_yz[j,k] + 1.
         
         skip:
      endfor   
      no_data:
    endfor
;    stop
;    idx = where(n_tp ne 0)
;    f_tp_avg[idx] = f_tp[idx]/n_tp[idx]
;    plotxyz,phi_dash_bin*!radeg,theta_dash_bin*!radeg,alog10(transpose(f_tp_avg)),xtit='phi',ytit='theta',multi='1,2',charsize=2,zrange=[0,4]
;    oplot,[0,0],!y.crange,line=2
;    plotxyz,phi_dash_bin*!radeg,theta_dash_bin*!radeg,alog10(transpose(n_tp)),xtit='phi',ytit='theta',/add,charsize=2,zrange=[0,3]
;    oplot,[0,0],!y.crange,line=2
;    
;    for j=0, 71 do begin
;      for i=0,17 do begin
;       if i eq 0 then rflct_number[i,j] = f_tp_avg[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
;       if i gt 0 then rflct_number[i,j] =  f_tp_avg[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
;      endfor
;    endfor
;    
;    print,total(rflct_number)
    
   
    
    !p.multi=[0,2,1]
    
    
    idx_xy = where(n_xy ne 0)
    f_xy_t2[idx_xy] = f_xy_t[idx_xy]/n_xy[idx_xy]
    f_xy_x2[idx_xy] = f_xy_x[idx_xy]/n_xy[idx_xy]
    f_xy_y2[idx_xy] = f_xy_y[idx_xy]/n_xy[idx_xy]
    f_xy_z2[idx_xy] = f_xy_z[idx_xy]/n_xy[idx_xy]
    v_xy_x2[idx_xy] = v_xy_x[idx_xy]/n_xy[idx_xy]
    v_xy_y2[idx_xy] = v_xy_y[idx_xy]/n_xy[idx_xy]
    v_xy_z2[idx_xy] = v_xy_z[idx_xy]/n_xy[idx_xy]
    
    
    idx_xz = where(n_xz ne 0)
    f_xz_t2[idx_xz] = f_xz_t[idx_xz]/n_xz[idx_xz]
    f_xz_x2[idx_xz] = f_xz_x[idx_xz]/n_xz[idx_xz]
    f_xz_y2[idx_xz] = f_xz_y[idx_xz]/n_xz[idx_xz]
    f_xz_z2[idx_xz] = f_xz_z[idx_xz]/n_xz[idx_xz]
    v_xz_x2[idx_xz] = v_xz_x[idx_xz]/n_xz[idx_xz]
    v_xz_y2[idx_xz] = v_xz_y[idx_xz]/n_xz[idx_xz]
    v_xz_z2[idx_xz] = v_xz_z[idx_xz]/n_xz[idx_xz]

    
    idx_yz = where(n_yz ne 0)
    f_yz_t2[idx_yz] = f_yz_t[idx_yz]/n_yz[idx_yz]
    f_yz_x2[idx_yz] = f_yz_x[idx_yz]/n_yz[idx_yz]
    f_yz_y2[idx_yz] = f_yz_y[idx_yz]/n_yz[idx_yz]
    f_yz_z2[idx_yz] = f_yz_z[idx_yz]/n_yz[idx_yz]
    v_yz_x2[idx_yz] = v_yz_x[idx_yz]/n_yz[idx_yz]
    v_yz_y2[idx_yz] = v_yz_y[idx_yz]/n_yz[idx_yz]
    v_yz_z2[idx_yz] = v_yz_z[idx_yz]/n_yz[idx_yz]
    
    
   
    if energy eq '100eV_in' $
    or energy eq '1keV_in' $
    or energy eq '5keV_in' $
    or energy eq '10keV_in' $
    or energy eq '20keV_in' $
    or energy eq 'all_in' $
    or energy eq 'gt10keV_in' $
    or energy eq 'gt1keV_in' then begin
        f_xy_x2 = -f_xy_x2
        f_xz_x2 = -f_xz_x2
        f_yz_x2 = -f_yz_x2
        frange = [3,6]
    endif else begin
        frange = [1,4]
    endelse
    
    plot_fmap,each_axis,f_xy_x2,f_xz_x2,f_yz_x2,n_xy,n_xz,n_yz,energy=energy,frange=frange,nrange=[0,2]
    r_bs = 2.62
    F_int_all = total(f_yz_x2*(res*3389.9*1e3*1e2)^2)
    print,'Rate_YZ_all: ',F_int_all+' [#/s]'
    
    ;for i=(2.*4./res)/4.,2*(2.*4./res)/4. do begin
     for i=6,26 do begin
      j = sqrt((r_bs/res+1.)^2-(i-(2.*4./res)/2.)^2)+(2.*4./res)/2.-0.5
      k = -sqrt((r_bs/res+1.)^2-(i-(2.*4./res)/2.)^2)+(2.*4./res)/2.+0.5
      if j lt 2.*4./res/2. then j2 = ceil(j)   ;X<0, Y>0
      if j gt 2.*4./res/2. then j2 = floor(j)  ;X>0, Y>0
      if k lt 2.*4./res/2. then k2 = ceil(k)   ;X<0, Y<0
      if k gt 2.*4./res/2. then k2 = floor(k)  ;X>0, Y<0
      for l=k2,j2 do begin
        F_yz_map[i,l] = f_yz_x2[i,l]
      endfor
    endfor
    
    F_int = total(F_yz_map*(res*3389.9*1e3*1e2)^2)
    print,'Rate_YZ_limited_area: ',F_int+' [#/s]'
    
    stop
    plotxyz,each_axis,each_axis,f_xz_t2,mult='2,3',zrange=[0,1000],xrange=[4,0],yrange=[-4,4],tit='Flux (total)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    plotxyz,each_axis,each_axis,n_xz,/add,zrange=[0,100],xrange=[4,0],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    
    
    plotxyz,each_axis,each_axis,f_yz_t2,/add,zrange=[0,1000],xrange=[-4,4],yrange=[-4,4],tit='Flux (total)',xtit='Y',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    plotxyz,each_axis,each_axis,n_yz,/add,zrange=[0,100],xrange=[-4,4],yrange=[-4,4],xtit='Y',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    
    plotxyz,each_axis,each_axis,f_yz_t2,/add,zrange=[0,1000],xrange=[-4,4],yrange=[-4,4],tit='Flux (total)',xtit='Y',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    plotxyz,each_axis,each_axis,n_yz,/add,zrange=[0,100],xrange=[-4,4],yrange=[-4,4],xtit='Y',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/yz

    stop
    
    plotxyz,each_axis,each_axis,f_xz_x2,mult='2,2',zrange=[0,1000],xrange=[4,0],yrange=[-4,4],tit='Flux (X dir)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    velovect,f_xz_x2,f_xz_z2,each_axis,each_axis,/over
    plotxyz,each_axis,each_axis,n_xz,/add,zrange=[0,100],xrange=[4,0],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/xz

    plotxyz,each_axis,each_axis,f_xz_z2,/add,zrange=[-1000,1000],xrange=[4,0],yrange=[-4,4],tit='Flux (Z dir)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    velovect,f_xz_x,f_xz_z,each_axis,each_axis,/over
    plotxyz,each_axis,each_axis,n_xz,/add,zrange=[0,100],xrange=[4,0],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/xz

    stop
    
    plotxyz,each_axis,each_axis,f_yz_y2,mult='2,2',zrange=[-1000,1000],xrange=[-4,4],yrange=[-4,4],tit='Flux (Y dir)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    velovect,f_yz_y2,f_yz_z2,each_axis,each_axis,/over
    plotxyz,each_axis,each_axis,n_yz,/add,zrange=[0,100],xrange=[-4,4],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/yz

    plotxyz,each_axis,each_axis,f_yz_z2,/add,zrange=[-1000,1000],xrange=[-4,4],yrange=[-4,4],tit='Flux (Z dir)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    velovect,f_yz_y2,f_yz_z2,each_axis,each_axis,/over
    plotxyz,each_axis,each_axis,n_yz,/add,zrange=[0,100],xrange=[-4,4],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    
    stop


    plot_bs_imb_mars,/edb,/xz
    velovect,v_xz_x,v_xz_z,each_axis,each_axis,/over
    !p.multi=[0,2,1]

    plot_bs_imb_mars,/edb,/xz
    velovect,v_xz_x2,v_xz_z2,each_axis,each_axis,/over,length=2.
    
    plot_bs_imb_mars,/edb,/yz
    velovect,v_yz_y2,v_yz_z2,each_axis,each_axis,/over,length=2.
    
stop
end