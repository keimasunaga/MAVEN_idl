pro plot_fmap,each_axis,f_xy,f_xz,f_yz,n_xy,n_xz,n_yz,tit=tit,energy=energy,frange=frange,nrange=nrange

  ;loadct2,60,file='/Applications/exelis/idl84/resource/colors/colors2.tbl' 
  plotxyz,each_axis,each_axis,alog10(f_xy),mult='2,3',zrange=frange,xrange=[4,-4],yrange=[-4,4],tit='Flux ('+Energy+')',xtit='X',ytit='Y',ztit='flux [cm-2s-1]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/xz
  ;loadct2,61,file='/Applications/exelis/idl84/resource/colors/colors2.tbl' 
  plotxyz,each_axis,each_axis,alog10(n_xy),/add,zrange=nrange,xrange=[4,-4],yrange=[-4,4],xtit='X',ytit='Y',ztit='measurements [#]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/xz

  ;loadct2,49,file='/Applications/exelis/idl84/resource/colors/colors2.tbl' 
  plotxyz,each_axis,each_axis,alog10(f_xz),/add,zrange=frange,xrange=[4,-4],yrange=[-4,4],tit='Flux ('+Energy+')',xtit='X',ytit='Z',ztit='flux [cm-2s-1]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/xz
  ;loadct2,50,file='/Applications/exelis/idl84/resource/colors/colors2.tbl' 
  plotxyz,each_axis,each_axis,alog10(n_xz),/add,zrange=nrange,xrange=[4,-4],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/xz

  ;loadct2,39,file='/Applications/exelis/idl84/resource/colors/colors2.tbl' 
  plotxyz,each_axis,each_axis,alog10(f_yz),/add,zrange=frange,xrange=[-4,4],yrange=[-4,4],tit='Flux ('+Energy+')',xtit='Y',ytit='Z',ztit='flux [cm-2s-1]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/yz
  ;loadct2,55,file='/Applications/exelis/idl84/resource/colors/colors2.tbl' 
  plotxyz,each_axis,each_axis,alog10(n_yz),/add,zrange=nrange,xrange=[-4,4],yrange=[-4,4],xtit='Y',ytit='Z',ztit='measurements [#]',charsize=1.5
  plot_bs_imb_mars,/edb,/oplot,/yz


end




pro plot_reflect_ion_map10_v3,energy,vdf_type=vdf_type,surf_type=surf_type,frange=frange,fregion=fregion,$
                          Rg_mean=Rg_mean,Vmso_perp_mean=Vmso_perp_mean,Bt_mean=Bt_mean,Pdyn_mean=Pdyn_mean,larger=larger,smaller=smaller,$
                          make_notes=make_notes,journal_file=journal_file
   
   

    orbit_arr = [317+findgen(4)];[317+findgen(450)];[317+findgen(500),1470+findgen(500), 2423+findgen(600)];346;,347,348,349,350] ;+ findgen(472)   
    zrange = [1e0,1e5] 
    zrange_n = [1e0,1e3]
    
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    !p.multi=[0,1,1]    
    
    ;;Define variables for a flux plot against the shock surface
    Rm = 3389.9D * 1e3 * 1e2 ;[cm]
    L = 2.1
    e = 1.05
    X0 = 0.55
    theta_arr = findgen(103)*!DTOR   ;; Angle from the X axis. Note that in this coordinate system the origin is a focus of the hyperboric curve
    r_arr = L/(1.+e*cos(theta_arr))  ;; Distance to the shock surface from the focus of the hyperboric curve
    X = r_arr * cos(theta_arr) ;L*cos(theta_arr)/(1.+e*cos(theta_arr))
    Y = r_arr * sin(theta_arr) ;L*sin(theta_arr)/(1.+e*cos(theta_arr))
    rth_bin = fltarr(18) & rph_bin = fltarr(18)
    phi_dash_bin = ((findgen(72)*5. + 2.5) - 180.)*!DTOR
    phi_dash_bin_edge = ((findgen(73)*5.) - 180.)*!DTOR
    theta_dash_bin = (findgen(18)*5 + 2.5)*!DTOR  ;; Angle from the X axis with a even gap of 5 degree. Note that in this coordinate system the origin is a center of Mars
    theta_dash_bin_edge = findgen(19)*5.*!DTOR    ;; Array of edge of the theta' array with a even gap of 5 degree.
    
    thres = 5 * !DTOR  ;;Resolutin of Theta' [Degree]
    phres = 5 * !DTOR  ;;Resolution of Phi' [Degree]
      
    ;;calculate r' (Distance to the shock surface from the center of Mars)
    theta_dash_arr = acos((X0+X)/sqrt((X0+X)^2+Y^2)) ;; Angle from the X axis (data points correspnd to those of theta_arr)
    r_dash = (X0 + r_arr*cos(theta_arr))/cos(theta_dash_arr) ;;r'      ;(X0 + r_arr*cos(theta_arr))/cos(theta_dash_arr)

    ;;r' has to be interpolated by data points of theta_dash_bin because data points of r' are correspoinding to those of theta_dash_arr
    r_dash_bin = interpol(r_dash,theta_dash_arr,theta_dash_bin)
    r_dash_bin_edge = interpol(r_dash,theta_dash_arr,theta_dash_bin_edge)


    for i=0,n_elements(r_dash_bin)-1 do rth_bin[i] = sqrt(r_dash_bin_edge[i]^2 + r_dash_bin_edge[i+1]^2 - 2.*r_dash_bin_edge[i]*r_dash_bin_edge[i+1]*cos(5.*!DTOR))
    for i=0,n_elements(r_dash_bin)-1 do rph_bin[i] = r_dash_bin_edge[i]*sin(theta_dash_bin[i])*(5.*!DTOR)

 
    nth = !pi/2./thres
    nph = 2.*!pi/thres
    
    f_tp = fltarr(nth,nph) & f_tp_avg = fltarr(nth,nph)
    n_tp = fltarr(nth,nph)
    rflct_number = fltarr(18,72)
   
  
    ;;Define bin for a x-ward flux plot of each (xy, xz, and yz) surface
    r_bs = 2.62
    res = 0.25
    range = 4.
    nbin = 2.*range/res
    each_axis = (findgen(nbin+1) - nbin/2.)*res
    
    ;;define xy bin
    f_xy_x = fltarr(nbin+1,nbin+1)
    f_xy_x2 = fltarr(nbin+1,nbin+1)
    v_xy_x = fltarr(nbin+1,nbin+1)
    v_xy_x2 = fltarr(nbin+1,nbin+1)
    n_xy = fltarr(nbin+1,nbin+1)

    ;;define xz bin
    f_xz_x = fltarr(nbin+1,nbin+1)
    f_xz_x2 = fltarr(nbin+1,nbin+1)
    v_xz_x = fltarr(nbin+1,nbin+1)
    v_xz_x2 = fltarr(nbin+1,nbin+1)
    n_xz = fltarr(nbin+1,nbin+1)
      
    ;;define yz bin
    f_yz_x = fltarr(nbin+1,nbin+1)
    f_yz_x2 = fltarr(nbin+1,nbin+1)
    v_yz_x = fltarr(nbin+1,nbin+1)
    v_yz_x2 = fltarr(nbin+1,nbin+1)
    n_yz = fltarr(nbin+1,nbin+1)
    F_yz_map = fltarr(nbin+1,nbin+1)
   
   bflg_p = 0.
   bflg_m = 0.
   cone_angle_arr = 0.
   clock_angle_arr = 0.
   rg_arr = 0.
   Vmso_perp_arr = 0.
   Bt_arr = 0.
   Pdyn_arr = 0.
      
    ;; start sorting flux into bins
    for in=0,n_elements(orbit_arr)-1 do begin  
      
      print,orbit_arr[in]
      
      filename = SAVE_LOC + '/maven/sav/flux/deflect_ion_rmv_ring/d0/'+string(orbit_arr[in],format='(i05)')+'/flux_vdf_*.sav'   
      fs = file_search(filename)
      if fs[0] eq '' then goto, no_data
      
      file_sw = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit_arr[in],format='(i05)')+'.sav'
      ft_sw = file_test(file_sw)
      if ft_sw eq 0 then goto, no_data
      restore,file_sw
      time_bs = dat_sw.time 
      
      for ii=0,n_elements(fs)-1 do begin
          
          restore,fs[ii]
          if vdf_type eq 'shock_normal' then begin
             dat_flux_out = dat_sav.vdf_sav_out_sh
             dat_flux_in = dat_sav.vdf_sav_in_sh
          endif
          
          if vdf_type eq 'xdir' then begin
             dat_flux_out = dat_sav.vdf_sav_out_x
             dat_flux_in = dat_sav.vdf_sav_in_x
          endif
          
          if vdf_type eq 'xzdir' then begin
             dat_flux_out = dat_sav.vdf_sav_out_x
             dat_flux_in = dat_sav.vdf_sav_in_xz
          endif     
          
          time = dat_flux_out.time
          mtime = total(time)/2d
          if mtime gt time_bs[0] and mtime lt time_bs[1] then goto, skip
          ;if mtime lt time_bs[0] or mtime gt time_bs[1] then goto,skip       
          Nsw = dat_flux_out.dens
          Vmso = dat_flux_out.Vmso
          Bmso = dat_flux_out.Bmso         
          fnt_v = finite(Vmso)
          fnt_b = finite(Bmso)
          if fnt_v[0] eq 0 or fnt_v[1] eq 0 or fnt_v[2] eq 0 then goto, skip
          if fnt_b[0] eq 0 or fnt_b[1] eq 0 or fnt_b[2] eq 0 then goto, skip
          
          Bt = norm(Bmso)
          Bt_arr = [Bt_arr,Bt]
          
          Emso = -crossp(Vmso,Bmso)*1e-6
          Et = norm(Emso)         
          
          theta = acos(inpro(Vmso,Bmso)/norm(Vmso)/norm(Bmso))
          Vmso_perp = norm(Vmso)*sin(theta)
          Vmso_perp_arr = [Vmso_perp_arr,Vmso_perp]
          
          Gyro_ratio = Vmso_perp*1e3/norm(Bmso)/1e-9
          Gyro_radii = 16*1.67e-27/1.6e-19*Gyro_ratio/1e3
          Rg_arr = [Rg_arr,Gyro_radii]
          
          Pdyn = 1.67*Nsw*norm(Vmso)^2*1e-6
          Pdyn_arr = [Pdyn_arr,Pdyn]
          
          if keyword_set(Rg_mean) then begin
            if keyword_set(larger) then if Gyro_radii lt 11754.954 then goto, skip ;15140.626 11489.402 then goto,skip
            if keyword_set(smaller) then if Gyro_radii ge 11754.954 then goto, skip ;15140.626 11489.402then goto,skip
          endif
          if keyword_set(Vmso_perp_mean) then begin
            if keyword_set(larger) then if Vmso_perp lt 309.23590 then goto, skip ;299.60058 then goto,skip
            if keyword_set(smaller) then if Vmso_perp ge 309.23590 then goto, skip ;299.60058 then goto,skip
          endif
          if keyword_set(Bt_mean) then begin
            if keyword_set(larger) then if Bt lt 3.1389064 then goto, skip ;3.6122810 then goto,skip
            if keyword_set(smaller) then if Bt ge 3.1389064 then goto, skip ;3.6122810 then goto,skip
          endif
          if keyword_set(Pdyn_mean) then begin
            if keyword_set(larger) then if Bt lt 0.60362769 then goto, skip ;0.81145722 then goto,skip
            if keyword_set(smaller) then if Bt ge 0.60362769 then goto, skip ;0.81145722 then goto,skip
          endif
          rot = get_rot_angle(Vmso,Bmso)
          
          pos_mso = dat_flux_out.pos_mso
          pos_mse = mso2mse(pos_mso[0],pos_mso[1],pos_mso[2],rot)
          pos_r = total(pos_mse^2)^.5
          pos_th = acos(pos_mse[0]/pos_r)
          if pos_mse[2] ge 0 then pos_ph = acos(pos_mse[1]/pos_r/sin(pos_th)) $
          else pos_ph = -acos(pos_mse[1]/pos_r/sin(pos_th))
          if pos_mse[0] lt 0 then goto, skip
          ;if pos_mse[0] gt 2.2 or pos_mse[0] lt 1.7 then goto,skip
          norm_sh = get_norm_shock3(pos_mse,ind=ind,theta=theta_ex,phi=phi_ex)
          ;stop
          ith = ind[0]
          iph = ind[1]
;           norm_sh = [1.,0.,0.]
;           ith = 0
;           iph = 36
          
          ix = round(pos_mse[0]/res) + nbin/2.
          iy = round(pos_mse[1]/res) + nbin/2.
          iz = round(pos_mse[2]/res) + nbin/2.
          
          if ith ge 18 then goto, skip
  
          if Bmso[0] ge 0 then bflg_p = bflg_p + 1. else bflg_m = bflg_m + 1.
          cone_angle = acos(bmso[0]/norm(bmso))*!radeg
          cone_angle_arr = [cone_angle_arr,cone_angle]
          if bmso[2] gt 0 then clock_angle = acos(bmso[1]/sqrt(bmso[1]^2+bmso[2]^2))*!radeg else clock_angle = -acos(bmso[1]/sqrt(bmso[1]^2+bmso[2]^2))*!radeg
          clock_angle_arr = [clock_angle_arr, clock_angle]
         
;          if cone_angle gt 90 then goto, skip
;          if clock_angle lt -135 or clock_angle gt 135 then goto, skip
          
          ;if (clock_angle gt -45 and clock_angle lt 45) or (clock_angle gt -135 and clock_angle lt 135) then begin 
          ;if  (((clock_angle gt -180 and clock_angle lt -135) or (clock_angle gt 135 and clock_angle lt 180)) and cone_angle lt 90) or (((clock_angle gt -45 and clock_angle lt 45) and cone_angle gt 90)) then begin 
          ;if  (((clock_angle gt -180 and clock_angle lt -135) or (clock_angle gt 135 and clock_angle lt 180)) and cone_angle gt 90) or (((clock_angle gt -45 and clock_angle lt 45) and cone_angle lt 90)) then begin
              
          Odens_20keV = dat_flux_out.Odens_20keV
          Ovel_mso_20keV = dat_flux_out.Ovel_mso_20keV
          Ovel_mse_20keV = mso2mse(Ovel_mso_20keV[0],Ovel_mso_20keV[1],Ovel_mso_20keV[2],rot)
          Oflux_mso_20keV = Odens_20keV * Ovel_mso_20keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_20keV = mso2mse(Oflux_mso_20keV[0],Oflux_mso_20keV[1],Oflux_mso_20keV[2],rot)
          
          
          Odens_10keV = dat_flux_out.Odens_10keV
          Ovel_mso_10keV = dat_flux_out.Ovel_mso_10keV   
          Ovel_mse_10keV = mso2mse(Ovel_mso_10keV[0],Ovel_mso_10keV[1],Ovel_mso_10keV[2],rot)   
          Oflux_mso_10keV = Odens_10keV * Ovel_mso_10keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_10keV = mso2mse(Oflux_mso_10keV[0],Oflux_mso_10keV[1],Oflux_mso_10keV[2],rot)       
          
          
          Odens_5keV = dat_flux_out.Odens_5keV
          Ovel_mso_5keV = dat_flux_out.Ovel_mso_5keV
          Ovel_mse_5keV = mso2mse(Ovel_mso_5keV[0],Ovel_mso_5keV[1],Ovel_mso_5keV[2],rot)
          Oflux_mso_5keV = Odens_5keV * Ovel_mso_5keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_5keV = mso2mse(Oflux_mso_5keV[0],Oflux_mso_5keV[1],Oflux_mso_5keV[2],rot)
          
          
          Odens_1keV = dat_flux_out.Odens_1keV
          Ovel_mso_1keV = dat_flux_out.Ovel_mso_1keV
          Ovel_mse_1keV = mso2mse(Ovel_mso_1keV[0],Ovel_mso_1keV[1],Ovel_mso_1keV[2],rot)
          Oflux_mso_1keV = Odens_1keV * Ovel_mso_1keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_1keV = mso2mse(Oflux_mso_1keV[0],Oflux_mso_1keV[1],Oflux_mso_1keV[2],rot)
          
          
          Odens_100eV = dat_flux_out.Odens_100eV
          Ovel_mso_100eV = dat_flux_out.Ovel_mso_100eV
          Ovel_mse_100eV = mso2mse(Ovel_mso_100eV[0],Ovel_mso_100eV[1],Ovel_mso_100eV[2],rot)
          Oflux_mso_100eV = Odens_100eV * Ovel_mso_100eV * 1e5 ;[#/cm2/sec]
          Oflux_mse_100eV = mso2mse(Oflux_mso_100eV[0],Oflux_mso_100eV[1],Oflux_mso_100eV[2],rot)
          
          
          Odens_all = dat_flux_out.Odens_all
          Ovel_mso_all = dat_flux_out.Ovel_mso_all
          Ovel_mse_all = mso2mse(Ovel_mso_all[0],Ovel_mso_all[1],Ovel_mso_all[2],rot)
          Oflux_mso_all = Odens_all * Ovel_mso_all * 1e5 ;[#/cm2/sec]
          Oflux_mse_all = mso2mse(Oflux_mso_all[0],Oflux_mso_all[1],Oflux_mso_all[2],rot)
          
          
          Odens_gt10keV = dat_flux_out.Odens_gt10keV
          Ovel_mso_gt10keV = dat_flux_out.Ovel_mso_gt10keV
          Ovel_mse_gt10keV = mso2mse(Ovel_mso_gt10keV[0],Ovel_mso_gt10keV[1],Ovel_mso_gt10keV[2],rot)
          Oflux_mso_gt10keV = Odens_gt10keV * Ovel_mso_gt10keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt10keV = mso2mse(Oflux_mso_gt10keV[0],Oflux_mso_gt10keV[1],Oflux_mso_gt10keV[2],rot)
             
         
          Odens_gt5keV = dat_flux_out.Odens_gt5keV
          Ovel_mso_gt5keV = dat_flux_out.Ovel_mso_gt5keV
          Ovel_mse_gt5keV = mso2mse(Ovel_mso_gt5keV[0],Ovel_mso_gt5keV[1],Ovel_mso_gt5keV[2],rot)
          Oflux_mso_gt5keV = Odens_gt5keV * Ovel_mso_gt5keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt5keV = mso2mse(Oflux_mso_gt5keV[0],Oflux_mso_gt5keV[1],Oflux_mso_gt5keV[2],rot)
          
          
          Odens_gt1keV = dat_flux_out.Odens_gt1keV
          Ovel_mso_gt1keV = dat_flux_out.Ovel_mso_gt1keV
          Ovel_mse_gt1keV = mso2mse(Ovel_mso_gt1keV[0],Ovel_mso_gt1keV[1],Ovel_mso_gt1keV[2],rot)
          Oflux_mso_gt1keV = Odens_gt1keV * Ovel_mso_gt1keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt1keV = mso2mse(Oflux_mso_gt1keV[0],Oflux_mso_gt1keV[1],Oflux_mso_gt1keV[2],rot)
              
          
          ;; precipitating ion density and velocity
          Odens_20keV_in = dat_flux_in.Odens_20keV
          Ovel_mso_20keV_in = dat_flux_in.Ovel_mso_20keV
          Ovel_mse_20keV_in = mso2mse(Ovel_mso_20keV_in[0],Ovel_mso_20keV_in[1],Ovel_mso_20keV_in[2],rot)
          Oflux_mso_20keV_in = Odens_20keV_in * Ovel_mso_20keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_20keV_in = mso2mse(Oflux_mso_20keV_in[0],Oflux_mso_20keV_in[1],Oflux_mso_20keV_in[2],rot)


          Odens_10keV_in = dat_flux_in.Odens_10keV
          Ovel_mso_10keV_in = dat_flux_in.Ovel_mso_10keV
          Ovel_mse_10keV_in = mso2mse(Ovel_mso_10keV_in[0],Ovel_mso_10keV_in[1],Ovel_mso_10keV_in[2],rot)
          Oflux_mso_10keV_in = Odens_10keV_in * Ovel_mso_10keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_10keV_in = mso2mse(Oflux_mso_10keV_in[0],Oflux_mso_10keV_in[1],Oflux_mso_10keV_in[2],rot)


          Odens_5keV_in = dat_flux_in.Odens_5keV
          Ovel_mso_5keV_in = dat_flux_in.Ovel_mso_5keV
          Ovel_mse_5keV_in = mso2mse(Ovel_mso_5keV_in[0],Ovel_mso_5keV_in[1],Ovel_mso_5keV_in[2],rot)
          Oflux_mso_5keV_in = Odens_5keV_in * Ovel_mso_5keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_5keV_in = mso2mse(Oflux_mso_5keV_in[0],Oflux_mso_5keV_in[1],Oflux_mso_5keV_in[2],rot)


          Odens_1keV_in = dat_flux_in.Odens_1keV
          Ovel_mso_1keV_in = dat_flux_in.Ovel_mso_1keV
          Ovel_mse_1keV_in = mso2mse(Ovel_mso_1keV_in[0],Ovel_mso_1keV_in[1],Ovel_mso_1keV_in[2],rot)
          Oflux_mso_1keV_in = Odens_1keV_in * Ovel_mso_1keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_1keV_in = mso2mse(Oflux_mso_1keV_in[0],Oflux_mso_1keV_in[1],Oflux_mso_1keV_in[2],rot)


          Odens_100eV_in = dat_flux_in.Odens_100eV
          Ovel_mso_100eV_in = dat_flux_in.Ovel_mso_100eV
          Ovel_mse_100eV_in = mso2mse(Ovel_mso_100eV_in[0],Ovel_mso_100eV_in[1],Ovel_mso_100eV_in[2],rot)
          Oflux_mso_100eV_in = Odens_100eV_in * Ovel_mso_100eV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_100eV_in = mso2mse(Oflux_mso_100eV_in[0],Oflux_mso_100eV_in[1],Oflux_mso_100eV_in[2],rot)


          Odens_all_in = dat_flux_in.Odens_all
          Ovel_mso_all_in = dat_flux_in.Ovel_mso_all
          Ovel_mse_all_in = mso2mse(Ovel_mso_all_in[0],Ovel_mso_all_in[1],Ovel_mso_all_in[2],rot)
          Oflux_mso_all_in = Odens_all_in * Ovel_mso_all_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_all_in = mso2mse(Oflux_mso_all_in[0],Oflux_mso_all_in[1],Oflux_mso_all_in[2],rot)


          Odens_gt10keV_in = dat_flux_in.Odens_gt10keV
          Ovel_mso_gt10keV_in = dat_flux_in.Ovel_mso_gt10keV
          Ovel_mse_gt10keV_in = mso2mse(Ovel_mso_gt10keV_in[0],Ovel_mso_gt10keV_in[1],Ovel_mso_gt10keV_in[2],rot)
          Oflux_mso_gt10keV_in = Odens_gt10keV_in * Ovel_mso_gt10keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt10keV_in = mso2mse(Oflux_mso_gt10keV_in[0],Oflux_mso_gt10keV_in[1],Oflux_mso_gt10keV_in[2],rot)


          Odens_gt5keV_in = dat_flux_in.Odens_gt5keV
          Ovel_mso_gt5keV_in = dat_flux_in.Ovel_mso_gt5keV
          Ovel_mse_gt5keV_in = mso2mse(Ovel_mso_gt5keV_in[0],Ovel_mso_gt5keV_in[1],Ovel_mso_gt5keV_in[2],rot)
          Oflux_mso_gt5keV_in = Odens_gt5keV_in * Ovel_mso_gt5keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt5keV_in = mso2mse(Oflux_mso_gt5keV_in[0],Oflux_mso_gt5keV_in[1],Oflux_mso_gt5keV_in[2],rot)


          Odens_gt1keV_in = dat_flux_in.Odens_gt1keV
          Ovel_mso_gt1keV_in = dat_flux_in.Ovel_mso_gt1keV
          Ovel_mse_gt1keV_in = mso2mse(Ovel_mso_gt1keV_in[0],Ovel_mso_gt1keV_in[1],Ovel_mso_gt1keV_in[2],rot)
          Oflux_mso_gt1keV_in = Odens_gt1keV_in * Ovel_mso_gt1keV_in * 1e5 ;[#/cm2/sec]
          Oflux_mse_gt1keV_in = mso2mse(Oflux_mso_gt1keV_in[0],Oflux_mso_gt1keV_in[1],Oflux_mso_gt1keV_in[2],rot)
                 
          
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
         
         if Energy eq 'gt5keV' then begin
           Oflux_mse = Oflux_mse_gt5keV
           Ovel_mse = Ovel_mse_gt5keV
         endif
                
         if Energy eq 'gt1keV' then begin
           Oflux_mse = Oflux_mse_gt1keV
           Ovel_mse = Ovel_mse_gt1keV
         endif
         
         

         if Energy eq '20keV_in' then begin
           Oflux_mse = -Oflux_mse_20keV_in
           Ovel_mse = Ovel_mse_20keV_in
         endif

         if Energy eq '10keV_in' then begin
           Oflux_mse = -Oflux_mse_10keV_in
           Ovel_mse = Ovel_mse_10keV_in
         endif

         if Energy eq '5keV_in' then begin
           Oflux_mse = -Oflux_mse_5keV_in
           Ovel_mse = Ovel_mse_5keV_in
         endif

         if Energy eq '1keV_in' then begin
           Oflux_mse = -Oflux_mse_1keV_in
           Ovel_mse = Ovel_mse_1keV_in
         endif

         if Energy eq '100eV_in' then begin
           Oflux_mse = -Oflux_mse_100eV_in
           Ovel_mse = Ovel_mse_100eV_in
         endif
         
         if Energy eq 'all_in' then begin
           Oflux_mse = -Oflux_mse_all_in
           Ovel_mse = Ovel_mse_all_in
         endif

         if Energy eq 'gt10keV_in' then begin
           Oflux_mse = -Oflux_mse_gt10keV_in
           Ovel_mse = Ovel_mse_gt10keV_in
         endif

         if Energy eq 'gt5keV_in' then begin
           Oflux_mse = -Oflux_mse_gt5keV_in
           Ovel_mse = Ovel_mse_gt5keV_in
         endif

         if Energy eq 'gt1keV_in' then begin
           Oflux_mse = -Oflux_mse_gt1keV_in
           Ovel_mse = Ovel_mse_gt1keV_in
         endif

         if surf_type eq 'shock_surface' then begin 
           f_shock = inpro(Oflux_mse,norm_sh)
           f_tp[ith,iph] = f_tp[ith,iph] + f_shock
           n_tp[ith,iph] = n_tp[ith,iph] + 1.
           if f_shock lt 0 then stop
         endif
         
         if surf_type eq 'plane' then begin
           ;;xy plane   
           f_xy_x[ix,iy] = f_xy_x[ix,iy] + Oflux_mse[0]
           v_xy_x[ix,iy] = v_xy_x[ix,iy] + Ovel_mse[0]
           n_xy[ix,iy] = n_xy[ix,iy] + 1.
           ;;xz plane   
           f_xz_x[ix,iz] = f_xz_x[ix,iz] + Oflux_mse[0]
           v_xz_x[ix,iz] = v_xz_x[ix,iz] + Ovel_mse[0]
           n_xz[ix,iz] = n_xz[ix,iz] + 1.
           ;;yz plane
           f_yz_x[iy,iz] = f_yz_x[iy,iz] + Oflux_mse[0]
           v_yz_x[iy,iz] = v_yz_x[iy,iz] + Ovel_mse[0]
           n_yz[iy,iz] = n_yz[iy,iz] + 1.
         endif
         ;endif
         ;endif
         skip:
         
      endfor   
      no_data:
    endfor
    
    ;;calculate total number of precipitating/reflected ions against the shock surface
    if surf_type eq 'shock_surface' then begin
      idx = where(n_tp ne 0)
      f_tp_avg[idx] = f_tp[idx]/n_tp[idx] > 1e-10
      
      ;;plot average flux map
      loadct,39
      !p.multi=[0,1,2]
      plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
      for i=0,71 do begin                   
         for j=0,17 do begin
             flx=alog10(f_tp_avg[j,i])                              
             phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
             th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
             clr=bytscl(flx, min=alog10(zrange[0]), max=alog10(zrange[1]), top=top, /nan)
             if clr eq 255 then clr = 254
             if n_tp[j,i] eq 0 then clr = 255   
             ; Create the vectors of X and Y values:
             X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
             Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
             POLYFILL, X, Y, COLOR = clr;, /DEVICE                    
         endfor
      endfor
      plots,circle_v2(0,0,45),linestyle=2
      plots,circle_v2(0,0,90),linestyle=2
      oplot,[-90,90],[0,0],linestyle=2
      colorbar_v2,range=[alog10(zrange[0]),alog10(zrange[1])],/ver ,/right,charsize=2,format='(f3.1)',$
                  position=[!x.window[1]+0.03,!y.window[0],!x.window[1]+0.05,!y.window[1]]

      ;;plot number of measurements
      loadct,39
      plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
      for i=0,71 do begin
        for j=0,17 do begin
          flx=alog10(n_tp[j,i])
          phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
          th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
          clr=bytscl(flx, min=alog10(zrange_n[0]), max=alog10(zrange_n[1]), top=top, /nan)
          if clr eq 255 then clr = 254
          if n_tp[j,i] eq 0 then clr = 255           
          ; Create the vectors of X and Y values:
          X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
          Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
          POLYFILL, X, Y, COLOR = clr;, /DEVICE
        endfor
      endfor
      plots,circle_v2(0,0,45),linestyle=2
      plots,circle_v2(0,0,90),linestyle=2
      oplot,[-90,90],[0,0],linestyle=2
      colorbar_v2,range=[alog10(zrange_n[0]),alog10(zrange_n[1])],/ver ,/right,charsize=2,format='(f3.1)',$
      position=[!x.window[1]+0.03,!y.window[0],!x.window[1]+0.05,!y.window[1]]


;      ;loadct2,60,file='/Applications/exelis/idl84/resource/colors/colors2.tbl'  
;      plotxyz,phi_dash_bin*!radeg,theta_dash_bin*!radeg,alog10(transpose(f_tp_avg)),xtit='phi',ytit='theta',multi='1,2',charsize=2,zrange=[0,4]
;      oplot,[0,0],!y.crange,line=2
;      ;loadct2,61,file='/Applications/exelis/idl84/resource/colors/colors2.tbl'
;      plotxyz,phi_dash_bin*!radeg,theta_dash_bin*!radeg,alog10(transpose(n_tp)),xtit='phi',ytit='theta',/add,charsize=2,zrange=[0,3]
;      oplot,[0,0],!y.crange,line=2
    
     if keyword_set(make_notes) then journal,SAVE_LOC+'/maven/journal/'+journal_file+'.txt' ;;open journal
     ;;Calculate number in each region 
     ;;region 1 (Phi<0, SZA<45)
     j0 = 0 & j1 = 35
     i0 = 0 & i1 = 8   
      for j=j0, j1 do begin
        for i=i0,i1 do begin
         if i eq 0 then rflct_number[i,j] = f_tp_avg[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
         if i gt 0 then rflct_number[i,j] =  f_tp_avg[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
        endfor
      endfor
      print,'Region 1:',total(rflct_number[i0:i1,j0:j1])
      
      ;;region 2 (Phi<0, SZA>45)
      j0 = 0 & j1 = 35
      i0 = 9 & i1 = 17
      for j=j0, j1 do begin
        for i=i0,i1 do begin
          if i eq 0 then rflct_number[i,j] = f_tp_avg[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          if i gt 0 then rflct_number[i,j] =  f_tp_avg[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
        endfor
      endfor
      print,'Region 2:',total(rflct_number[i0:i1,j0:j1])
      
      ;;region 3 (Phi>0, SZA<45)
      j0 = 36 & j1 = 71
      i0 = 0 & i1 = 8
      for j=j0, j1 do begin
        for i=i0,i1 do begin
          if i eq 0 then rflct_number[i,j] = f_tp_avg[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          if i gt 0 then rflct_number[i,j] =  f_tp_avg[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
        endfor
      endfor
      print,'Region 3:',total(rflct_number[i0:i1,j0:j1])
      
      ;;region 4 (Phi>0, SZA>45)
      j0 = 36 & j1 = 71
      i0 = 9 & i1 = 17
      for j=j0, j1 do begin
        for i=i0,i1 do begin
          if i eq 0 then rflct_number[i,j] = f_tp_avg[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          if i gt 0 then rflct_number[i,j] =  f_tp_avg[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
        endfor
      endfor
      print,'Region 4:',total(rflct_number[i0:i1,j0:j1])
      
    endif
    if keyword_set(make_notes) then journal ;;close journal
    
    ;;calculate total number of precipitating/reflected ions against the yz plane
    if surf_type eq 'plane' then begin
     
      ;;xy plane
      idx_xy = where(n_xy ne 0)
      f_xy_x2[idx_xy] = f_xy_x[idx_xy]/n_xy[idx_xy] > 1e-20 
      v_xy_x2[idx_xy] = v_xy_x[idx_xy]/n_xy[idx_xy]
      ;;xz plane
      idx_xz = where(n_xz ne 0)
      f_xz_x2[idx_xz] = f_xz_x[idx_xz]/n_xz[idx_xz] > 1e-20 
      v_xz_x2[idx_xz] = v_xz_x[idx_xz]/n_xz[idx_xz]
      ;;yz plane
      idx_yz = where(n_yz ne 0)
      f_yz_x2[idx_yz] = f_yz_x[idx_yz]/n_yz[idx_yz] > 1e-20 
      v_yz_x2[idx_yz] = v_yz_x[idx_yz]/n_yz[idx_yz]
      
      if energy eq '100eV_in' $
        or energy eq '1keV_in' $
        or energy eq '5keV_in' $
        or energy eq '10keV_in' $
        or energy eq '20keV_in' $
        or energy eq 'all_in' $
        or energy eq 'gt10keV_in' $
        or energy eq 'gt5keV_in' $
        or energy eq 'gt1keV_in' then begin
          if not keyword_set(frange) then frange = [2,5]
      endif else begin
          if not keyword_set(frange) then frange = [2,4]
      endelse

      plot_fmap,each_axis,f_xy_x2,f_xz_x2,f_yz_x2,n_xy,n_xz,n_yz,energy=energy,frange=frange,nrange=[0,2]
      
      F_int_all = total(f_yz_x2*(res*Rm)^2)
      print,'Rate_YZ_all: ',F_int_all+' [#/s]'

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

      F_int = total(F_yz_map*(res*Rm)^2)
      print,'Rate_YZ_limited_area: ',F_int+' [#/s]'  
     
    endif
  
;    !p.multi=[0,1,2]
;    cone_angle_arr = cone_angle_arr[1:*]
;    pdf = histogram(cone_angle_arr,locations=xbin,binsize=10)
;    plot,xbin,pdf,psym=10,xrange=[0,180],/xstyle
    

;     !p.multi=[0,1,2]
;     clock_angle_arr = clock_angle_arr[1:*]
;     pdf = histogram(clock_angle_arr,locations=xbin,binsize=10)
;     plot,xbin,pdf,psym=10,xrange=[-180,180],/xstyle
 
end