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




pro plot_reflect_ion_map10_v5,median=median,frange=frange,$
                              sw_dpnd=sw_dpnd,Rg=Rg,Vperp=Vperp,Vt=Vt,dens=dens,Bt=Bt,Pdy=Pdy,euv=euv,Bcrst=Bcrst,Mach=Mach,Bsheath=Bsheath,cone=cone,$
                              large=large,small=small,day=day,night=night,perp=perp,para=para,$
                              make_notes=make_notes,journal_file=journal_file,ps=ps,fileps=fileps,$
                              rmv_ring_percentage=rmv_ring_percentage,$
                              plot_position=plot_position
   
  

    orbit_arr = [317+findgen(2412)];[317+findgen(450)];[317+findgen(500),1470+findgen(500), 2423+findgen(600)];346;,347,348,349,350] ;+ findgen(472)   
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
    
    f_out_gt5keV = fltarr(nth,nph)    & f_out_gt5keV_err = fltarr(nth,nph) 
    f_out_gt10keV = dblarr(nth,nph)   & f_out_gt10keV_err = dblarr(nth,nph) 
    f_out_20keV = fltarr(nth,nph)     & f_out_gt20keV_err = fltarr(nth,nph) 
    f_in_gt5keV = fltarr(nth,nph)     & f_in_gt5keV_err = fltarr(nth,nph) 
    f_in_gt10keV = dblarr(nth,nph)    & f_in_gt10keV_err = dblarr(nth,nph) 
    f_in_20keV = fltarr(nth,nph)      & f_in_gt20keV_err = fltarr(nth,nph) 
    n_f = fltarr(nth,nph)
    Out_number_gt5keV_1 = fltarr(18,72)
    Out_number_gt10keV_1 = fltarr(18,72)
    Out_number_20keV_1 = fltarr(18,72)
    Out_number_gt5keV_2 = fltarr(18,72)
    Out_number_gt10keV_2 = fltarr(18,72)
    Out_number_20keV_2 = fltarr(18,72)
    Out_number_gt5keV_3 = fltarr(18,72)
    Out_number_gt10keV_3 = fltarr(18,72)
    Out_number_20keV_3 = fltarr(18,72)
    Out_number_gt5keV_4 = fltarr(18,72)
    Out_number_gt10keV_4 = fltarr(18,72)
    Out_number_20keV_4 = fltarr(18,72)
    Out_number_gt5keV_all = fltarr(18,72)       
    Out_number_gt10keV_all = dblarr(18,72)  & Out_number_gt10keV_all_var = dblarr(18,72)
    Out_number_20keV_all = fltarr(18,72)
    
    In_number_gt5keV_1 = fltarr(18,72)
    In_number_gt10keV_1 = fltarr(18,72)
    In_number_20keV_1 = fltarr(18,72)
    In_number_gt5keV_2 = fltarr(18,72)
    In_number_gt10keV_2 = fltarr(18,72)
    In_number_20keV_2 = fltarr(18,72)
    In_number_gt5keV_3 = fltarr(18,72)
    In_number_gt10keV_3 = fltarr(18,72)
    In_number_20keV_3 = fltarr(18,72)
    In_number_gt5keV_4 = fltarr(18,72)
    In_number_gt10keV_4 = fltarr(18,72)
    In_number_20keV_4 = fltarr(18,72)
    In_number_gt5keV_all = fltarr(18,72)
    In_number_gt10keV_all = dblarr(18,72)  & In_number_gt10keV_all_var = dblarr(18,72)
    In_number_20keV_all = fltarr(18,72)
  
   
   bflg_p = 0.
   bflg_m = 0.
   cone_angle_arr = 0.
   clock_angle_arr = 0.
   rg_arr = 0.
   Vmso_perp_arr = 0.
   Bt_arr = 0.
   Pdyn_arr = 0.
   
   mtime_arr = 0d
   pos_mse_arr = [0.,0.,0.]
   Nsw_arr = 0.
   Vmso_arr = [0.,0.,0.]
   Bmso_arr = [0.,0.,0.]
   euv_arr = dblarr(1,3)
   f_sh_gt5keV_out_arr = 0.
   f_sh_gt5keV_in_arr = 0.
   f_sh_gt10keV_out_arr = 0.
   f_sh_gt10keV_in_arr = 0.
   f_sh_20keV_out_arr = 0.
   f_sh_20keV_in_arr = 0.
   bin_theta_phi_arr = fltarr(2,1)
    
    
 
    ;; start sorting flux into bins
    for in=0,n_elements(orbit_arr)-1 do begin  
      
      print,orbit_arr[in]
      
      filename = SAVE_LOC + '/maven/sav/flux/deflect_ion_rmv_ring/d0/flux_vdf_'+string(orbit_arr[in],format='(i05)')+'.sav'   
      fs = file_search(filename)
      if fs[0] eq '' then goto, no_data      
      restore,fs
      if size(dat_sav_arr,/type) ne 8 then begin
        stop
        goto,no_data
      endif
      
;      file_euv = SAVE_LOC + '/maven/sav/euv/avg_with_sta_d0_flux/euv_avg_d0_'+string(orbit_arr[in],format='(i05)')+'.sav' 
;      ft_euv = file_test(file_euv)
;      if ft_euv ne 0 then begin
;       restore,file_euv
;       euv_time = euv_dat.time
;       euv_avg = euv_dat.euv_avg
;       euv_arr = [ euv_arr, euv_avg ]
;      endif
      
      
      stime = dat_sav_arr.stime
      etime = dat_sav_arr.etime
      mtime = (stime + etime)/2.d
      mtime_arr = [mtime_arr,mtime]
      pos_mse_arr = [[pos_mse_arr],[dat_sav_arr.pos_mse]]
      Nsw = dat_sav_arr.Dens
      Vmso = dat_sav_arr.Vmso
      Bmso = dat_sav_arr.Bmso
      Nsw_arr = [Nsw_arr, Nsw]
      Vmso_arr = [[Vmso_arr], [Vmso]]
      Bmso_arr = [[Bmso_arr], [Bmso]] 
      bin_theta_phi_arr = [[bin_theta_phi_arr],[dat_sav_arr.bin_theta_phi]]
      
      ;; NO REMOVAL      
      if rmv_ring_percentage eq 0 then begin      
        Odens_gt5keV_out = dat_sav_arr.Odens_gt5keV_out_0
        Ovel_sh_gt5keV_out = dat_sav_arr.Ovel_sh_0_gt5keV_out_0
        f_sh_gt5keV_out = Odens_gt5keV_out * Ovel_sh_gt5keV_out
        f_sh_gt5keV_out_arr = [f_sh_gt5keV_out_arr, f_sh_gt5keV_out]
        Odens_gt5keV_in = dat_sav_arr.Odens_gt5keV_in_0
        Ovel_sh_gt5keV_in = dat_sav_arr.Ovel_sh_0_gt5keV_in_0
        f_sh_gt5keV_in = Odens_gt5keV_in * Ovel_sh_gt5keV_in
        f_sh_gt5keV_in_arr = [f_sh_gt5keV_in_arr, f_sh_gt5keV_in]

        Odens_gt10keV_out = dat_sav_arr.Odens_gt10keV_out_0
        Ovel_sh_gt10keV_out = dat_sav_arr.Ovel_sh_0_gt10keV_out_0
        f_sh_gt10keV_out = Odens_gt10keV_out * Ovel_sh_gt10keV_out
        f_sh_gt10keV_out_arr = [f_sh_gt10keV_out_arr, f_sh_gt10keV_out]
        Odens_gt10keV_in = dat_sav_arr.Odens_gt10keV_in_0
        Ovel_sh_gt10keV_in = dat_sav_arr.Ovel_sh_0_gt10keV_in_0
        f_sh_gt10keV_in = Odens_gt10keV_in * Ovel_sh_gt10keV_in
        f_sh_gt10keV_in_arr = [f_sh_gt10keV_in_arr, f_sh_gt10keV_in]
      
        Odens_20keV_out = dat_sav_arr.Odens_20keV_out_0
        Ovel_sh_20keV_out = dat_sav_arr.Ovel_sh_0_20keV_out_0
        f_sh_20keV_out = Odens_20keV_out * Ovel_sh_20keV_out
        f_sh_20keV_out_arr = [f_sh_20keV_out_arr, f_sh_20keV_out]
        Odens_20keV_in = dat_sav_arr.Odens_20keV_in_0
        Ovel_sh_20keV_in = dat_sav_arr.Ovel_sh_0_20keV_in_0
        f_sh_20keV_in = Odens_20keV_in * Ovel_sh_20keV_in
        f_sh_20keV_in_arr = [f_sh_20keV_in_arr, f_sh_20keV_in]
      endif

      ;; REMOVE 50% ring    
      if rmv_ring_percentage eq 50 then begin      
        Odens_gt5keV_out = dat_sav_arr.Odens_gt5keV_out_50
        Ovel_sh_gt5keV_out = dat_sav_arr.Ovel_sh_50_gt5keV_out_50
        f_sh_gt5keV_out = Odens_gt5keV_out * Ovel_sh_gt5keV_out
        f_sh_gt5keV_out_arr = [f_sh_gt5keV_out_arr, f_sh_gt5keV_out]
        Odens_gt5keV_in = dat_sav_arr.Odens_gt5keV_in_50
        Ovel_sh_gt5keV_in = dat_sav_arr.Ovel_sh_50_gt5keV_in_50
        f_sh_gt5keV_in = Odens_gt5keV_in * Ovel_sh_gt5keV_in
        f_sh_gt5keV_in_arr = [f_sh_gt5keV_in_arr, f_sh_gt5keV_in]

        Odens_gt10keV_out = dat_sav_arr.Odens_gt10keV_out_50
        Ovel_sh_gt10keV_out = dat_sav_arr.Ovel_sh_50_gt10keV_out_50
        f_sh_gt10keV_out = Odens_gt10keV_out * Ovel_sh_gt10keV_out
        f_sh_gt10keV_out_arr = [f_sh_gt10keV_out_arr, f_sh_gt10keV_out]
        Odens_gt10keV_in = dat_sav_arr.Odens_gt10keV_in_50
        Ovel_sh_gt10keV_in = dat_sav_arr.Ovel_sh_50_gt10keV_in_50
        f_sh_gt10keV_in = Odens_gt10keV_in * Ovel_sh_gt10keV_in
        f_sh_gt10keV_in_arr = [f_sh_gt10keV_in_arr, f_sh_gt10keV_in]
      
        Odens_20keV_out = dat_sav_arr.Odens_20keV_out_50
        Ovel_sh_20keV_out = dat_sav_arr.Ovel_sh_50_20keV_out_50
        f_sh_20keV_out = Odens_20keV_out * Ovel_sh_20keV_out
        f_sh_20keV_out_arr = [f_sh_20keV_out_arr, f_sh_20keV_out]
        Odens_20keV_in = dat_sav_arr.Odens_20keV_in_50
        Ovel_sh_20keV_in = dat_sav_arr.Ovel_sh_50_20keV_in_50
        f_sh_20keV_in = Odens_20keV_in * Ovel_sh_20keV_in
        f_sh_20keV_in_arr = [f_sh_20keV_in_arr, f_sh_20keV_in]
      endif
   
      
      ;; REMOVE 80% ring   
      if rmv_ring_percentage eq 80 then begin
        Odens_gt5keV_out = dat_sav_arr.Odens_gt5keV_out_80
        Ovel_sh_gt5keV_out = dat_sav_arr.Ovel_sh_80_gt5keV_out_80
        f_sh_gt5keV_out = Odens_gt5keV_out * Ovel_sh_gt5keV_out
        f_sh_gt5keV_out_arr = [f_sh_gt5keV_out_arr, f_sh_gt5keV_out]
        Odens_gt5keV_in = dat_sav_arr.Odens_gt5keV_in_80
        Ovel_sh_gt5keV_in = dat_sav_arr.Ovel_sh_80_gt5keV_in_80
        f_sh_gt5keV_in = Odens_gt5keV_in * Ovel_sh_gt5keV_in
        f_sh_gt5keV_in_arr = [f_sh_gt5keV_in_arr, f_sh_gt5keV_in]

        Odens_gt10keV_out = dat_sav_arr.Odens_gt10keV_out_80
        Ovel_sh_gt10keV_out = dat_sav_arr.Ovel_sh_80_gt10keV_out_80
        f_sh_gt10keV_out = Odens_gt10keV_out * Ovel_sh_gt10keV_out
        f_sh_gt10keV_out_arr = [f_sh_gt10keV_out_arr, f_sh_gt10keV_out]
        Odens_gt10keV_in = dat_sav_arr.Odens_gt10keV_in_80
        Ovel_sh_gt10keV_in = dat_sav_arr.Ovel_sh_80_gt10keV_in_80
        f_sh_gt10keV_in = Odens_gt10keV_in * Ovel_sh_gt10keV_in
        f_sh_gt10keV_in_arr = [f_sh_gt10keV_in_arr, f_sh_gt10keV_in]

        Odens_20keV_out = dat_sav_arr.Odens_20keV_out_80
        Ovel_sh_20keV_out = dat_sav_arr.Ovel_sh_80_20keV_out_80
        f_sh_20keV_out = Odens_20keV_out * Ovel_sh_20keV_out
        f_sh_20keV_out_arr = [f_sh_20keV_out_arr, f_sh_20keV_out]
        Odens_20keV_in = dat_sav_arr.Odens_20keV_in_80
        Ovel_sh_20keV_in = dat_sav_arr.Ovel_sh_80_20keV_in_80
        f_sh_20keV_in = Odens_20keV_in * Ovel_sh_20keV_in
        f_sh_20keV_in_arr = [f_sh_20keV_in_arr, f_sh_20keV_in]
      endif
      
      
      ;; REMOVE 100% ring
      if rmv_ring_percentage eq 100 then begin
        Odens_gt5keV_out = dat_sav_arr.Odens_gt5keV_out_100
        Ovel_sh_gt5keV_out = dat_sav_arr.Ovel_sh_100_gt5keV_out_100
        f_sh_gt5keV_out = Odens_gt5keV_out * Ovel_sh_gt5keV_out
        f_sh_gt5keV_out_arr = [f_sh_gt5keV_out_arr, f_sh_gt5keV_out]
        Odens_gt5keV_in = dat_sav_arr.Odens_gt5keV_in_100
        Ovel_sh_gt5keV_in = dat_sav_arr.Ovel_sh_100_gt5keV_in_100
        f_sh_gt5keV_in = Odens_gt5keV_in * Ovel_sh_gt5keV_in
        f_sh_gt5keV_in_arr = [f_sh_gt5keV_in_arr, f_sh_gt5keV_in]

        Odens_gt10keV_out = dat_sav_arr.Odens_gt10keV_out_100
        Ovel_sh_gt10keV_out = dat_sav_arr.Ovel_sh_100_gt10keV_out_100
        f_sh_gt10keV_out = Odens_gt10keV_out * Ovel_sh_gt10keV_out
        f_sh_gt10keV_out_arr = [f_sh_gt10keV_out_arr, f_sh_gt10keV_out]
        Odens_gt10keV_in = dat_sav_arr.Odens_gt10keV_in_100
        Ovel_sh_gt10keV_in = dat_sav_arr.Ovel_sh_100_gt10keV_in_100
        f_sh_gt10keV_in = Odens_gt10keV_in * Ovel_sh_gt10keV_in
        f_sh_gt10keV_in_arr = [f_sh_gt10keV_in_arr, f_sh_gt10keV_in]

        Odens_20keV_out = dat_sav_arr.Odens_20keV_out_100
        Ovel_sh_20keV_out = dat_sav_arr.Ovel_sh_100_20keV_out_100
        f_sh_20keV_out = Odens_20keV_out * Ovel_sh_20keV_out
        f_sh_20keV_out_arr = [f_sh_20keV_out_arr, f_sh_20keV_out]
        Odens_20keV_in = dat_sav_arr.Odens_20keV_in_100
        Ovel_sh_20keV_in = dat_sav_arr.Ovel_sh_100_20keV_in_100
        f_sh_20keV_in = Odens_20keV_in * Ovel_sh_20keV_in
        f_sh_20keV_in_arr = [f_sh_20keV_in_arr, f_sh_20keV_in]
      endif
      
      
      no_data:
    endfor
    
    mtime_arr = mtime_arr[1:*]
    pos_mse_arr = pos_mse_arr[*,1:*]
    Nsw_arr = Nsw_arr[1:*]
    Vmso_arr = Vmso_arr[*,1:*]
    Vmso_t_arr = total(Vmso_arr^2,1)^.5
    Pdy_arr = Nsw_arr * Vmso_t_arr^2 * 1.6726 * 1e-6
    Bmso_arr = Bmso_arr[*,1:*]
    Bmso_t_arr = total(Bmso_arr^2,1)^.5
    Bmso_norm_arr = Bmso_arr[0,*]
    Bmso_trans_arr = sqrt(Bmso_arr[1,*]^2 + Bmso_arr[2,*]^2)
    Vperp_arr = fltarr(n_elements(mtime_arr))
    Vpara_arr = fltarr(n_elements(mtime_arr))
    for i=0,n_elements(mtime_arr)-1 do begin
         Vperp_arr[i] = norm(crossp(Vmso_arr[*,i],Bmso_arr[*,i])/norm(Bmso_arr[*,i]))
         Vpara_arr[i] = (transpose(Bmso_arr[*,i]) # Vmso_arr[*,i])/norm(Bmso_arr[*,i])
    endfor
    pos_crst_geo = fltarr(3,n_elements(mtime_arr))
    th_crst = -53*!DTOR
    ph_crst = 179*!DTOR
    xcrst = cos(th_crst)*cos(ph_crst)
    ycrst = cos(th_crst)*sin(ph_crst)
    zcrst = sin(ph_crst)
    
    if keyword_set(Bcrst) then begin
      Pos_crst_geo[0,*] = xcrst
      Pos_crst_geo[1,*] = ycrst
      Pos_crst_geo[2,*] = zcrst    
      Pos_crst_mso = spice_vector_rotate(Pos_crst_geo,mtime_arr,'IAU_MARS','MAVEN_MSO',check_objects=check_objects,verbose=verbose,qrot=qrot)
    endif
    
   
      tplot_saved_mvn_sw_temp,/noplot
      tsmooth_in_time,'Te',128
      tsmooth_in_time,'Ti',128
      get_data,'Te_smoothed',data=Tedat
      get_data,'Ti_smoothed',data=Tidat
      Te_intp = interpol(Tedat.y,Tedat.x, mtime_arr)
      Ti_intp = interpol(Tidat.y,Tidat.x, mtime_arr)
      store_data,'Te_intp',data={x:mtime_arr, y:Te_intp}
      store_data,'Ti_intp',data={x:mtime_arr, y:Ti_intp}
      Te_arr = Te_intp
      Ti_arr = Ti_intp
    
    
    Moxy = 16.*1.67e-27 ;;[kg]
    q = 1.602e-19 ;;[C]
    R_m = 3389.9 ;;[km]
    Rg_arr = Moxy*Vperp_arr/q/(Bmso_t_arr*1e-9)/R_m
    
    mp = 1.67e-27
    k = 1.3806e-23
    myu0 = 4.*!pi * 1e-7 ;;[N/A^2]
    sza_arr = acos( pos_mse_arr[0,*]/sqrt(pos_mse_arr[0,*]^2+pos_mse_arr[1,*]^2+pos_mse_arr[2,*]^2) )
    Bmpr_arr = sqrt( 2.*myu0*Pdy_arr*1e-9 ) *1e9 ;;[nT]
    ;Bmpr_arr = sqrt( 2.*myu0*0.85*Pdy_arr*cos(sza_arr)*1e-9 ) *1e9 ;;[nT]
    
    gamma = 5./3.
    Va_arr = 2.19e4*sqrt( Bmso_t_arr^2/(Nsw_arr*1e6) )
    Vs_arr = 91.*sqrt(gamma*(Te_arr+Ti_arr))/1e3  ;sqrt(gamma*k*(Te_arr+Ti_arr)/mp)/1e3 ;
    Mach_arr = Vmso_t_arr/sqrt( Va_arr^2 + Vs_arr^2 )
;    Va_arr = Bmso_t_arr*1e-9/sqrt(myu0*Nsw_arr*1e6*1.67e-27)/1e3
;    Mach_arr = Vmso_t_arr/Va_arr
    Bsheath_norm_arr =  Bmso_norm_arr
    Bsheath_trans_arr = (gamma + 1.)/(gamma - 1. + 2./Mach_arr^2) * Bmso_trans_arr
    Bsheath_arr = sqrt(Bsheath_norm_arr^2 + Bsheath_trans_arr^2);(gamma + 1.)/(gamma - 1. + 2./Mach_arr^2) * Bmso_t_arr 
    cone_agl_arr = acos( Bmso_arr[0,*]/(Bmso_arr[0,*]^2 + Bmso_arr[1,*]^2 + Bmso_arr[2,*]^2)^.5) * !radeg
    
    
    bin_theta_phi_arr = bin_theta_phi_arr[*,1:*]    
    f_sh_gt5keV_out_arr = f_sh_gt5keV_out_arr[1:*]*1e5
    f_sh_gt5keV_in_arr = f_sh_gt5keV_in_arr[1:*]*1e5
    f_sh_gt10keV_out_arr = f_sh_gt10keV_out_arr[1:*]*1e5
    f_sh_gt10keV_in_arr = f_sh_gt10keV_in_arr[1:*]*1e5
    f_sh_20keV_out_arr = f_sh_20keV_out_arr[1:*]*1e5
    f_sh_20keV_in_arr = f_sh_20keV_in_arr[1:*]*1e5
       
    if keyword_set(euv) then begin
      tplot_saved_mvn_euv,/noplot
      tsmooth_in_time,'EUVM',128
      get_data,'EUVM_smoothed',data=euvdat
      euv_intp_ch1 = interpol(euvdat.y[*,0],euvdat.x, mtime_arr)  ;; A: 17-22 nm
      euv_intp_ch2 = interpol(euvdat.y[*,1],euvdat.x, mtime_arr)  ;; B: 0-7 nm
      euv_intp_ch3 = interpol(euvdat.y[*,2],euvdat.x, mtime_arr)  ;; C: 121-122 nm
      store_data,'EUVM_intp_ch1',data={x:mtime_arr, y:euv_intp_ch1}
      store_data,'EUVM_intp_ch2',data={x:mtime_arr, y:euv_intp_ch2}
      store_data,'EUVM_intp_ch3',data={x:mtime_arr, y:euv_intp_ch3}
      euv_arr = euv_intp_ch1
    endif
    
    
    ;;;;Remove data with M_MS < 2 ;;;;;
   
    idx_mach = where(Mach_arr ge 1.8)
    mtime_arr = mtime_arr[idx_mach]
    pos_mse_arr = pos_mse_arr[idx_mach]
    Nsw_arr = Nsw_arr[idx_mach]
    Vmso_arr = Vmso_arr[idx_mach]
    Vmso_t_arr = Vmso_t_arr[idx_mach]
    Pdy_arr = Pdy_arr[idx_mach]
    Bmso_arr = Bmso_arr[idx_mach]
    Bmso_t_arr = Bmso_t_arr[idx_mach]
    Bmso_norm_arr = Bmso_norm_arr[idx_mach]
    Bmso_trans_arr = Bmso_trans_arr[idx_mach]
    Vperp_arr = Vperp_arr[idx_mach]
    Vpara_arr = Vpara_arr[idx_mach]
    Rg_arr = Rg_arr[idx_mach]
    sza_arr = sza_arr[idx_mach]
    Bmpr_arr = Bmpr_arr[idx_mach]
    cone_agl_arr = cone_agl_arr[idx_mach]

    
      Te_arr = Te_arr[idx_mach]
      Ti_arr = Ti_arr[idx_mach]
      Va_arr = Va_arr[idx_mach]
      Vs_arr = Vs_arr[idx_mach]
      Mach_arr = Mach_arr[idx_mach]
      Bsheath_norm_arr =  Bsheath_norm_arr[idx_mach]
      Bsheath_trans_arr = Bsheath_trans_arr[idx_mach]
      Bsheath_arr = Bsheath_arr[idx_mach]
  
    
    if keyword_set(euv) then euv_arr = euv_arr[idx_mach]
    if keyword_set(Bcrst) then Pos_crst_mso = Pos_crst_mso[*,idx_mach]
    
    bin_theta_phi_arr = bin_theta_phi_arr[*,idx_mach]
    f_sh_gt5keV_out_arr = f_sh_gt5keV_out_arr[idx_mach]
    f_sh_gt5keV_in_arr = f_sh_gt5keV_in_arr[idx_mach]
    f_sh_gt10keV_out_arr = f_sh_gt10keV_out_arr[idx_mach]
    f_sh_gt10keV_in_arr = f_sh_gt10keV_in_arr[idx_mach]
    f_sh_20keV_out_arr = f_sh_20keV_out_arr[idx_mach]
    f_sh_20keV_in_arr = f_sh_20keV_in_arr[idx_mach]
    
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Observation postion (MSE);;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
  if keyword_set(plot_position) then begin
    !p.multi=[0,3,1]
    xmse_arr = transpose(pos_mse_arr[0,*])
    ymse_arr = transpose(pos_mse_arr[1,*])
    zmse_arr = transpose(pos_mse_arr[2,*]) 
    plot_mvn_orbit,/only_map,/xy,charsize=3
    oplot,xmse_arr,ymse_arr,psym=3
    plot_mvn_orbit,/only_map,/xz,charsize=3
    oplot,xmse_arr,zmse_arr,psym=3
    plot_mvn_orbit,/only_map,/yz,charsize=3
    oplot,ymse_arr,zmse_arr,psym=3
    !p.multi=[0,1,1]
  endif
      
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;solar wind dependences;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
    if keyword_set(sw_dpnd) then begin
      !p.multi=[0,2,2,0,0]
      charsize=1.5


      dNsw = 0.5
      Nsw_hist = histogram(Nsw_arr,binsize=dNsw)
      hist_bins_Nsw = ( findgen( n_elements(Nsw_arr) ) * dNsw ) + min( Nsw_arr )
      plot,hist_bins_Nsw,Nsw_hist,psym=10,xrange=[0,30],charsize=charsize,xtit='Nsw [cm-3]'
      oplot,[median(Nsw_arr),median(Nsw_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Nsw_arr),mean(Nsw_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      
      dV = 20
      V_hist = histogram(Vmso_t_arr,binsize=dV)
      hist_bins_V = ( findgen( n_elements(Vmso_t_arr) ) * dV ) + min( Vmso_t_arr )
      plot,hist_bins_V,V_hist,psym=10,xrange=[0,700],charsize=charsize,xtit='Vsw [km/s]'
      oplot,[median(Vmso_t_arr),median(Vmso_t_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Vmso_t_arr),mean(Vmso_t_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      
;      dVperp = 20
;      Vperp_hist = histogram(Vperp_arr,binsize=dVperp)
;      hist_bins_Vperp = ( findgen( n_elements(Vperp_arr) ) * dVperp ) + min( Vperp_arr )
;      plot,hist_bins_Vperp,Vperp_hist,psym=10,xrange=[0,max(Vperp_arr)],charsize=charsize,xtit='Vperp [km/s]'
;      oplot,[median(Vperp_arr),median(Vperp_arr)],[!y.crange[0],!y.crange[1]],line=2
;      oplot,[mean(Vperp_arr),mean(Vperp_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      
      dBt = 0.5
      Bt_hist = histogram(Bmso_t_arr,binsize=dBt)
      hist_bins_Bt = ( findgen( n_elements(Bmso_t_arr) ) * dBt ) + min( Bmso_t_arr )
      plot,hist_bins_Bt,Bt_hist,psym=10,xrange=[0,20],charsize=charsize,xtit='Bt [nT]',yrange=[0,1.4e4],/ysty
      oplot,[median(Bmso_t_arr),median(Bmso_t_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Bmso_t_arr),mean(Bmso_t_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      
      ;if keyword_set(cone) then begin
        dcone = 5
        cone_hist = histogram(cone_agl_arr,binsize=dcone)
        hist_bins_cone = ( findgen( n_elements(cone_agl_arr) ) * dcone ) + min( cone_agl_arr )
        plot,hist_bins_cone,cone_hist,psym=10,xrange=[0,180],/xsty,charsize=charsize,xtit='Cone angle [degree]'
        oplot,[median(cone_agl_arr),median(cone_agl_arr)],[!y.crange[0],!y.crange[1]],line=2
        oplot,[mean(cone_agl_arr),mean(cone_agl_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      ;endif
      
      
      dPdy = 0.1
      Pdy_hist = histogram(Pdy_arr,binsize=dPdy)
      hist_bins_Pdy = ( findgen( n_elements(Pdy_arr) ) * dPdy) + min( Pdy_arr )
      plot,hist_bins_Pdy,Pdy_hist,psym=10,xrange=[0,5],charsize=charsize,xtit='Pdy [nPa]'
      oplot,[median(Pdy_arr),median(Pdy_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Pdy_arr),mean(Pdy_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
                
      dRg = 0.5
      Rg_hist = histogram(Rg_arr,binsize=dRg)
      hist_bins_Rg = ( findgen( n_elements(Rg_arr) ) * dRg ) + min( Rg_arr )
      plot,hist_bins_Rg,Rg_hist,psym=10,xrange=[0,30],charsize=charsize,xtit='Rg [Rm]'
      oplot,[median(Rg_arr),median(Rg_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Rg_arr),mean(Rg_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
;      
;      if keyword_set(Tion) then begin
;        dTi = 10000.
;        Ti_hist = histogram(Ti_arr,binsize=dTi)
;        hist_bins_Ti = ( findgen( n_elements(Ti_arr) ) * dTi ) + min( Ti_arr )
;        plot,hist_bins_Ti,Ti_hist,psym=10,xrange=[0,1e6],charsize=charsize,xtit='Temperature [K]'
;        oplot,[median(Ti_arr),median(Ti_arr)],[!y.crange[0],!y.crange[1]],line=2
;        oplot,[mean(Ti_arr),mean(Ti_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
;      endif
;      
;      if keyword_set(Telec) then begin
;        dTe = 10000.
;        Te_hist = histogram(Te_arr,binsize=dTe)
;        hist_bins_Te = ( findgen( n_elements(Te_arr) ) * dTe ) + min( Te_arr )
;        plot,hist_bins_Te,Te_hist,psym=10,xrange=[0,1e6],charsize=charsize,xtit='Temperature [K]'
;        oplot,[median(Te_arr),median(Te_arr)],[!y.crange[0],!y.crange[1]],line=2
;        oplot,[mean(Te_arr),mean(Te_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
;      endif
      
      if keyword_set(Mach) then begin
        dMach = 0.5
        Mach_hist = histogram(Mach_arr,binsize=dMach)
        hist_bins_Mach = ( findgen( n_elements(Mach_arr) ) * dMach ) + min( Mach_arr )
        plot,hist_bins_Mach,Mach_hist,psym=10,xrange=[0,10],charsize=charsize,xtit='Mach number'
        oplot,[median(Mach_arr),median(Mach_arr)],[!y.crange[0],!y.crange[1]],line=2
        oplot,[mean(Mach_arr),mean(Mach_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      endif
      
      if keyword_set(Bsheath) then begin
        dBsheath = 1
        Bsheath_hist = histogram(Bsheath_arr,binsize=dBsheath)
        hist_bins_Bsheath = ( findgen( n_elements(Bsheath_arr) ) * dBsheath ) + min( Bsheath_arr )
        plot,hist_bins_Bsheath,Bsheath_hist,psym=10,xrange=[0,60],charsize=charsize,xtit='Sheath Field [nT]'
        oplot,[median(Bsheath_arr),median(Bsheath_arr)],[!y.crange[0],!y.crange[1]],line=2
        oplot,[mean(Bsheath_arr),mean(Bsheath_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      endif
      
      if keyword_set(euv) then begin
      deuv = 0.00001
      euv_hist = histogram(euv_arr,binsize=deuv)
      hist_bins_euv = ( findgen( n_elements(euv_arr) ) * deuv ) + min( euv_arr )
      plot,hist_bins_euv,euv_hist,psym=10,xrange=[0,0.0008],charsize=charsize,xtit='EUV (17-22nm) [W/m^2]'
      oplot,[median(euv_arr),median(euv_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(euv_arr),mean(euv_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      endif
      
      if keyword_set(Bcrst) then begin
      nday = n_elements( where(Pos_crst_mso[0,*] gt 0) )
      nnight = n_elements( where(Pos_crst_mso[0,*] le 0) )
      plot,[0,0],[0,nday],xrange=[-1,4],/xsty,xticks=1,xminor=1,charsize=charsize
      oplot,[1,1],[0,nday]
      oplot,[0,1],[nday,nday]
      oplot,[2,2],[0,nnight]
      oplot,[3,3],[0,nnight]
      oplot,[2,3],[nnight,nnight]
      endif
      
      
    
      
      
  

       if keyword_set(dens) then begin
         if keyword_set(Small) then idx_sort = where(Nsw_arr le median(Nsw_arr))
         if keyword_set(Large) then idx_sort = where(Nsw_arr gt median(Nsw_arr))
       endif
     
       if keyword_set(Vperp) then begin
         if keyword_set(Small) then idx_sort = where(Vperp_arr le median(Vperp_arr))
         if keyword_set(Large) then idx_sort = where(Vperp_arr gt median(Vperp_arr))
       endif
       
       if keyword_set(Vt) then begin
         if keyword_set(Small) then idx_sort = where(Vmso_t_arr le median(Vmso_t_arr))
         if keyword_set(Large) then idx_sort = where(Vmso_t_arr gt median(Vmso_t_arr))
       endif
          
       if keyword_set(Bt) then begin
         ;idx_sort = where( Bmso_t_arr gt 8  )
         if keyword_set(Small) then idx_sort = where(Bmso_t_arr le median(Bmso_t_arr))
         if keyword_set(Large) then idx_sort = where(Bmso_t_arr gt median(Bmso_t_arr))  
       endif
       
       if keyword_set(cone) then begin
         if keyword_set(perp) then idx_sort = where( cone_agl_arr gt 45 and cone_agl_arr lt 135 )
         if keyword_set(para) then idx_sort = where( cone_agl_arr le 45 or cone_agl_arr ge 135 )
       endif
       
       if keyword_set(Pdy) then begin
         if keyword_set(Small) then idx_sort = where(Pdy_arr le median(Pdy_arr))
         if keyword_set(Large) then idx_sort = where(Pdy_arr gt median(Pdy_arr))
       endif
            
       if keyword_set(Rg) then begin
         ;idx_sort = where( Rg_arr gt 8. )
         if keyword_set(Small) then idx_sort = where(Rg_arr le median(Rg_arr))
         if keyword_set(Large) then idx_sort = where(Rg_arr gt median(Rg_arr))
       endif
       
       
       if keyword_set(Mach) then begin
         if keyword_set(Small) then idx_sort = where(Mach_arr le median(Mach_arr))
         if keyword_set(Large) then idx_sort = where(Mach_arr gt median(Mach_arr))
       endif
       
       if keyword_set(Bsheath) then begin
         if keyword_set(Small) then idx_sort = where(Bsheath_arr le median(Bsheath_arr))
         if keyword_set(Large) then idx_sort = where(Bsheath_arr gt median(Bsheath_arr))
       endif
       
       if keyword_set(euv) then begin
         ;idx_sort = where( Rg_arr gt 8. )
         if keyword_set(Small) then idx_sort = where(euv_arr le median(euv_arr))
         if keyword_set(Large) then idx_sort = where(euv_arr gt median(euv_arr))
       endif

       if keyword_set(Bcrst) then begin
         ;idx_sort = where( Rg_arr gt 8. )
         if keyword_set(day) then idx_sort = where(Pos_crst_mso[0,*] gt 0)
         if keyword_set(night) then idx_sort = where(Pos_crst_mso[0,*] le 0)
       endif
       
       ;idx_sort = where(  Bmso_t_arr lt 2 )
       ;idx_sort = where( Pos_crst_mso[0,*] le 0 and Pdy_arr gt median(Pdy_arr) )
       ;idx_sort = where( Rg_arr ge 8 )
      stop
       bin_theta_phi_arr = bin_theta_phi_arr[*,idx_sort]
       f_sh_gt5keV_out_arr = f_sh_gt5keV_out_arr[idx_sort]
       f_sh_gt5keV_in_arr = f_sh_gt5keV_in_arr[idx_sort]
       f_sh_gt10keV_out_arr = f_sh_gt10keV_out_arr[idx_sort]
       f_sh_gt10keV_in_arr = f_sh_gt10keV_in_arr[idx_sort]
       f_sh_20keV_out_arr = f_sh_20keV_out_arr[idx_sort]
       f_sh_20keV_in_arr = f_sh_20keV_in_arr[idx_sort]
       
    endif
    
      
      ;;plot average flux map
      loadct,39
      !p.multi=[0,5,2,0,1]
      if keyword_set(ps) then begin
        !p.multi=[0,4,2,0,1]
        popen,'~/work/save_data/maven/ps/random/'+fileps,/land
      endif
      
      ;;Map for >5keV ions
      plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
      for i=0,71 do begin
        for j=0,17 do begin

          idx = where((bin_theta_phi_arr[0,*] eq j) and (bin_theta_phi_arr[1,*] eq i))
          if idx[0] ne -1 then begin
            if keyword_set(median) then f_out_gt5keV[j,i] = median(f_sh_gt5keV_out_arr[idx]) $
            else f_out_gt5keV[j,i] = mean(f_sh_gt5keV_out_arr[idx])
            n_f[j,i] = n_elements(idx)
          endif

          flx=alog10(f_out_gt5keV[j,i]) > 1e-10
          phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
          th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
          clr=bytscl(flx, min=alog10(zrange[0]), max=alog10(zrange[1]), top=top, /nan)
          if clr eq 255 then clr = 254
          if n_f[j,i] eq 0 then clr = 255
          ; Create the vectors of X and Y values:
          X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
          Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
          POLYFILL, X, Y, COLOR = clr,/data;, /DEVICE
        endfor
      endfor
      plots,circle_v2(0,0,45),linestyle=2
      plots,circle_v2(0,0,90),linestyle=2
      oplot,[-90,90],[0,0],linestyle=2
      colorbar_v2,range=[alog10(zrange[0]),alog10(zrange[1])],/ver ,/right,charsize=2,format='(f3.1)',$
        position=[!x.window[1]+0.01,!y.window[0],!x.window[1]+0.02,!y.window[1]]


      plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
      for i=0,71 do begin
        for j=0,17 do begin

          idx = where((bin_theta_phi_arr[0,*] eq j) and (bin_theta_phi_arr[1,*] eq i))
          if idx[0] ne -1 then begin
            if keyword_set(median) then f_in_gt5keV[j,i] = median(f_sh_gt5keV_in_arr[idx]) $
            else f_in_gt5keV[j,i] = mean(f_sh_gt5keV_in_arr[idx])
            n_f[j,i] = n_elements(idx)
          endif

          flx=alog10(f_in_gt5keV[j,i]) > 1e-10
          phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
          th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
          clr=bytscl(flx, min=alog10(zrange[0]), max=alog10(zrange[1]), top=top, /nan)
          if clr eq 255 then clr = 254
          if n_f[j,i] eq 0 then clr = 255
          ; Create the vectors of X and Y values:
          X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
          Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
          POLYFILL, X, Y, COLOR = clr,/data;, /DEVICE
        endfor
      endfor
      plots,circle_v2(0,0,45),linestyle=2
      plots,circle_v2(0,0,90),linestyle=2
      oplot,[-90,90],[0,0],linestyle=2
      colorbar_v2,range=[alog10(zrange[0]),alog10(zrange[1])],/ver ,/right,charsize=2,format='(f3.1)',$
        position=[!x.window[1]+0.01,!y.window[0],!x.window[1]+0.02,!y.window[1]]     
      
      
      
      
      ;;Map for >10keV ions
      plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
      for i=0,71 do begin                   
         for j=0,17 do begin
             
             idx = where((bin_theta_phi_arr[0,*] eq j) and (bin_theta_phi_arr[1,*] eq i))
             if idx[0] ne -1 then begin 
               if keyword_set(median) then f_out_gt10keV[j,i] = median(f_sh_gt10keV_out_arr[idx]) $
               else f_out_gt10keV[j,i] = mean(f_sh_gt10keV_out_arr[idx])
                  
;                  if n_elements(idx) gt 10 then begin
;                  dfout = 500
;                  fout_hist = histogram(f_sh_gt10keV_out_arr[idx],binsize=dfout)
;                  hist_bins_fout = ( findgen( n_elements(f_sh_gt10keV_out_arr[idx]) ) * dfout ) + min( f_sh_gt10keV_out_arr[idx] )
;                  plot,hist_bins_fout,fout_hist,psym=10,xrange=[0,5000],charsize=charsize,xtit='Flux [cm-2 s-1]'
;                  stop
;                  endif
                  
               
               if n_elements(f_sh_gt10keV_out_arr[idx]) gt 1 then f_out_gt10keV_err[j,i] = stddev(f_sh_gt10keV_out_arr[idx]) else f_out_gt10keV_err[j,i] = 0.
               
               if finite(f_out_gt10keV_err[j,i]) eq 0 then stop
               n_f[j,i] = n_elements(idx)
             endif
             
             flx=alog10(f_out_gt10keV[j,i]) > 1e-10                           
             phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
             th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
             clr=bytscl(flx, min=alog10(zrange[0]), max=alog10(zrange[1]), top=top, /nan)
             if clr eq 255 then clr = 254
             if n_f[j,i] eq 0 then clr = 255   
             ; Create the vectors of X and Y values:
             X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
             Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
             POLYFILL, X, Y, COLOR = clr,/data;, /DEVICE                    
         endfor
      endfor
      plots,circle_v2(0,0,45),linestyle=2
      plots,circle_v2(0,0,90),linestyle=2
      oplot,[-90,90],[0,0],linestyle=2
      colorbar_v2,range=[alog10(zrange[0]),alog10(zrange[1])],/ver ,/right,charsize=2,format='(f3.1)',$
                  position=[!x.window[1]+0.01,!y.window[0],!x.window[1]+0.02,!y.window[1]]

      
      plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
      for i=0,71 do begin                   
         for j=0,17 do begin
             
             idx = where((bin_theta_phi_arr[0,*] eq j) and (bin_theta_phi_arr[1,*] eq i))
             if idx[0] ne -1 then begin 
               if keyword_set(median) then f_in_gt10keV[j,i] = median(f_sh_gt10keV_in_arr[idx]) $
               else f_in_gt10keV[j,i] = mean(f_sh_gt10keV_in_arr[idx])
               if n_elements(f_sh_gt10keV_in_arr[idx]) gt 1 then f_in_gt10keV_err[j,i] = stddev(f_sh_gt10keV_in_arr[idx]) else f_in_gt10keV_err[j,i] = 0.
               n_f[j,i] = n_elements(idx)
             endif
             
             flx=alog10(f_in_gt10keV[j,i]) > 1e-10                           
             phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
             th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
             clr=bytscl(flx, min=alog10(zrange[0]), max=alog10(zrange[1]), top=top, /nan)
             if clr eq 255 then clr = 254
             if n_f[j,i] eq 0 then clr = 255   
             ; Create the vectors of X and Y values:
             X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
             Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
             POLYFILL, X, Y, COLOR = clr,/data;, /DEVICE                    
         endfor
      endfor
      plots,circle_v2(0,0,45),linestyle=2
      plots,circle_v2(0,0,90),linestyle=2
      oplot,[-90,90],[0,0],linestyle=2
      colorbar_v2,range=[alog10(zrange[0]),alog10(zrange[1])],/ver ,/right,charsize=2,format='(f3.1)',$
      position=[!x.window[1]+0.01,!y.window[0],!x.window[1]+0.02,!y.window[1]]



    ;;Map for >20keV ions
    plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
    for i=0,71 do begin
      for j=0,17 do begin

        idx = where((bin_theta_phi_arr[0,*] eq j) and (bin_theta_phi_arr[1,*] eq i))
        if idx[0] ne -1 then begin
          if keyword_set(median) then f_out_20keV[j,i] = median(f_sh_20keV_out_arr[idx]) $
          else f_out_20keV[j,i] = mean(f_sh_20keV_out_arr[idx])
          n_f[j,i] = n_elements(idx)
        endif

        flx=alog10(f_out_20keV[j,i]) > 1e-10
        phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
        th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
        clr=bytscl(flx, min=alog10(zrange[0]), max=alog10(zrange[1]), top=top, /nan)
        if clr eq 255 then clr = 254
        if n_f[j,i] eq 0 then clr = 255
        ; Create the vectors of X and Y values:
        X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
        Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
        POLYFILL, X, Y, COLOR = clr,/data;, /DEVICE
      endfor
    endfor
    plots,circle_v2(0,0,45),linestyle=2
    plots,circle_v2(0,0,90),linestyle=2
    oplot,[-90,90],[0,0],linestyle=2
    colorbar_v2,range=[alog10(zrange[0]),alog10(zrange[1])],/ver ,/right,charsize=2,format='(f3.1)',$
      position=[!x.window[1]+0.01,!y.window[0],!x.window[1]+0.02,!y.window[1]]


    plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
    for i=0,71 do begin
      for j=0,17 do begin

        idx = where((bin_theta_phi_arr[0,*] eq j) and (bin_theta_phi_arr[1,*] eq i))
        if idx[0] ne -1 then begin
          if keyword_set(median) then f_in_20keV[j,i] = median(f_sh_20keV_in_arr[idx]) $
          else f_in_20keV[j,i] = mean(f_sh_20keV_in_arr[idx])
          n_f[j,i] = n_elements(idx)
        endif

        flx=alog10(f_in_20keV[j,i]) > 1e-10
        phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
        th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
        clr=bytscl(flx, min=alog10(zrange[0]), max=alog10(zrange[1]), top=top, /nan)
        if clr eq 255 then clr = 254
        if n_f[j,i] eq 0 then clr = 255
        ; Create the vectors of X and Y values:
        X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
        Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
        POLYFILL, X, Y, COLOR = clr,/data;, /DEVICE
      endfor
    endfor
    plots,circle_v2(0,0,45),linestyle=2
    plots,circle_v2(0,0,90),linestyle=2
    oplot,[-90,90],[0,0],linestyle=2
    colorbar_v2,range=[alog10(zrange[0]),alog10(zrange[1])],/ver ,/right,charsize=2,format='(f3.1)',$
      position=[!x.window[1]+0.01,!y.window[0],!x.window[1]+0.02,!y.window[1]]


    ;;Plot number of measurements
    plot,[0],xrange=[-90,90],yrange=[-90,90],charsize=2,/iso
    for i=0,71 do begin
      for j=0,17 do begin

        idx = where((bin_theta_phi_arr[0,*] eq j) and (bin_theta_phi_arr[1,*] eq i))
        if idx[0] ne -1 then begin
          n_f[j,i] = n_elements(idx)
        endif

        ndat=alog10(n_f[j,i]) > 1e-10
        phi0=phi_dash_bin[i]*!radeg+2.5 & phi1=phi_dash_bin[i]*!radeg-2.5
        th0=theta_dash_bin[j]*!radeg-2.5 & th1=theta_dash_bin[j]*!radeg+2.5
        clr=bytscl(ndat, min=alog10(zrange_n[0]), max=alog10(zrange_n[1]), top=top, /nan)
        if clr eq 255 then clr = 254
        if n_f[j,i] eq 0 then clr = 255
        ; Create the vectors of X and Y values:
        X = [th0*cos(phi0*!DTOR), th0*cos(phi1*!DTOR),th1*cos(phi1*!DTOR), th1*cos(phi0*!DTOR)]
        Y = [th0*sin(phi0*!DTOR), th0*sin(phi1*!DTOR),th1*sin(phi1*!DTOR), th1*sin(phi0*!DTOR)]
        POLYFILL, X, Y, COLOR = clr,/data;, /DEVICE
      endfor
    endfor
    plots,circle_v2(0,0,45),linestyle=2
    plots,circle_v2(0,0,90),linestyle=2
    oplot,[-90,90],[0,0],linestyle=2
    colorbar_v2,range=[alog10(zrange_n[0]),alog10(zrange_n[1])],/ver ,/right,charsize=2,format='(f3.1)',$
      position=[!x.window[1]+0.01,!y.window[0],!x.window[1]+0.02,!y.window[1]]
    
    if keyword_set(ps) then pclose



      ;;Calculate number in each region 
     if keyword_set(make_notes) then journal,SAVE_LOC+'/maven/journal/'+journal_file+'.txt' ;;open journal   
     ;;region 1 (Phi>0, SZA>45)
     j0_1 = 36 & j1_1 = 71
     i0_1 = 9 & i1_1 = 17
     for j=j0_1, j1_1 do begin
       for i=i0_1,i1_1 do begin
         if i eq 0 then begin
          Out_number_gt5keV_1[i,j] = f_out_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          Out_number_gt10keV_1[i,j] = f_out_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          Out_number_20keV_1[i,j] = f_out_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_gt5keV_1[i,j] = f_in_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_gt10keV_1[i,j] = f_in_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_20keV_1[i,j] = f_in_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)          
         endif
         if i gt 0 then begin
          Out_number_gt5keV_1[i,j] =  f_out_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          Out_number_gt10keV_1[i,j] =  f_out_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          Out_number_20keV_1[i,j] =  f_out_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_gt5keV_1[i,j] =  f_in_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_gt10keV_1[i,j] =  f_in_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_20keV_1[i,j] =  f_in_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
         endif
       endfor
     endfor
     ;print,'+E (SZA>45):',total(rflct_number[i0:i1,j0:j1])
     
     

     ;;region 2 (Phi>0, SZA<45)
     j0_2 = 36 & j1_2 = 71
     i0_2 = 0 & i1_2 = 8
     for j=j0_2, j1_2 do begin
       for i=i0_2,i1_2 do begin
         if i eq 0 then begin
          Out_number_gt5keV_2[i,j] = f_out_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          Out_number_gt10keV_2[i,j] = f_out_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          Out_number_20keV_2[i,j] = f_out_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_gt5keV_2[i,j] = f_in_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_gt10keV_2[i,j] = f_in_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_20keV_2[i,j] = f_in_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)          
         endif
         if i gt 0 then begin
          Out_number_gt5keV_2[i,j] =  f_out_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          Out_number_gt10keV_2[i,j] =  f_out_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          Out_number_20keV_2[i,j] =  f_out_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_gt5keV_2[i,j] =  f_in_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_gt10keV_2[i,j] =  f_in_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_20keV_2[i,j] =  f_in_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
         endif
       endfor
     endfor
    
     

     ;;region 3 (Phi<0, SZA<45)
     j0_3 = 0 & j1_3 = 35
     i0_3 = 0 & i1_3 = 8   
      for j=j0_3, j1_3 do begin
        for i=i0_3,i1_3 do begin
         if i eq 0 then begin
          Out_number_gt5keV_3[i,j] = f_out_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          Out_number_gt10keV_3[i,j] = f_out_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          Out_number_20keV_3[i,j] = f_out_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_gt5keV_3[i,j] = f_in_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_gt10keV_3[i,j] = f_in_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_20keV_3[i,j] = f_in_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)          
         endif
         if i gt 0 then begin
          Out_number_gt5keV_3[i,j] =  f_out_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          Out_number_gt10keV_3[i,j] =  f_out_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          Out_number_20keV_3[i,j] =  f_out_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_gt5keV_3[i,j] =  f_in_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_gt10keV_3[i,j] =  f_in_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_20keV_3[i,j] =  f_in_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
         endif
        endfor
      endfor
     
      
      ;;region 4 (Phi<0, SZA>45)
      j0_4 = 0 & j1_4 = 35
      i0_4 = 9 & i1_4 = 17
      for j=j0_4, j1_4 do begin
        for i=i0_4,i1_4 do begin
         if i eq 0 then begin
          Out_number_gt5keV_4[i,j] = f_out_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          Out_number_gt10keV_4[i,j] = f_out_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          Out_number_20keV_4[i,j] = f_out_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_gt5keV_4[i,j] = f_in_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_gt10keV_4[i,j] = f_in_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          In_number_20keV_4[i,j] = f_in_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)          
         endif
         if i gt 0 then begin
          Out_number_gt5keV_4[i,j] =  f_out_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          Out_number_gt10keV_4[i,j] =  f_out_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          Out_number_20keV_4[i,j] =  f_out_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_gt5keV_4[i,j] =  f_in_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_gt10keV_4[i,j] =  f_in_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
          In_number_20keV_4[i,j] =  f_in_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
         endif
        endfor
      endfor
      
      
      ;;region all 
      j0 = 0 & j1 = 71
      i0 = 0 & i1 = 17
      for j=j0, j1 do begin
        for i=i0,i1 do begin
          if i eq 0 then begin
            ;;Rates in each bin
            Out_number_gt5keV_all[i,j] = f_out_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            Out_number_gt10keV_all[i,j] = f_out_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            Out_number_20keV_all[i,j] = f_out_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            In_number_gt5keV_all[i,j] = f_in_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            In_number_gt10keV_all[i,j] = f_in_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            In_number_20keV_all[i,j] = f_in_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            ;;Variance in each bin
            Out_number_gt10keV_all_var[i,j] = ( f_out_gt10keV_err[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres) )^2
            In_number_gt10keV_all_var[i,j] =  ( f_in_gt10keV_err[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres) )^2
          endif
          if i gt 0 then begin
            ;;Rates in each bin
            Out_number_gt5keV_all[i,j] =  f_out_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            Out_number_gt10keV_all[i,j] =  f_out_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            Out_number_20keV_all[i,j] =  f_out_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            In_number_gt5keV_all[i,j] =  f_in_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            In_number_gt10keV_all[i,j] =  f_in_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            In_number_20keV_all[i,j] =  f_in_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            ;;Variance in each bin
            Out_number_gt10keV_all_var[i,j] =  ( f_out_gt10keV_err[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm) )^2
            In_number_gt10keV_all_var[i,j] =  ( f_in_gt10keV_err[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm) )^2
          endif
        endfor
      endfor
      
      print,'NUMBER for Outward >5keV ions'
      print,'OUT +E (SZA>45):',total(OUT_number_gt5keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'OUT +E (SZA<45):',total(OUT_number_gt5keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'OUT -E (SZA<45):',total(OUT_number_gt5keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'OUT -E (SZA>45):',total(OUT_number_gt5keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'OUT ALL:        ',total(OUT_number_gt5keV_all[i0:i1,j0:j1])
      
      print,'NUMBER for Inward >5keV ions'
      print,'IN +E (SZA>45):',total(IN_number_gt5keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'IN +E (SZA<45):',total(IN_number_gt5keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'IN -E (SZA<45):',total(IN_number_gt5keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'IN -E (SZA>45):',total(IN_number_gt5keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'IN ALL:        ',total(IN_number_gt5keV_all[i0:i1,j0:j1])
      
      print,'RATIO for >5keV ions'
      print,'RATIO +E (SZA>45):',total(OUT_number_gt5keV_1[i0_1:i1_1,j0_1:j1_1])/total(IN_number_gt5keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'RATIO +E (SZA<45):',total(OUT_number_gt5keV_2[i0_2:i1_2,j0_2:j1_2])/total(IN_number_gt5keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'RATIO -E (SZA<45):',total(OUT_number_gt5keV_3[i0_3:i1_3,j0_3:j1_3])/total(IN_number_gt5keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'RATIO -E (SZA>45):',total(OUT_number_gt5keV_4[i0_4:i1_4,j0_4:j1_4])/total(IN_number_gt5keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'RATIO ALL        :',total(OUT_number_gt5keV_all[i0:i1,j0:j1])/total(IN_number_gt5keV_all[i0:i1,j0:j1])
      
      print,'NUMBER for Outward >10keV ions'
      print,'OUT +E (SZA>45):',total(OUT_number_gt10keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'OUT +E (SZA<45):',total(OUT_number_gt10keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'OUT -E (SZA<45):',total(OUT_number_gt10keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'OUT -E (SZA>45):',total(OUT_number_gt10keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'OUT ALL:        ',total(OUT_number_gt10keV_all[i0:i1,j0:j1]), '  +/-  ',sqrt(total(OUT_number_gt10keV_all_var[i0:i1,j0:j1]))

      print,'NUMBER for Inward >10keV ions'
      print,'IN +E (SZA>45):',total(IN_number_gt10keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'IN +E (SZA<45):',total(IN_number_gt10keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'IN -E (SZA<45):',total(IN_number_gt10keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'IN -E (SZA>45):',total(IN_number_gt10keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'IN ALL:        ',total(IN_number_gt10keV_all[i0:i1,j0:j1]), '  +/-  ',sqrt(total(IN_number_gt10keV_all_var[i0:i1,j0:j1]))
       
      print,'RATIO for >10keV ions'
      print,'RATIO +E (SZA>45):',total(OUT_number_gt10keV_1[i0_1:i1_1,j0_1:j1_1])/total(IN_number_gt10keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'RATIO +E (SZA<45):',total(OUT_number_gt10keV_2[i0_2:i1_2,j0_2:j1_2])/total(IN_number_gt10keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'RATIO -E (SZA<45):',total(OUT_number_gt10keV_3[i0_3:i1_3,j0_3:j1_3])/total(IN_number_gt10keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'RATIO -E (SZA>45):',total(OUT_number_gt10keV_4[i0_4:i1_4,j0_4:j1_4])/total(IN_number_gt10keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'RATIO ALL        :',total(OUT_number_gt10keV_all[i0:i1,j0:j1])/total(IN_number_gt10keV_all[i0:i1,j0:j1]), '  +/-  ', sqrt(   (  sqrt(total(OUT_number_gt10keV_all_var[i0:i1,j0:j1])) / total(IN_number_gt10keV_all[i0:i1,j0:j1])  )^2  +  (  total(OUT_number_gt10keV_all[i0:i1,j0:j1]) / total(IN_number_gt10keV_all[i0:i1,j0:j1])^2 * sqrt(total(IN_number_gt10keV_all_var[i0:i1,j0:j1]))  )^2   )
      
      print,'NUMBER for Outward >20keV ions'
      print,'OUT +E (SZA>45):',total(OUT_number_20keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'OUT +E (SZA<45):',total(OUT_number_20keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'OUT -E (SZA<45):',total(OUT_number_20keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'OUT -E (SZA>45):',total(OUT_number_20keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'OUT ALL:        ',total(OUT_number_20keV_all[i0:i1,j0:j1])
      
      print,'NUMBER for Inward >20keV ions'
      print,'IN +E (SZA>45):',total(IN_number_20keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'IN +E (SZA<45):',total(IN_number_20keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'IN -E (SZA<45):',total(IN_number_20keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'IN -E (SZA>45):',total(IN_number_20keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'IN ALL:        ',total(IN_number_20keV_all[i0:i1,j0:j1])
       
      print,'RATIO for >20keV ions'
      print,'RATIO +E (SZA>45):',total(OUT_number_20keV_1[i0_1:i1_1,j0_1:j1_1])/total(IN_number_20keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'RATIO +E (SZA<45):',total(OUT_number_20keV_2[i0_2:i1_2,j0_2:j1_2])/total(IN_number_20keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'RATIO -E (SZA<45):',total(OUT_number_20keV_3[i0_3:i1_3,j0_3:j1_3])/total(IN_number_20keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'RATIO -E (SZA>45):',total(OUT_number_20keV_4[i0_4:i1_4,j0_4:j1_4])/total(IN_number_20keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'RATIO ALL        :',total(OUT_number_20keV_all[i0:i1,j0:j1])/total(IN_number_20keV_all[i0:i1,j0:j1])
      
      if keyword_set(make_notes) then journal
   
   
   
   
   stop
   
   
end