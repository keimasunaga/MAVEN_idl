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




pro plot_reflect_ion_map4,energy
   
   

    orbit_arr = 336+findgen(472);346;,347,348,349,350] ;+ findgen(472) 
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    
    ;;Define variables
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
    nr = 10. ;; scale number of the resolution bin of the r direction
      
    ;;calculate r' (Distance to the shock surface from the center of Mars)
    theta_dash_arr = acos((X0+X)/sqrt((X0+X)^2+Y^2)) ;; Angle from the X axis (data points correspnd to those of theta_arr)
    r_dash = (X0 + r_arr*cos(theta_arr))/cos(theta_dash_arr) ;;r'      ;(X0 + r_arr*cos(theta_arr))/cos(theta_dash_arr)

    ;;r' has to be interpolated by data points of theta_dash_bin because data points of r' are correspoinding to those of theta_dash_arr
    r_dash_bin = interpol(r_dash,theta_dash_arr,theta_dash_bin)
    r_dash_bin_edge = interpol(r_dash,theta_dash_arr,theta_dash_bin_edge)
    rres = r_dash_bin / nr
    rres_edge = r_dash_bin_edge / nr
   

    for i=0,n_elements(r_dash_bin)-1 do rth_bin[i] = sqrt(r_dash_bin_edge[i]^2 + r_dash_bin_edge[i+1]^2 - 2.*r_dash_bin_edge[i]*r_dash_bin_edge[i+1]*cos(5.*!DTOR))
    for i=0,n_elements(r_dash_bin)-1 do rph_bin[i] = r_dash_bin_edge[i]*sin(theta_dash_bin[i])*(5.*!DTOR)
stop
 
    nth = !pi/2./thres
    nph = 2.*!pi/thres
    
    f_rtp = fltarr(2*nr,nth,nph) & f_tp = fltarr(nth,nph) & f_tp_avg = fltarr(nth,nph)
    n_rtp = fltarr(2*nr,nth,nph) & n_tp = fltarr(nth,nph)
    Rm = 3389.9D * 1e3 * 1e2 ;[cm]
    rflct_number = fltarr(18,72)
   
    for in=0,n_elements(orbit_arr)-1 do begin  
      
      
      filename = SAVE_LOC + '/maven/sav/flux/deflect_ion/d0/'+string(orbit_arr[in],format='(i05)')+'/flux_vdf_*.sav'   
      fs = file_search(filename)
      if fs[0] eq '' then goto, no_data
      
      file_sw = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit_arr[in],format='(i05)')+'.sav'
      ft_sw = file_test(file_sw)
      if ft_sw eq 0 then goto, no_data
      restore,file_sw
      time_bs = dat_sw.time 
      
      for ii=0,n_elements(fs)-1 do begin
          
          restore,fs[ii]
          time = dat_sav.time
          mtime = total(time)/2d
          if mtime gt time_bs[0] and mtime lt time_bs[1] then goto, skip
          ;if mtime lt time_bs[0] or mtime gt time_bs[1] then goto,skip       
          Vmso = dat_sav.Vmso
          Bmso = dat_sav.Bmso
          fnt_v = finite(Vmso)
          fnt_b = finite(Bmso)
          if fnt_v[0] eq 0 or fnt_v[1] eq 0 or fnt_v[2] eq 0 then goto, skip
          if fnt_b[0] eq 0 or fnt_b[1] eq 0 or fnt_b[2] eq 0 then goto, skip
          rot = get_rot_angle(Vmso,Bmso)
          
          pos_mso = dat_sav.pos_mso
          pos_mse = mso2mse(pos_mso[0],pos_mso[1],pos_mso[2],rot)
          pos_r = total(pos_mse^2)^.5
          pos_th = acos(pos_mse[0]/pos_r)
          if pos_mse[2] ge 0 then pos_ph = acos(pos_mse[1]/pos_r/sin(pos_th)) $
          else pos_ph = -acos(pos_mse[1]/pos_r/sin(pos_th))
          if pos_mse[0] lt 0 then goto, skip


          ;;; calculate shock normal according to MAVEN positions in the MSE frame 
          ith = fix(pos_th/thres)
          iph = fix(pos_ph/phres) + 35.5
          ir = fix(pos_r/rres[ith])

          th_m = theta_dash_bin_edge[ith]
          th_p = theta_dash_bin_edge[ith+1]
          ph_m = phi_dash_bin[iph]
          ph_p = phi_dash_bin[iph+1]
          r_m = r_dash_bin_edge[ith] * ir/nr
          r_p = r_dash_bin_edge[ith+1] * ir/nr

         ;;calculate 4 points sorrounding a center of the bin
          x1 = r_m * cos(th_m)
          y1 = r_m * sin(th_m) * cos(ph_m)
          z1 = r_m * sin(th_m) * sin(ph_m)

          x2 = r_p * cos(th_p)
          y2 = r_p * sin(th_p) * cos(ph_m)
          z2 = r_p * sin(th_p) * sin(ph_m)

          x3 = r_p * cos(th_p)
          y3 = r_p * sin(th_p) * cos(ph_p)
          z3 = r_p * sin(th_p) * sin(ph_p)

          x4 = r_m * cos(th_m)
          y4 = r_m * sin(th_m) * cos(ph_p)
          z4 = r_m * sin(th_m) * sin(ph_p)

         ;; calculate a center of the bin
          Xg = (x1+x2+x3+x4)/4.
          Yg = (y1+y2+y3+y4)/4.
          Zg = (z1+z2+z3+z4)/4.

         ;;Define variables to calculate normal vector of the bin
          a1 = X1-Xg
          b1 = Y1-Yg
          c1 = Z1-Zg
          a2 = X2-Xg
          b2 = Y2-Yg
          c2 = Z2-Zg
          a3 = X3-Xg
          b3 = Y3-Yg
          c3 = Z3-Zg
          a4 = X4-Xg
          b4 = Y4-Yg
          c4 = Z4-Zg
          vec_12 = [a2-a1, b2-b1, c2-c1]
          vec_14 = [a4-a1, b4-b1, c4-c1]
          vec_13 = [a3-a1, b3-b1, c3-c1]

        ;;calculate normal a vector. Normal vector of a square ABCD can be calculated by crossp(AB,AD)
          vec_sh_norm = crossp(vec_12,vec_14)
          vec_sh_norm2 = vec_sh_norm/total(vec_sh_norm^2)^.5

          if x1 eq x4 and y1 eq y4 and z1 eq z4 then begin
            vec_sh_norm = crossp(vec_12,vec_13)
            vec_sh_norm2 = vec_sh_norm/total(vec_sh_norm^2)^.5
          endif
      
      
      
                    
          
       ;; convert Oflux from MSO to MSE            
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





         f_shock = inpro(Oflux_mse,vec_sh_norm2)

         f_rtp[ir,ith,iph] = f_rtp[ir,ith,iph] + f_shock
         n_rtp[ir,ith,iph] = n_rtp[ir,ith,iph] + 1.
    
         
         skip:
      endfor   
      no_data:
    endfor
    
    
    f_tp = -total(f_rtp[*,*,*],1)
    n_tp = total(n_rtp[*,*,*],1)
    idx = where(n_tp ne 0)
    f_tp_avg[idx] = f_tp[idx]/n_tp[idx] > 1e-10    
    plotxyz,phi_dash_bin*!radeg,theta_dash_bin*!radeg,alog10(transpose(f_tp_avg)),xtit='phi',ytit='theta',multi='1,2',charsize=2,zrange=[0,4]
    oplot,[0,0],!y.crange,line=2
    plotxyz,phi_dash_bin*!radeg,theta_dash_bin*!radeg,alog10(transpose(n_tp)),xtit='phi',ytit='theta',/add,charsize=2,zrange=[0,3]
    oplot,[0,0],!y.crange,line=2
    
    for j=0, 71 do begin
      for i=0,17 do begin
       if i eq 0 then rflct_number[i,j] = f_tp_avg[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
       if i gt 0 then rflct_number[i,j] =  f_tp_avg[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
      endfor
    endfor
    
    print,total(rflct_number)
    
  stop
    
  
end