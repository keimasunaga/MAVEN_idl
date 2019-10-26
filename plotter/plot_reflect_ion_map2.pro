pro plot_reflect_ion_map2
   
   
   
 


    orbit_arr = 336 + findgen(100) 

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    
    nr = 16.
    nt = 18.
    
    
    res = 0.25
    range = 4.
    nbin = 2.*range/res     
    each_axis = (findgen(nbin+1) - nbin/2.)*res
    f_xz_t = fltarr(nbin+1,nbin+1)
    f_xz_t2 = fltarr(nbin+1,nbin+1)
    f_xz_x = fltarr(nbin+1,nbin+1)
    f_xz_x2 = fltarr(nbin+1,nbin+1)
    f_xz_z = fltarr(nbin+1,nbin+1)
    f_xz_z2 = fltarr(nbin+1,nbin+1) 

    
    v_xz_x = fltarr(nbin+1,nbin+1)
    v_xz_x2 = fltarr(nbin+1,nbin+1)
    v_xz_z = fltarr(nbin+1,nbin+1)
    v_xz_z2 = fltarr(nbin+1,nbin+1)  
    n_xz = fltarr(nbin+1,nbin+1)
   
    f_yz_t = fltarr(nbin+1,nbin+1)
    f_yz_t2 = fltarr(nbin+1,nbin+1)
    f_yz_y = fltarr(nbin+1,nbin+1)
    f_yz_y2 = fltarr(nbin+1,nbin+1)
    f_yz_z = fltarr(nbin+1,nbin+1)
    f_yz_z2 = fltarr(nbin+1,nbin+1)

    v_yz_y = fltarr(nbin+1,nbin+1)
    v_yz_y2 = fltarr(nbin+1,nbin+1)
    v_yz_z = fltarr(nbin+1,nbin+1)
    v_yz_z2 = fltarr(nbin+1,nbin+1)
    n_yz = fltarr(nbin+1,nbin+1)
   
    
    
    
    
    
    
    
    
    
    theta_arr = findgen(103)*!DTOR
    L = 2.1
    e = 1.05
    X0 = 0.55

    r = L/(1.+e*cos(theta_arr))



    A = L*cos(theta_arr)/(1.+e*cos(theta_arr))
    B = L*sin(theta_arr)/(1.+e*cos(theta_arr))

    theta_dash_arr = acos((X0 + A)/sqrt((X0+A)^2+B^2))

    r_dash = (X0 + r*cos(theta_arr))/cos(theta_dash_arr)


    theta_dash_bin = (findgen(18)*5 + 2.5)*!DTOR
    theta_dash_bin_edge = findgen(19)*5.*!DTOR
    r_dash_bin = interpol(r_dash,theta_dash_arr,theta_dash_bin)
    r_dash_bin_edge = interpol(r_dash,theta_dash_arr,theta_dash_bin_edge)
    rth_bin = fltarr(18) & rph_bin = fltarr(18)
    
    
    for i=0,n_elements(r_dash_bin)-1 do rth_bin[i] = sqrt(r_dash_bin_edge[i]^2 + r_dash_bin_edge[i+1]^2 - 2.*r_dash_bin_edge[i]*r_dash_bin_edge[i+1]*cos(5.*!DTOR))
    for i=0,n_elements(r_dash_bin)-1 do rph_bin[i] = r_dash_bin_edge[i]*sin(theta_dash_bin[i])*(5.*!DTOR)
  
    
    phi_dash_bin = ((findgen(72)*5. + 2.5) - 180.)*!DTOR 
    
    
    thres = 5 * !DTOR
    phres = 5 * !DTOR
    rres = 0.2 * r_dash_bin
    
    nr = 10
    nth = !pi/2./thres
    nph = 2.*!pi/thres
    
    f_rtp = fltarr(nr,nth,nph) & f_tp = fltarr(nth,nph) & f_tp_avg = fltarr(nth,nph)
    n_rtp = fltarr(nr,nth,nph) & n_tp = fltarr(nth,nph)
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
                  
          Vmso = dat_sav.Vmso
          Bmso = dat_sav.Bmso
          fnt_v = finite(Vmso)
          fnt_b = finite(Bmso)
          if fnt_v[0] eq 0 or fnt_v[1] eq 0 or fnt_v[2] eq 0 then goto, skip
          if fnt_b[0] eq 0 or fnt_b[1] eq 0 or fnt_b[2] eq 0 then goto, skip
          rot = get_rot_angle(Vmso,Bmso)   
        
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
          
          if pos_th gt 90 * !DTOR then goto,skip
          ith = fix(pos_th/thres)
          iph = fix(pos_ph/phres) + 35.5
          ;if iph eq 72 then iph = 0 
          ir = fix(pos_r/rres[ith])
;
;          print,transpose(pos_mse)
;          print,pos_th*!radeg,pos_ph*!radeg
;          print,ith,iph
          
          
          th_m = theta_dash_bin[ith] ;- 2.5 * !DTOR
          th_p = theta_dash_bin[ith] + 5. * !DTOR
          ph_m = phi_dash_bin[iph] ;- 2.5 * !DTOR
          ph_p = phi_dash_bin[iph] + 5. * !DTOR
          
          x1 = r_dash_bin[ir] * cos(th_m)
          y1 = r_dash_bin[ir] * sin(th_m) * cos(ph_m)
          z1 = r_dash_bin[ir] * sin(th_m) * sin(ph_m)
          
          x2 = r_dash_bin[ir] * cos(th_p)
          y2 = r_dash_bin[ir] * sin(th_p) * cos(ph_m)
          z2 = r_dash_bin[ir] * sin(th_p) * sin(ph_m)
          
          x3 = r_dash_bin[ir] * cos(th_p)
          y3 = r_dash_bin[ir] * sin(th_p) * cos(ph_p)
          z3 = r_dash_bin[ir] * sin(th_p) * sin(ph_p)
          
          x4 = r_dash_bin[ir] * cos(th_m)
          y4 = r_dash_bin[ir] * sin(th_m) * cos(ph_p)
          z4 = r_dash_bin[ir] * sin(th_m) * sin(ph_p)
          
                  
          Xg = (x1+x2+x3+x4)/4.
          Yg = (y1+y2+y3+y4)/4.
          Zg = (z1+z2+z3+z4)/4.

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

          vec_norm = crossp(vec_12,vec_14)
          vec_norm2 = vec_norm/total(vec_norm^2)^.5
          f_shock = inpro(Oflux_mse_10keV,vec_norm2)
          ;shock_norm = [ [shock_norm],[vec_norm2] ]
          ;bs_pos2 = [[bs_pos2], [xg,yg,zg]]
          
          ;f_rtp[ir,ith,iph] = f_rtp[ir,ith,iph] + f_shock
          ;n_rtp[ir,ith,iph] = n_rtp[ir,ith,iph] + 1.

          f_tp[ith,iph] = f_tp[ith,iph] + f_shock
          n_tp[ith,iph] = n_tp[ith,iph] + 1.
          
          
          
          
;          plotxyz,phi_dash_bin*!radeg,theta_dash_bin*!radeg,transpose(f_tp/n_tp),xtit='phi',ytit='theta',multi='1,2'
;          plotxyz,phi_dash_bin*!radeg,theta_dash_bin*!radeg,transpose(n_tp),xtit='phi',ytit='theta',/add
;          
         
          
          i = round(pos_mse[0]/res) + nbin/2.
          j = round(pos_mse[1]/res) + nbin/2.
          k = round(pos_mse[2]/res) + nbin/2.
          
          f_xz_t[i,k] = f_xz_t[i,k] + total(Oflux_mse_20keV^2)^.5
          f_xz_x[i,k] = f_xz_x[i,k] + Oflux_mse_20keV[0]
          f_xz_z[i,k] = f_xz_z[i,k] + Oflux_mse_20keV[2]
          v_xz_x[i,k] = v_xz_x[i,k] + Ovel_mse_20keV[0]
          v_xz_z[i,k] = v_xz_z[i,k] + Ovel_mse_20keV[2]
          n_xz[i,k] = n_xz[i,k] + 1.
          
          f_yz_t[j,k] = f_yz_t[j,k] + total(Oflux_mse_20keV^2)^.5        
          f_yz_y[j,k] = f_yz_y[j,k] + Oflux_mse_20keV[1]
          f_yz_z[j,k] = f_yz_z[j,k] + Oflux_mse_20keV[2]
          v_yz_y[j,k] = v_yz_y[j,k] + Ovel_mse_20keV[1]
          v_yz_z[j,k] = v_yz_z[j,k] + Ovel_mse_20keV[2]
          n_yz[j,k] = n_yz[j,k] + 1.
          
         
         skip:
      endfor   
      no_data:
    endfor
    stop
    idx = where(n_tp ne 0)
    f_tp_avg[idx] = f_tp[idx]/n_tp[idx]
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
    
    
    stop
    
    
    !p.multi=[0,2,1]
    idx = where(n_xz ne 0)
    f_xz_t2[idx] = f_xz_t[idx]/n_xz[idx]
    f_xz_x2[idx] = f_xz_x[idx]/n_xz[idx]
    f_xz_z2[idx] = f_xz_z[idx]/n_xz[idx]
    v_xz_x2[idx] = v_xz_x[idx]/n_xz[idx]
    v_xz_z2[idx] = v_xz_z[idx]/n_xz[idx]

    
    idx = where(n_yz ne 0)
    f_yz_t2[idx] = f_yz_t[idx]/n_yz[idx]
    f_yz_y2[idx] = f_yz_y[idx]/n_yz[idx]
    f_yz_z2[idx] = f_yz_z[idx]/n_yz[idx]
    v_yz_y2[idx] = v_yz_y[idx]/n_yz[idx]
    v_yz_z2[idx] = v_yz_z[idx]/n_yz[idx]
    
    
    plotxyz,each_axis,each_axis,f_xz_t2,mult='2,2',zrange=[0,1000],xrange=[4,0],yrange=[-4,4],tit='Flux',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    plotxyz,each_axis,each_axis,n_xz,/add,zrange=[0,100],xrange=[4,0],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/xz

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
    
    
    plotxyz,each_axis,each_axis,f_yz_t2,mult='2,2',zrange=[0,1000],xrange=[-4,4],yrange=[-4,4],tit='Flux',xtit='Y',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    plotxyz,each_axis,each_axis,n_yz,/add,zrange=[0,100],xrange=[-4,4],yrange=[-4,4],xtit='Y',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/yz

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