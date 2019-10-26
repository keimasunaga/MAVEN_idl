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




pro plot_reflect_ion_map10_v4,median=median,frange=frange,$
                              sw_dpnd=sw_dpnd,Rg=Rg,Vperp=Vperp,Bt=Bt,Pdy=Pdy,large=large,small=small,$
                              make_notes=make_notes,journal_file=journal_file,ps=ps,fileps=fileps
   
   

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
    
    f_out_gt5keV = fltarr(nth,nph) 
    f_out_gt10keV = fltarr(nth,nph) 
    f_out_20keV = fltarr(nth,nph)
    f_in_gt5keV = fltarr(nth,nph)
    f_in_gt10keV = fltarr(nth,nph) 
    f_in_20keV = fltarr(nth,nph)
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
    Out_number_gt10keV_all = fltarr(18,72)
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
    In_number_gt10keV_all = fltarr(18,72)
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
   Nsw_arr = 0.
   Vmso_arr = [0.,0.,0.]
   Bmso_arr = [0.,0.,0.]
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
      stime = dat_sav_arr.stime
      etime = dat_sav_arr.etime
      mtime = (stime + etime)/2.d
      mtime_arr = [mtime_arr,mtime]
      Nsw = dat_sav_arr.Dens
      Vmso = dat_sav_arr.Vmso
      Bmso = dat_sav_arr.Bmso
      Nsw_arr = [Nsw_arr, Nsw]
      Vmso_arr = [[Vmso_arr], [Vmso]]
      Bmso_arr = [[Bmso_arr], [Bmso]] 
      bin_theta_phi_arr = [[bin_theta_phi_arr],[dat_sav_arr.bin_theta_phi]]
      

      Odens_gt5keV_out = dat_sav_arr.Odens_gt5keV_out
      Ovel_sh_gt5keV_out = dat_sav_arr.Ovel_sh_gt5keV_out
      f_sh_gt5keV_out = Odens_gt5keV_out * Ovel_sh_gt5keV_out
      f_sh_gt5keV_out_arr = [f_sh_gt5keV_out_arr, f_sh_gt5keV_out]
      Odens_gt5keV_in = dat_sav_arr.Odens_gt5keV_in
      Ovel_sh_gt5keV_in = dat_sav_arr.Ovel_sh_gt5keV_in
      f_sh_gt5keV_in = Odens_gt5keV_in * Ovel_sh_gt5keV_in
      f_sh_gt5keV_in_arr = [f_sh_gt5keV_in_arr, f_sh_gt5keV_in]

      Odens_gt10keV_out = dat_sav_arr.Odens_gt10keV_out
      Ovel_sh_gt10keV_out = dat_sav_arr.Ovel_sh_gt10keV_out
      f_sh_gt10keV_out = Odens_gt10keV_out * Ovel_sh_gt10keV_out
      f_sh_gt10keV_out_arr = [f_sh_gt10keV_out_arr, f_sh_gt10keV_out]
      Odens_gt10keV_in = dat_sav_arr.Odens_gt10keV_in
      Ovel_sh_gt10keV_in = dat_sav_arr.Ovel_sh_gt10keV_in
      f_sh_gt10keV_in = Odens_gt10keV_in * Ovel_sh_gt10keV_in
      f_sh_gt10keV_in_arr = [f_sh_gt10keV_in_arr, f_sh_gt10keV_in]
      
      Odens_20keV_out = dat_sav_arr.Odens_20keV_out
      Ovel_sh_20keV_out = dat_sav_arr.Ovel_sh_20keV_out
      f_sh_20keV_out = Odens_20keV_out * Ovel_sh_20keV_out
      f_sh_20keV_out_arr = [f_sh_20keV_out_arr, f_sh_20keV_out]
      Odens_20keV_in = dat_sav_arr.Odens_20keV_in
      Ovel_sh_20keV_in = dat_sav_arr.Ovel_sh_20keV_in
      f_sh_20keV_in = Odens_20keV_in * Ovel_sh_20keV_in
      f_sh_20keV_in_arr = [f_sh_20keV_in_arr, f_sh_20keV_in]
      
      
      no_data:
    endfor
    
    mtime_arr = mtime_arr[1:*]
    Nsw_arr = Nsw_arr[1:*]
    Vmso_arr = Vmso_arr[*,1:*]
    Vmso_t_arr = total(Vmso_arr^2,1)^.5
    Pdy_arr = Nsw_arr * Vmso_t_arr^2 * 1.6726 * 1e-6
    Bmso_arr = Bmso_arr[*,1:*]
    Bmso_t_arr = total(Bmso_arr^2,1)^.5
    Vperp_arr = fltarr(n_elements(mtime_arr))
    Vpara_arr = fltarr(n_elements(mtime_arr))
    for i=0,n_elements(mtime_arr)-1 do begin
         Vperp_arr[i] = norm(crossp(Vmso_arr[*,i],Bmso_arr[*,i])/norm(Bmso_arr[*,i]))
         Vpara_arr[i] = (transpose(Bmso_arr[*,i]) # Vmso_arr[*,i])/norm(Bmso_arr[*,i])
    endfor
    Moxy = 16.*1.67e-27 ;;[kg]
    q = 1.602e-19 ;;[C]
    Rg_arr = Moxy*Vperp_arr/q/(Bmso_t_arr*1e-9)
    
    bin_theta_phi_arr = bin_theta_phi_arr[*,1:*]    
    f_sh_gt5keV_out_arr = f_sh_gt5keV_out_arr[1:*]*1e5
    f_sh_gt5keV_in_arr = f_sh_gt5keV_in_arr[1:*]*1e5
    f_sh_gt10keV_out_arr = f_sh_gt10keV_out_arr[1:*]*1e5
    f_sh_gt10keV_in_arr = f_sh_gt10keV_in_arr[1:*]*1e5
    f_sh_20keV_out_arr = f_sh_20keV_out_arr[1:*]*1e5
    f_sh_20keV_in_arr = f_sh_20keV_in_arr[1:*]*1e5
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;solar wind dependences;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
    if keyword_set(sw_dpnd) then begin
      !p.multi=[0,2,2]
      dBt = 0.1
      Bt_hist = histogram(Bmso_t_arr,binsize=dBt)
      hist_bins_Bt = ( findgen( n_elements(Bmso_t_arr) ) * dBt ) + min( Bmso_t_arr )
      plot,hist_bins_Bt,Bt_hist,psym=10,xrange=[0,max(Bmso_t_arr)],charsize=1.5,xtit='Btotal [nT]'
      oplot,[median(Bmso_t_arr),median(Bmso_t_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Bmso_t_arr),mean(Bmso_t_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      
      dVperp = 10
      Vperp_hist = histogram(Vperp_arr,binsize=dVperp)
      hist_bins_Vperp = ( findgen( n_elements(Vperp_arr) ) * dVperp ) + min( Vperp_arr )
      plot,hist_bins_Vperp,Vperp_hist,psym=10,xrange=[0,max(Vperp_arr)],charsize=1.5,xtit='Vperp [km/s]'
      oplot,[median(Vperp_arr),median(Vperp_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Vperp_arr),mean(Vperp_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      
      dPdy = 0.05
      Pdy_hist = histogram(Pdy_arr,binsize=dPdy)
      hist_bins_Pdy = ( findgen( n_elements(Pdy_arr) ) * dPdy) + min( Pdy_arr )
      plot,hist_bins_Pdy,Pdy_hist,psym=10,xrange=[0,max(Pdy_arr)],charsize=1.5,xtit='Pdy [nPa]'
      oplot,[median(Pdy_arr),median(Pdy_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Pdy_arr),mean(Pdy_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
      
      dRg = 1000
      Rg_hist = histogram(Rg_arr,binsize=dRg)
      hist_bins_Rg = ( findgen( n_elements(Rg_arr) ) * dRg ) + min( Rg_arr )
      plot,hist_bins_Rg,Rg_hist,psym=10,xrange=[0,max(Rg_arr)],charsize=1.5,xtit='Rg [km]'
      oplot,[median(Rg_arr),median(Rg_arr)],[!y.crange[0],!y.crange[1]],line=2
      oplot,[mean(Rg_arr),mean(Rg_arr)],[!y.crange[0],!y.crange[1]],line=2,color=200
       
       if keyword_set(Bt) then begin
         if keyword_set(Small) then idx = where(Bmso_t_arr le median(Bmso_t_arr))
         if keyword_set(Large) then idx = where(Bmso_t_arr gt median(Bmso_t_arr))  
       endif
       
       if keyword_set(Vperp) then begin
         if keyword_set(Small) then idx = where(Vperp_arr le median(Vperp_arr))
         if keyword_set(Large) then idx = where(Vperp_arr gt median(Vperp_arr))
       endif
       
       if keyword_set(Rg) then begin
         if keyword_set(Small) then idx = where(Rg_arr le median(Rg_arr))
         if keyword_set(Large) then idx = where(Rg_arr gt median(Rg_arr))
       endif
       
       if keyword_set(Pdy) then begin
         if keyword_set(Small) then idx = where(Pdy_arr le median(Pdy_arr))
         if keyword_set(Large) then idx = where(Pdy_arr gt median(Pdy_arr))
       endif
       
       bin_theta_phi_arr = bin_theta_phi_arr[*,idx]
       f_sh_gt5keV_out_arr = f_sh_gt5keV_out_arr[idx]
       f_sh_gt5keV_in_arr = f_sh_gt5keV_in_arr[idx]
       f_sh_gt10keV_out_arr = f_sh_gt10keV_out_arr[idx]
       f_sh_gt10keV_in_arr = f_sh_gt10keV_in_arr[idx]
       f_sh_20keV_out_arr = f_sh_20keV_out_arr[idx]
       f_sh_20keV_in_arr = f_sh_20keV_in_arr[idx]
       
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
            Out_number_gt5keV_all[i,j] = f_out_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            Out_number_gt10keV_all[i,j] = f_out_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            Out_number_20keV_all[i,j] = f_out_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            In_number_gt5keV_all[i,j] = f_in_gt5keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            In_number_gt10keV_all[i,j] = f_in_gt10keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
            In_number_20keV_all[i,j] = f_in_20keV[i,j] * (0.5 * (rth_bin[i]*Rm)^2 * phres)
          endif
          if i gt 0 then begin
            Out_number_gt5keV_all[i,j] =  f_out_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            Out_number_gt10keV_all[i,j] =  f_out_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            Out_number_20keV_all[i,j] =  f_out_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            In_number_gt5keV_all[i,j] =  f_in_gt5keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            In_number_gt10keV_all[i,j] =  f_in_gt10keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
            In_number_20keV_all[i,j] =  f_in_20keV[i,j] * (0.5 * (rth_bin[i-1]*Rm) * (rph_bin[i] + rph_bin[i])*Rm)
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
      print,'OUT ALL:        ',total(OUT_number_gt10keV_all[i0:i1,j0:j1])

      print,'NUMBER for Inward >10keV ions'
      print,'IN +E (SZA>45):',total(IN_number_gt10keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'IN +E (SZA<45):',total(IN_number_gt10keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'IN -E (SZA<45):',total(IN_number_gt10keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'IN -E (SZA>45):',total(IN_number_gt10keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'IN ALL:        ',total(IN_number_gt10keV_all[i0:i1,j0:j1])
       
      print,'RATIO for >10keV ions'
      print,'RATIO +E (SZA>45):',total(OUT_number_gt10keV_1[i0_1:i1_1,j0_1:j1_1])/total(IN_number_gt10keV_1[i0_1:i1_1,j0_1:j1_1])
      print,'RATIO +E (SZA<45):',total(OUT_number_gt10keV_2[i0_2:i1_2,j0_2:j1_2])/total(IN_number_gt10keV_2[i0_2:i1_2,j0_2:j1_2])
      print,'RATIO -E (SZA<45):',total(OUT_number_gt10keV_3[i0_3:i1_3,j0_3:j1_3])/total(IN_number_gt10keV_3[i0_3:i1_3,j0_3:j1_3])
      print,'RATIO -E (SZA>45):',total(OUT_number_gt10keV_4[i0_4:i1_4,j0_4:j1_4])/total(IN_number_gt10keV_4[i0_4:i1_4,j0_4:j1_4])
      print,'RATIO ALL        :',total(OUT_number_gt10keV_all[i0:i1,j0:j1])/total(IN_number_gt10keV_all[i0:i1,j0:j1])
      
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
      
      
   
   
   
   
   
   
   
end