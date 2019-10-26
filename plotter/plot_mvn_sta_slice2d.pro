pro plot_mvn_sta_slice2d,load=load

  if keyword_set(load) then begin
    mvn_spice_load
    mvn_sta_l2_load,/tplot
    mvn_mag_load,spice_frame='MAVEN_MSO'
  endif

  mvn_sta_slice2d_snap, /arc, /bline, /mso, mass=[12,20],/showdata,_extra={rot: 'perp',charsize: 1,xrange:[-1500,1500],range:[1e-18,1e-5]}

end