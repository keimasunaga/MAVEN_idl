pro plot_mvn_swi_slice2d,load=load
 
   if keyword_set(load) then begin
     mvn_spice_load
     mvn_swia_load_l2_data,/loadcoarse,/tplot
     mvn_mag_load,spice_frame='MAVEN_MSO'
     mvn_swia_add_magf
   endif
   
   mvn_swia_slice2d_snap,xrange=[-1500,1500],/mso,rot='perp',charsize=1,/showdata,erange=[100,30000]

end