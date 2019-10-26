pro tplot_mvn_ecnv,noplot=noplot


  if tplot_exist('mvn_swica_velocity_mso') then begin
    tinterpol_mxn,'mvn_B_1sec_MAVEN_MSO','mvn_swica_velocity_mso'
    get_data,'mvn_swica_velocity_mso',data=Vdat
    get_data,'mvn_B_1sec_MAVEN_MSO_interp',data=Bdat
    goto, skip_swics
  endif
  
  if tplot_exist('mvn_swics_velocity_mso') then begin
    tinterpol_mxn,'mvn_B_1sec_MAVEN_MSO','mvn_swics_velocity_mso'
    get_data,'mvn_swics_velocity_mso',data=Vdat
    get_data,'mvn_B_1sec_MAVEN_MSO_interp',data=Bdat
  endif
  
  skip_swics:
  E = dblarr(n_elements(Vdat.x),3)
  for i=0,n_elements(Vdat.x)-1 do E[i,*] = -crossp(Vdat.y[i,*],Bdat.y[i,*])
  
  Etot = total(E^2,2)^.5
  store_data,'Ecnv',data={x:Vdat.x, y:E}
  store_data,'Ecnv_tot',data={x:Vdat.x, y:Etot}
  store_data,'Ecnv_all',data=['Esw','Etot']
  options,'Esw',colors=[80,120,230]
  if not keyword_set(noplot) then tplot,'Esw'
end