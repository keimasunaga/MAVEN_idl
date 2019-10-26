  ;+
  ; :Description:
  ;    Purpose of this routine is to display (and save) a SAVED time series of pfp (plasma and field package) of single orbit
  ;
  ; ${parameters}
  ;  orbit: orbit number
  ;
  ; ${keywords}
  ;
  ; ${Related routines}
  ;  tplot_mvn_pfp_orbit
  ;
  ; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
  ;
  ; $Last modified May 13, 2014
  ;-


; sub routine

pro rename_temp,tvar_arr,num=num

  if not keyword_set(num) then num = 0
  for i=0,n_elements(tvar_arr)-1 do store_data,tvar_arr[i],newname=tvar_arr[i]+'_'+strcompress(string(num),/remove)

end

pro cmb_tvar,tvar_arr,num1,num2



   for j=0,n_elements(tvar_arr)-1 do begin
     
     ;get_data,tvar_arr[j]+'_'+strcompress(string(num1),/remove),data1
     ;get_data,tvar_arr[j]+'_'+strcompress(string(num2),/remove),data2
     store_data,tvar_arr[j],data=[ tvar_arr[j]+'_'+strcompress(string(num1),/remove), tvar_arr[j]+'_'+strcompress(string(num2),/remove) ]
   endfor
   
end

pro get_lims,tva_arr,num

 ; for k=0,n_elements(tvar_arr)-1 get_data,tvar_arr[k],data=data
 ; ylim,tvar_arr[k],

end


; main routine
pro tplot_saved_mvn_pfp_orbit_bk2,orbit,noplot=noplot

   del_data,'*'

   if n_elements(orbit) eq 1 then begin
     env = init_env()
     SAVE_LOC = env.SAVE_LOC
     filename = SAVE_LOC +'/maven/tplot_save/orbit/tplot_mvn_pfp_orbit_'+string(orbit,format='(I05)')+'.tplot'
     tplot_restore,filename=filename
     tspan = mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
     timespan,tspan
     ;if tplot_exist('mvn_swim_velocity_mso') eq 1 then begin
     tplot_vec_tot,'mvn_swim_velocity_mso'
     store_data,'mvn_swim_velocity_mso_all',data=['mvn_swim_velocity_mso','mvn_swim_velocity_mso_tot']
     ;endif 
   
     ;if tplot_exist('mvn_B_1sec_MAVEN_MSO') eq 1 then begin
     tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
     store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
     ;endif
   
     store_data,'alt2',data= ['wind_','sheath_','pileup_','wake_']
     options,'sheath_',colors=4
     options,'pileup_',colors=5
     options,'wake_',colors=2
     
     tplot_options,'var_label',['orbnum_','lat_','lon_','Zmso','Ymso','Xmso']

   if ~keyword_set(noplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso_all','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza_']

   endif

   


   if n_elements(orbit) eq 2 then begin
     env = init_env()
     SAVE_LOC = env.SAVE_LOC
     filename0 = SAVE_LOC +'/maven/tplot_save/orbit/tplot_mvn_pfp_orbit_'+string(orbit[0],format='(I05)')+'.tplot'
     filename1 = SAVE_LOC +'/maven/tplot_save/orbit/tplot_mvn_pfp_orbit_'+string(orbit[1],format='(I05)')+'.tplot'
     print,filename0
     tplot_restore,filename=filename0
     tspan = mvn_orbit_num(orbnum=[orbit[0]-0.5,orbit[1]+0.5])
     timespan,tspan

     tplot_vec_tot,'mvn_swim_velocity_mso'
     tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
         
     rename_temp,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M',$
                  'mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso','mvn_swim_velocity_mso_tot','mvn_swim_pdy','mvn_swim_temperature_mso','mvn_swim_pressure',$
                  'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
                  'wind_','sheath_','pileup_','wake_','lat_','lon_','sza_','Zmso','Ymso','Xmso'],num=1
  
     tplot_restore,filename=filename1
     
     rename_temp,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M',$
                  'mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso','mvn_swim_velocity_mso_tot','mvn_swim_pdy','mvn_swim_temperature_mso','mvn_swim_pressure',$
                  'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
                  'wind_','sheath_','pileup_','wake_','lat_','lon_','sza_','Zmso','Ymso','Xmso'],num=2
      
     cmb_tvar,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M',$
               'mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso','mvn_swim_velocity_mso_tot','mvn_swim_pdy','mvn_swim_temperature_mso','mvn_swim_pressure',$
               'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
               'wind_','sheath_','pileup_','wake_','lat_','lon_','sza_','Zmso','Ymso','Xmso'],1,2    
     
     store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
   

     options,'sheath_*',colors=4
     options,'pileup_*',colors=5
     options,'wake_*',colors=2
     store_data,'alt2',data=['wind__1','sheath__1','pileup__1','wake__1','wind__2','sheath__2','pileup__2','wake__2']
     tplot_options,'var_label',['orbnum_','lat_','lon_']
     
     
     zlim,'mvn_sep*_Energy',10.,100000,1
     ylim,'mvn_sep*_Energy',10.,100000,1
     ylim,'mvn_sta_c0_E',1.,100000,1
          ylim,'mvn_sta_c6_M',1.,100,1
     ylim,'mvn_swi*_en_eflux',1,30000
     if ~keyword_set(noplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swim_density','mvn_swim_velocity_mso','mvn_swim_pdy','mvn_B_1sec_MAVEN_MSO','alt2','sza_']
    
   endif

end