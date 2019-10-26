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
     
;     ;get_data,tvar_arr[j]+'_'+strcompress(string(num1),/remove),data1
;     ;get_data,tvar_arr[j]+'_'+strcompress(string(num2),/remove),data2
;     store_data,tvar_arr[j],data=[ tvar_arr[j]+'_'+strcompress(string(num1),/remove), tvar_arr[j]+'_'+strcompress(string(num2),/remove) ]
   
     get_data,tvar_arr[j]+'_'+strcompress(string(num1),/remove),data=data1
     get_data,tvar_arr[j]+'_'+strcompress(string(num2),/remove),data=data2
     if (size(data1.y))[0] eq 1 then store_data,tvar_arr,data={x:[data1.x,data2.x], y:[data1.y,data2.y]} $
     else store_data,tvar_arr,data={x:[data1.x,data2.x], y:[[data1.y],[data2.y]]}
   endfor
   
end

pro get_lims,tva_arr,num

 ; for k=0,n_elements(tvar_arr)-1 get_data,tvar_arr[k],data=data
 ; ylim,tvar_arr[k],

end


; main routine
pro tplot_saved_mvn_pfp_daily,date=date,orbit=orbit,noplot=noplot

   del_data,'*'
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   
   
   
   if keyword_set(date) then begin
    
     filename = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+date+'.tplot'
     tplot_restore,filename=filename 
     timespan,date,1,/day
     
     ;;merge vel_xyz and vel_total
     tplot_vec_tot,'mvn_swica_velocity_mso'
     store_data,'mvn_swica_velocity_mso_all',data=['mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot']
     tplot_vec_tot,'mvn_swics_velocity_mso'
     store_data,'mvn_swics_velocity_mso_all',data=['mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot']
     
     ;;merge mag_xyz and mag_total
     tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
     store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
     
     store_data,'alt2',data= ['wind','sheath','pileup','wake']
     
     tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']
     
     
     if ~keyword_set(noplot) then begin
       tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
              'mvn_swe_et_spec_svy',$
              'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
              'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy',$
              'mvn_B_1sec_MAVEN_MSO_all',$
              'alt2','sza']
       ;tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
     endif
   endif




   if keyword_set(orbit) then begin
    
   if n_elements(orbit) eq 1 then begin
     tspan = mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
     tspan_strng = time_string(tspan)
     date_s = strmid(tspan_strng[0],0,4)+strmid(tspan_strng[0],5,2)+strmid(tspan_strng[0],8,2)
     date_e = strmid(tspan_strng[1],0,4)+strmid(tspan_strng[1],5,2)+strmid(tspan_strng[1],8,2)
     if date_s eq date_e then begin
        filename = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+date_s+'.tplot'
        tplot_restore,filename=filename
        timespan,tspan
        ;if tplot_exist('mvn_swim_velocity_mso') eq 1 then begin
        tplot_vec_tot,'mvn_swica_velocity_mso'
        store_data,'mvn_swica_velocity_mso_all',data=['mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot']
        ;endif 
   
        ;if tplot_exist('mvn_B_1sec_MAVEN_MSO') eq 1 then begin
        tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
        store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
        ;endif
   
        store_data,'alt2',data= ['wind','sheath','pileup','wake']
;        options,'sheath_',colors=4
;        options,'pileup_',colors=5
;        options,'wake_',colors=2
     
        tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']

        if ~keyword_set(noplot) then begin
          tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
                 'mvn_swe_et_spec_svy',$
                 'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                 'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy',$
                 'mvn_B_1sec_MAVEN_MSO_all',$
                 'alt2','sza']
          tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
        endif
     endif else begin
      
       filename0 = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+date_s+'.tplot'
       filename1 = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+date_e+'.tplot'
       
       tplot_restore,filename=filename0
       tspan = mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
       timespan,tspan
       
       tplot_vec_tot,'mvn_swim_velocity_mso'
       tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'

       rename_temp,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
                    'mvn_swe_et_spec_svy',$
                    'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                    'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
                    'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
                    'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],num=1

       tplot_restore,filename=filename1

      rename_temp,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
                    'mvn_swe_et_spec_svy',$
                    'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                    'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
                    'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
                    'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],num=2

       cmb_tvar,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
                 'mvn_swe_et_spec_svy',$
                 'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                 'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
         'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
         'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],1,2

       store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']


       options,'sheath*',colors=4
       options,'pileup*',colors=5
       options,'wake*',colors=2
       store_data,'alt2',data=['wind_1','sheath_1','pileup_1','wake_1','wind_2','sheath_2','pileup_2','wake_2']
       tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']


       zlim,'mvn_sep*_Energy',10.,100000,1
       ylim,'mvn_sep*_Energy',10.,100000,1
       ylim,'mvn_sta_c0_E',1.,100000,1
       ylim,'mvn_sta_c0_H_E',1.,100000,1
       ylim,'mvn_sta_c6_M',1.,100,1
       ylim,'mvn_swi*_en_eflux',1,30000
       if ~keyword_set(noplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
                 'mvn_swe_et_spec_svy',$
                 'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                 'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy',$
                 'mvn_B_1sec_MAVEN_MSO_all',$
                 'alt2','sza']
            
     endelse
     
     
   endif

   


   if n_elements(orbit) eq 2 then begin
     tspan = mvn_orbit_num(orbnum=[orbit[0]-0.5,orbit[1]+0.5])
     timespan,tspan
     tspan_strng = time_string(tspan)
     date_s = strmid(tspan_strng[0],0,4)+strmid(tspan_strng[0],5,2)+strmid(tspan_strng[0],8,2)
     date_e = strmid(tspan_strng[1],0,4)+strmid(tspan_strng[1],5,2)+strmid(tspan_strng[1],8,2)
     filename0 = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+date_s+'.tplot'
     filename1 = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+date_e+'.tplot'
     print,filename0
     tplot_restore,filename=filename0
     
     tplot_vec_tot,'mvn_swim_velocity_mso'
     tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
         
     rename_temp,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                  'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
                  'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
                  'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],num=1
  
     tplot_restore,filename=filename1
     
     rename_temp,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                  'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
                  'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
                  'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],num=2
      
     cmb_tvar,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
               'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
               'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
               'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],1,2    
     
     store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
   

     options,'sheath_*',colors=4
     options,'pileup_*',colors=5
     options,'wake_*',colors=2
     store_data,'alt2',data=['wind_1','sheath_1','pileup_1','wake_1','wind_2','sheath_2','pileup_2','wake_2']
     tplot_options,'var_label',['orbnum_','lat_','lon_']
     
     
     zlim,'mvn_sep*_Energy',10.,100000,1
     ylim,'mvn_sep*_Energy',10.,100000,1
     ylim,'mvn_sta_c0_E',1.,100000,1
      ylim,'mvn_sta_c0_H_E',1.,100000,1
          ylim,'mvn_sta_c6_M',1.,100,1
     ylim,'mvn_swi*_en_eflux',1,30000
     if ~keyword_set(noplot) then begin
     tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
            'mvn_swe_et_spec_svy',$
            'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
            'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_pdy',$
            'mvn_B_1sec_MAVEN_MSO','alt2','sza']
     endif
     
   endif
   endif
end