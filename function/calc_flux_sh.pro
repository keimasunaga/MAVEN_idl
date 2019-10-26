pro calc_flux_sh,dat,$
                 Odens=Odens,Ovel_mso=Ovel_mso,Ovel_mse=Ovel_mse,Ovel_sh=Ovel_sh,$
                 erange=erange,mass_range=mass_range,mass_assmpt=mass_assmpt,rot=rot,q=q,vsh_dir=vsh_dir,vec_norm_sh=vec_norm_sh
   
   time = (dat.time + dat.end_time)/2d
   Odens = n_4d(dat,energy=erange,mass=mass_range,m_int=mass_assmpt)
   Ovel = v_4d(dat,energy=erange,mass=mass_range,m_int=mass_assmpt)
   if total(Ovel^2)^.5 ne 0. then begin
     vunit = Ovel/total(Ovel^2)^.5
     if ~keyword_set(q) then q = spice_body_att('MAVEN_STATIC', 'MAVEN_MSO', time, /quaternion, verbose=verbose)
     Ovel_mso = total(Ovel^2)^.5*spice_vector_rotate(vunit,time,'MAVEN_STATIC','MAVEN_MSO',check_objects=check_objects,verbose=verbose,qrot=q)
     Ovel_mse = mso2mse(Ovel_mso[0],Ovel_mso[1],Ovel_mso[2],rot)
     if keyword_set(vsh_dir) then begin
       if vsh_dir eq 1 then Ovel_sh = inpro(Ovel_mse,vec_norm_sh)
       if vsh_dir eq -1 then Ovel_sh = -inpro(Ovel_mse,vec_norm_sh)
     endif else begin
       Ovel_sh = inpro(Ovel_mse,vec_norm_sh)
     endelse
   endif else begin
     Ovel_mso = [0.,0.,0.]
     Ovel_mse = [0.,0.,0.]
     Ovel_sh = 0.
   endelse
   
   
end