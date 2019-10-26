pro ps2png_example


   
      ps_path = SAVE_LOC+'/maven/ps/vdf_reflect_rmv_ring/normal/oxy/'+string(orbit,format='(i05)')
      if file_test(ps_path) eq 0 then file_mkdir,ps_path
      popen,ps_path+'/vdf_reflect_'+date_s+'.eps',xsize=40,ysize=40,unit='cm',/enc
      
    ;;;;plot something here;;;
    
      pclose
      cd,current=current_dir
      cd,ps_path
      spawn,'convert vdf_reflect_'+date_s+'.eps vdf_reflect_'+date_s+'.png'
      spawn,'rm ' + 'vdf_reflect_'+date_s+'.eps'
      cd,current_dir
      
end