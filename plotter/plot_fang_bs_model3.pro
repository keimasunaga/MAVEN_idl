pro plot_fang_bs_model3

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    filename = SAVE_LOC + '/maven/ascii/model/fang/BS_dayside.dat'
    nline = file_lines(filename)
    sline = ''
    close,1
    openr,1,filename
    bs_pos = fltarr(3,nline-9)
      
    for i=0,8 do readf,1,sline
    readf,1,bs_pos   
    close,1
    !p.multi=[0,3,1]
    plot,bs_pos[0,*],bs_pos[1,*],psym=1,/iso;,xrange=[1.0,1.8],yrange=[-0.4,0.4]
    oplot,1.5*bs_pos[0,*],1.5*bs_pos[1,*],psym=1,color=230
    plot,bs_pos[0,*],bs_pos[2,*],psym=1,/iso;,xrange=[1.0,1.8],yrange=[-0.4,0.4]
    plot,bs_pos[1,*],bs_pos[2,*],psym=1,/iso;,xrange=[-1,1],yrange=[-1,1]
    !p.multi=[0,1,1]
    
    
    nbin = 300
    xpos = replicate(1.6,nbin+1)
    ypos = findgen(nbin+1) * 0.02d - fix(nbin/2)*0.02d
    zpos = findgen(nbin+1) * 0.02d - fix(nbin/2)*0.02d
    
    oplot,xpos,ypos,psym=1,color=230
        

    ;;calculate incident ion velocity
    ;;note: incident angle can be calculated by solving
    ;; Vx = Vperp*(1+cos(theta)), Vz = Vperp*sin(theta)
    ;;tan(Vz/Vx) = tan(45 deg) = 1.
    ;; For incident angle = 30deg, phase = 60 deg
    ;; For incident angle = 45deg, phase = 90 deg

    Vsw = 400.
    agl_parker = 56.*!DTOR
    Vperp_bulk = Vsw * cos(90.*!DTOR-agl_parker)
    phase = 60.*!DTOR   ;;read note above
    Vperp_oxy = ( (Vperp_bulk * (1.+cos(phase)))^2 + (Vperp_bulk * sin(phase))^2 )^.5
    Einc = 0.5*16.*1.67e-27*(Vperp_oxy*1e3)^2/1.6e-19
    vvec_inc = [ -Vperp_bulk*(1.+cos(phase))*sin(agl_parker) , -Vperp_bulk*(1.+cos(phase))*cos(agl_parker), Vperp_bulk*sin(phase) ]
    vvec_inc_tot = total(vvec_inc^2)^.5
    vvec_inc_unit = vvec_inc/total(vvec_inc^2)^.5
    ;vvec_arr = fltarr(3,n_elements(xpos))
    vvec_arr = transpose([ [replicate(vvec_inc_unit[0],n_elements(xpos))], [replicate(vvec_inc_unit[1],n_elements(ypos))],[replicate(vvec_inc_unit[2],n_elements(zpos))] ])
    E0 = replicate(Einc,n_elements(xpos))
    
    
    sav = [ [xpos],[ypos],[zpos],[E0],[transpose(vvec_arr[0,*])],[transpose(vvec_arr[1,*])],[transpose(vvec_arr[2,*])] ]
stop   
    filename = SAVE_LOC + '/maven/ascii/model/fang/incident_ions_plane/incident_ions_plane_30deg'+'.dat'
    format = '7(f17.10)'
    write_ascii,sav,filename,format
    
    
    stop



end