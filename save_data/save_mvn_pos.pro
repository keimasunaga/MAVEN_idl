;+
; :Description:
;  This routine saves MAVEN's orbit in the MSO or MSE plane.
;
; ${parameters}
;  orbit: orbit number
;
; ${keywords}
;
; ${Related routines}
;  plot_mvn_orbit.pro
;
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified Sep 15, 2015
;-

pro save_mvn_pos,orbit,frame=frame
    
    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    if not keyword_set(frame) then frame = 'MSO'
    
    plot_mvn_orbit,orbit=orbit,/seconds,frame=frame,output=output
    time = time_string(output.time)
    x = output.pos_x
    y = output.pos_y
    z = output.pos_z
    
    filename = SAVE_LOC + '/maven/ascii/maven_orbit/orbit_'+string(orbit,format='(i05)')+'_'+frame+'.dat'
    format = '(a19, 3d15.10)'
    close,1
    openw,1,filename
    for i=0,n_elements(time)-1 do begin
    printf,1,time[i],x[i],y[i],z[i],format=format
    endfor
    close,1  
 
end