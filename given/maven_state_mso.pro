PRO maven_state_mso, mvn_pos_mso, mvn_vel_mso, time_arr, $
                     unix_time = unix_time, trange = trange, dt = dt
    
    ;; Find MAVEN s/c postion (km) and velocity (km/s) in MSO coordinates, using SPICE kernels
    ;; Input: keywords
       ; unix_time: scalar or 1D array
       ; trange: time range 
       ; dt: time resolutsion; if not set, dt = 4s
       ;; Notes: must set either unix_time or trange
    ;; Output: 
       ; mvn_pos_mso Nx4 array [x, y, z, r]
       ; mvn_vel_mso Nx4 array [vx, vy, vz, v]
       ; time_arr [N]: unix time
    
    if keyword_set(dt) eq 0 then dt = 4.D         
    if keyword_set(unix_time) eq 0 then begin   
    ;if unix_time not set, creat unix_time array 
            t_start = time_double(trange[0]) 
            t_end = time_double(trange[1])   
            Nt = floor((t_end - t_start)/dt) + 1
            unix_time = indgen(Nt,/long)*dt+t_start  
    endif
    
    time_arr = unix_time
    et_time = time_ephemeris(unix_time)
    nele = n_elements(et_time)
    
    ;MAVEN position and velocity in MSO frame:
    ;General info:  
    frame    = 'MAVEN_MSO'
    abcorr   = 'LT+S'
    observer = 'Mars'
    target = 'MAVEN' 
   
    
    ;Using SPICE kernels    
    cspice_spkezr, target, et_time, frame, abcorr, observer, stateezr, ltime
    mvn_pos_mso = dblarr(nele,4)  ;x,y,z,r (4 rows)
    mvn_vel_mso = dblarr(nele,4)

    mvn_pos_mso[*,0] = stateezr[0,*]
    mvn_pos_mso[*,1] = stateezr[1,*]
    mvn_pos_mso[*,2] = stateezr[2,*]
    mvn_pos_mso[*,3] = sqrt(mvn_pos_mso[*,0]^2 + mvn_pos_mso[*,1]^2 + mvn_pos_mso[*,2]^2)  ;r, km
    
    mvn_vel_mso[*,0] = stateezr[3,*]  ;velocities in km/s   
    mvn_vel_mso[*,1] = stateezr[4,*]  
    mvn_vel_mso[*,2] = stateezr[5,*]  
    mvn_vel_mso[*,3] = sqrt(mvn_vel_mso[*,0]^2 + mvn_vel_mso[*,1]^2 + mvn_vel_mso[*,2]^2)  ;speed, km/s
  
END