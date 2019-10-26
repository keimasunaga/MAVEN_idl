;+
; :Description:
;    Purpose of this routine is to draw contour lines of angles 
;    between the input vector and the normal vector of each bin in the Martian Mercator map [360x180]
;
; ${parameters}
;  vec_mso: a vector in the mso coordinate system
;  time: time of the event, must be a doble scalar (unix time)
;  
; ${keywords}
;  over: if set, overwrite the lines on the map plotted before this routne runs
;  levels_plus: array of counter levels of the parallel part of the angle
;  levels_minus: array of counter levels of the anti-parallel part of the angle
;  
; ${Related routines}
;
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified May 13, 2014
;-

pro contour_vec_angle_bmap,vec_mso,time,over=over,levels_plus=levels_plus,levels_minus=levels_minus,png=png
  
  
  env = init_env()
  v = env.SAVE_LOC
  
  vec_geo = spice_vector_rotate(vec_mso,time,'MAVEN_MSO','IAU_MARS')
  vec_geo_t = sqrt(vec_geo[0]^2 + vec_geo[1]^2 + vec_geo[2]^2)
  vec_geo_norm = vec_geo/vec_geo_t
  
  agl_arr = fltarr(360,180)
  for i=0,359 do begin
    for j=0,179 do begin
      agl_arr[i,j]=acos( vec_geo_norm[0]*cos((j-90.)*!DTOR)*cos(i*!DTOR)+vec_geo_norm[1]*cos((j-90.)*!DTOR)*sin(i*!DTOR)+vec_geo_norm[2]*sin((j-90.)*!DTOR) )
    endfor
  endfor
  
  if not keyword_set(levels_plus) then levels_plus = [10,40,70]
  if not keyword_set(levels_minus) then levels_minus = [110,140,170]
  
  contour,agl_arr*!RADEG,findgen(360),findgen(180)-90,levels=levels_plus,color=230,over=over
  contour,agl_arr*!RADEG,findgen(360),findgen(180)-90,levels=levels_minus,color=80,/over
  contour,agl_arr*!RADEG,findgen(360),findgen(180)-90,levels=[90],/over
  
  
   
end