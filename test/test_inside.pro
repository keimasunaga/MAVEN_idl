pro test_inside

v1=[-91.204135, -141.31620,-106.94145,-165.70037,-132.02594,-204.56753, -147.64479, -228.76815]
v2 =[365.54607,566.39518,509.89394,790.05492,357.74154,554.30245,501.00287,776.27865]

v1=[-141.31620, -147.64479,-91.204135, -106.94145,-165.70037,-132.02594,-204.56753, -228.76815]
v2 =[566.39518,501.00287,365.54607,509.89394,790.05492,357.74154,554.30245,776.27865]

v1_poly = v1[2:7]
v2_poly = v2[2:7]

v1_poly_G = avg(v1_poly)
v2_poly_G = avg(v2_poly)

v1_poly_s = v1_poly-v1_poly_G
v2_poly_s = v2_poly-v2_poly_G

plot,v1,v2,psym=6
plots,v1_poly_G,v2_poly_G,psym=1,color=230

agl_arr = []
for qq=0,5 do begin
  print,v1_poly_s[qq],v2_poly_s[qq]
  arrow,v1_poly_G,v2_poly_G,v1_poly_G+v1_poly_s[qq],v2_poly_G+v2_poly_s[qq],/data
  agl = atan(v2_poly_s[qq],v1_poly_s[qq])
  if v2_poly_s[qq] lt 0 then agl = agl + 2*!pi
;  if v1_poly_s[qq] lt 0 and v2_poly_s[qq] lt 0 then agl=agl+!pi
;  if v1_poly_s[qq] gt 0 and v2_poly_s[qq] lt 0 then agl=agl+!pi
  agl_arr = [agl_arr, agl]
  stop
endfor
sort_agl = sort(agl_arr)
obj = obj_new('IDLanROI',v1_poly[sort_agl],v2_poly[sort_agl])
points = [[v1[0],v1[1]],[v2[0],v2[1]]]
result = obj->containspoints(transpose(points))
obj_destroy,obj

print,result
stop

end