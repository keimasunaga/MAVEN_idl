pro testtest


  x=indgen(10) & y=indgen(10) & z = indgen(10)
  imxy = fltarr(10,10) & imyz = fltarr(10,10) & imzx = fltarr(10,10)
  plotxyz, x, y, imxy, mult='2,2'
  plotxyz, y, z, imyz, /add
  
  p1 = convert_coord([0, 0], /data, /to_normal)
  p2 = convert_coord([9, 9], /data, /to_normal)
  plotxyz, z, x, imzx, /add
  
  p3 = convert_coord([0, 0], /data, /to_normal)
  p4 = convert_coord([9, 9], /data, /to_normal)
  plot, x, y, /noerase, pos=[p1[0], p3[1], p2[0], p4[1]]
;  makepng, 'multiplot_case2'
stop

  !p.multi=[2,2]
  x=indgen(10) & y=indgen(10) & z = indgen(10)
  imxy = fltarr(10,10) & imyz = fltarr(10,10) & imzx = fltarr(10,10)
  plotxyz,x,y,imxy, mult='2,2'
  plotxyz,y,z,imyz, /add
  plotxyz,z,x,imzx,/add
  plot,x,y

stop
end