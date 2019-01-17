param.combine = function(config, type, t ,f ,ext){

  t.f = paste0("nwm.t", sprintf("%02d", t), "z")

  c.f = ifelse(type == "forcing", sub('.*forcing_', '', config), config)

  type.f = if(type %in% c("terrain", "channel")){ paste0(type, "_rt", ext) } else { paste0(type, ext) }

  f.f = paste0("f", sprintf("%03d", f), ".conus.nc")

  vec = expand.grid(t.f, c.f)
  vec.p = do.call(paste, c(vec, sep="."))

  vec2 = expand.grid(vec.p, type.f)
  vec2.p = do.call(paste, c(vec2, sep="."))

  vec3 = expand.grid(vec2.p, f.f)
  vec3.p = do.call(paste, c(vec3, sep="."))

  return(vec3.p)
}
