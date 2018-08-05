
defineLayers = function(param, layer){

  param = toupper(param)

if(!(param %in% c('SOIL_T', 'SOIL_M', 'SNLIQ'))){
  layer.p = NULL
  l = NULL
}else{

if(param %in% c('SOIL_T', 'SOIL_M')){

  if(is.null(layer)){layer = 1
  message("No layer provided for ", param, " defaulting to layer 1")
  }

  if(!(layer %in% c(1,2,3))){
    layer = 1
    message("Only 3 layers available for ", param, " defaulting to layer 1")
  }
}

if(param %in% "SNLIQ"){
  if(is.null(layer)){
    layer = 1
    message("No layer provided for ", param, " defaulting to layer 1")
  }

  if(!(layer %in% c(1,2))){
    layer = 1
    message("Only 2 layers available for ", param, " defaulting to layer 1")
  }
}

layer.p = paste0("[",layer, ":1:", layer, "]")
l = paste0("_",layer)

}

return(list(layer.p = layer.p, layer = l))

}

