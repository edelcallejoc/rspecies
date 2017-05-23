

Función 1: grd_build() - listo (documentación lista)
Función 3: raster_breaks() - listo (documentación lista)
Función 4: get_species() - Prototipo (Falta hacer documentación)

Datos 1: Mex0 - lista (documentación lista)
Datos 2: mammals - lista (documentación lista)

Clase 1: BinMat - listo(Falta terminar documentación y dar ejemplos)
                  (Falta arreglar la validación)
Métodos clase 1: Accessors - listo (Falta terminar documentación)
Métodos clase 2: id_pts() - listo (documentación lista)

Clase 2: BinMatCount - listo(Falta terminar documentación y dar ejemplos)
                       (Falta arreglar la validación)
Métodos clase 2: Accessors - Listo (Falta terminar documentación)
Métodos clase 2: Setters - Listo (Falta terminar documentación)
Métodos plot: plot SPDF BinMat listo (Falta terminar documentación)
Método counts:  Listo (Falta revisar la documentación)
Método laplace: Listo (Falta revisar la documentación)

Clase 3: BinMatProb - listo (Falta terminar documentación y dar ejemplos)
                      (Falta arreglar la validación)
Métodos clase 3: Accessors - listo (Falta terminar documentación)
Métodos clase 3: Setters - listo (Falta terminar documentación)
Método probs:  Listo (Falta revisar la documentación)

Clase 4: BinMatEps - listo (Falta terminar documentación y dar ejemplos)
                     (Falta arreglar la validación)
Métodos clase 4: Accessors - listo (Falta terminar documentación)
Métodos clase 4: Setters - listo (Falta terminar documentación)
Método epsilon:  Listo (Falta revisar la documentación) Revisar resultados

Clase 4: BinMatScore - listo (Falta terminar documentación y dar ejemplos)
                     (Falta arreglar la validación)
Métodos clase 4: Accessors - Listo (Falta terminar documentación)
Métodos clase 4: Setters - Faltan (Falta terminar documentación)
Método score:  Listo (Falta revisar la documentación) Revisar resultados
Método predict: Falta revisarla y hacer una clase para esta función (Falta terminar documentación)

Clase 5: BinMatPred - listo listo (Falta terminar documentación y dar ejemplos)
                     (Falta arreglar la validación)
Métodos clase 5: Accessors - Faltan (Falta terminar documentación)
Métodos clase 5: Setters - Faltan (Falta terminar documentación)
Métodos clase 5: plot SPDF BinMatPred - listo (Falta terminar documentación)









##### getting data form species

Función lista (Falta limpiarla y hacer documentación)

Dar solo opción por especie

getSpecies  = qtype para especie por especie

a<-POST("http://species.conabio.gob.mx/niche3/niche/especie", body = list(qtype = "getSpecies", id = 27332, sfecha = "true"), encode = "json")

b<-content(a)

fromJSON(b$data[[1]]$json_geom)$coordinates

d<-POST("http://species.conabio.gob.mx/niche3/niche/especie", body = list(qtype = "getEntList", searchStr = "mam", nivel = "clasevalida", source = "1"), encode = "json")

