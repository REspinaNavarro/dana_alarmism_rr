## Application of the Refined Alarmism Dictionary to the Blind Validation Corpus

# 1. Packages installing if needed

#paquetes <- c('readxl','tidyverse', 'stringr')
#new_packages <- paquetes[!(paquetes %in% #installed.packages()[,"Package"])]
#if(length(new_packages)) install.packages(new_packages)

library(readxl)
library(tidyverse)

# 2. Importing data and headline cleaning

df_raw <-read_xlsx("data/blind_validation_corpus.xlsx")

# Text cleaning and normalizing

clean_text <- function(txt) {
  t <- tolower(txt)
  t <- iconv(t, to = "ASCII//TRANSLIT")
  return(t)
}

# 3. Dictionary refining   (from spanish words)

v1_tokens <- c("historic\\w*", "tragic\\w*", "inedit\\w*", "excepcional\\w*", "extraordin\\w*", "devast\\w*", "insolit\\w*", "catastrof\\w*", "impact\\w*", "azot\\w*", "apocalipt\\w*", "dramatic\\w*", "descomunal\\w*", "tragedia", "record", "apocalipsis", "diluvio", "locura", "desastre", "milagro", "ratonera")
v1_expres <- c("nunca visto", "jamas visto", "sin precedentes", "nunca antes", "visto antes", "revive el panico", "tormentas destructivas", "zona cero", "ha cambiado la historia", "brutal impacto", "ano horrible", "garaje de los horrores", "revolucion meteorologica", "vidas segadas", "fin del mundo", "dia negro")
v1_grupoA <- c("dana", "riada", "inundacion\\w*", "tormenta", "temporal", "victimas”, “dañ_”, “destrucción”, ”lluvias", "precipitacion\\w*", "litro_", "gota fria")
v1_grupoB <- c("peor", "mayor", "maxim\\w*", "extrem\\w*", "unico", "record", "mas grande", "histor\\w*", "siglo", "decadas", "europa") 

v2_tokens <- c("graves", "caos", "colapso", "inundaciones")
v2_expres <- c("sin suministro electrico", "sin agua potable", "grupo de violent\\w*", "demanda sin precedentes")
v2_grupoA <- c("colaps\\w*", "paraliz\\w*", "bloque\\w*", "incomunic\\w*", "aisl\\w*", "desbord\\w*", "satur\\w*", "interrump\\w*", "suspend\\w*", "cort\\w*", "cerr\\w*", "derrumb\\w*", "dud\\w*", "atrapad\\w*", "cancelad\\w*", "descarriad\\w*", "restring\\w*", "anegad\\w*","sumerg\\w*")
v2_grupoB <- c("trafico", "carretera\\w*", "tren\\w*", "metro\\w*", "aeropuerto\\w*", "vuelo\\w*", "transporte\\w*", "comunicac\\w*", "hospital\\w*", "colegio\\w*", "clase\\w*", "edificio\\w*", "servicio\\w*", "ciudad", "comedor\\w*", "atio\\w*", "conductor\\w*", "pueblo\\w*", "voluntar\\w*", "parking\\w*", "electricidad", "agua", "valencia", "puerto", "residencias", "centenares", "miles", "ave", "provincia", "archivos")

v3_tokens <- c("incapac\\w*", "descoordin\\w*", "desprote\\w*", "aband\\w*", "insuficient\\w*", "amenaz\\w*", "desaparecid\\w*", "incomunic\\w*", "atrapad\\w*", "resignad\\w*", "responsabl\\w*", "solos", "retraso", "demora", "ausencia", "desinformacion", "bulos", "mazon",  "explicac\\w*")
v3_expres <- c("respuesta tardia", "nadie acudio", "el pueblo para el pueblo", "filas de voluntarios", "vecinos indignados", "llam\\w a(?: \\S+){1,6} para despedirse", "abandonados por el estado", "cumulo de errores", "rescates in extremis", "centros de recogida", "acude a valencia", "se activo horas antes", "aviso dias antes", "cargas policiales", "por el cuello", "miles de voluntarios", "catastrofe\\w* anunciada\\w*", "alertas sobrepasadas", "continuan desaparecidos")
v3_grupoA <- c("\\bno\\b", "sin", "ausencia", "imped\\w*", "tard\\w*", "retras\\w*", "insuficient\\w*", "repulsa","por que","con lentitud")
v3_grupoB <- c("inform\\w*", "actualiz\\w*", "respond\\w*", "proteg\\w*", "auxil\\w*", "evacu\\w*", "rescat\\w*", "interven\\w*", "asist\\w*", "limp\\w*", "enviad\\w*", "ayuda\\w*", "supera\\w*", "respuesta", "avisad\\w*", "salir", "gestion", "emergencia\\w*", "protocolos","actuar\\w*")

v4_tokens <- c("agrav\\w*", "empeorar\\w*", "amenaz\\w*", "persist\\w*", "continu\\w*", "seguir\\w*", "provoc\\w*", "responsabilidad\\w*", "traumatiz\\w*", "afectad\\w*", "fallas")
v4_expres <- c("cuando el agua se ha ido", "no sabemos si podremos volver", "lo peor esta por llegar", "se cobra la primera victima", "catastrofe medioambiental", "arroz intoxicado", "de momento", "esta en peligro", "alerta roja", "lesiones en los cimientos", "nunca recuperaremos la normalidad")
v4_grupoA <- c("seguir\\w*", "continuar\\w*", "nuev\\w*", "proxim\\w*", "podr\\w*", "se espera", "apunta a", "esta por venir", "no ha acabado", "no ha terminado", "podria empeorar", "riesgo creciente", "alerta\\w*", "vuelve a", "dud\\w*", "alert\\w*", "acech\\w*", "dana mas", "siguen", "puede volver", "se acerca a", "riesgo\\w*", "se teme", "sube\\w*") 
v4_grupoB <- c("dana\\w*", "lluvias", "tormentas", "alertas", "riesgo\\w*", "fenomenos", "problema\\w*", "inundaciones", "barro", "derrumb\\w*", "contamina\\w*", "enfermedades", "plaga\\w*", "emergenc\\w*", "aemet", "otras zonas", "muert\\w*", "desaparecid\\w*", "victim\\w*", "salud", "demanda", "desgracia", "lodo", "infecciones") 

# 4. Detecting rhetorical resources in headlines

audit_with_evidence <- function(txt, tokens, expres, gA, gB) {
  if(is.na(txt) | txt == "") return(list(valor=0, evidencia="Absence"))
  t_clean <- clean_text(txt)
  
  # Sorting by lenght to avoid some false posive 
  tokens <- tokens[order(-nchar(tokens))]
  expres <- expres[order(-nchar(expres))]
  gA     <- gA[order(-nchar(gA))]
  gB     <- gB[order(-nchar(gB))]
  
  # Evaluation for fixed expressions
  
  for(e in expres) {
    if(str_detect(t_clean, e)) return(list(valor=1, evidencia=paste("Expresion:", e)))
  }
  
  # Evaluation for tokens 
  
  for(tok in tokens) {
    if(str_detect(t_clean, paste0("\\b", tok, "\\b"))) return(list(valor=1, evidencia=paste("Token:", tok)))
  }
  
  # Evaluation for patterns
  match_A <- NA; match_B <- NA
  for(a in gA) { if(str_detect(t_clean, paste0("\\b", a, "\\b"))) { match_A <- a; break } }
  for(b in gB) { if(str_detect(t_clean, paste0("\\b", b, "\\b"))) { match_B <- b; break } }
  
  if(!is.na(match_A) & !is.na(match_B)) {
    return(list(valor=1, evidencia=paste("Patron:", match_A, "+", match_B)))
  }
  
  return(list(valor=0, evidencia="Absence"))
}

# 5. Corpus processing

res_v1 <- lapply(df_raw$Titular, function(x) audit_with_evidence(x, v1_tokens, v1_expres, v1_grupoA, v1_grupoB))
res_v2 <- lapply(df_raw$Titular, function(x) audit_with_evidence(x, v2_tokens, v2_expres, v2_grupoA, v2_grupoB))
res_v3 <- lapply(df_raw$Titular, function(x) audit_with_evidence(x, v3_tokens, v3_expres, v3_grupoA, v3_grupoB))
res_v4 <- lapply(df_raw$Titular, function(x) audit_with_evidence(x, v4_tokens, v4_expres, v4_grupoA, v4_grupoB))

df_final <- df_raw %>%
  mutate(
    V1 = map_int(res_v1, ~.x$valor), Evidencia_V1 = map_chr(res_v1, ~.x$evidencia),
    V2 = map_int(res_v2, ~.x$valor), Evidencia_V2 = map_chr(res_v2, ~.x$evidencia),
    V3 = map_int(res_v3, ~.x$valor), Evidencia_V3 = map_chr(res_v3, ~.x$evidencia),
    V4 = map_int(res_v4, ~.x$valor), Evidencia_V4 = map_chr(res_v4, ~.x$evidencia)
  )


# 6. Prevalence analysis

# Resource prevalence in training sample

prevalences <- tibble(
  Resource = c("V1", "V2", "V3", "V4"),
  Prevalence = round(
    colMeans(df_final[c("V1", "V2", "V3", "V4")]) * 100,
    2
  )
)

print(prevalences)

# 7. File exporting
write.table(df_final, file = 'dict_validation_blind_data.csv', sep = ";", row.names = FALSE, col.names = TRUE)
print('Script processed succesfully')

