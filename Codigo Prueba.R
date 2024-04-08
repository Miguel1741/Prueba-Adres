######################### SECCIÓN DE LIBRERIAS A USAR ##########################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(RSQLite, dplyr, lubridate, plotly, stratification, FactoMineR, 
               factoextra, stats, openxlsx)


########### SECCIÓN DE CARGUE DE LA INFORMACIÓN Y CREACIÓN DE TABLAS ###########

usuario <- getwd() # Obtención del directorio

# se procede a realizar la conexión a las bases de sqlite
con <- dbConnect(drv = RSQLite::SQLite(), 
                 dbname= paste0(usuario, '/Prueba Adres/SQLite/Prueba.db'))
dbListTables(con)

# Se hacen algunas queries

dbGetQuery(con, "SELECT COUNT(*) AS Cantidad FROM Municipios;")
# Hay 1118 registros en la base de Municipios

dbGetQuery(con, "SELECT COUNT(*) AS Cantidad FROM Prestadores;")
# Hay 60946 registros en la base de prestadores

# Se hace verificación del estado de las tablas
str(dbGetQuery(con, "SELECT * FROM Municipios;"))

str(dbGetQuery(con, "SELECT * FROM Prestadores;"))
# En prestadores hay varias variables vacias y otras que tienen valores repetidos
# con base a lo anterior, se omitirá el uso de las que están vacias, las que 
# tienen un valor constante y las que se consideren poco relevantes.


# Se procede a crear tabla con solo los departamento y región para pegar 
# la variable region a la tabla prestadores
dbExecute(con, 'CREATE TABLE municipios_region AS
          SELECT DISTINCT * FROM (
          SELECT Departamento, Region FROM Municipios);')


dbGetQuery(con, "SELECT * FROM municipios_region;") # Quedó correcto

# Se hace verificacipon preliminar de que al unir las bases no cambien la cantidad
# de registros
dbGetQuery(con, 'SELECT COUNT(*) AS Cantidad FROM Prestadores AS A 
            LEFT JOIN municipios_region AS B ON A.depa_nombre = B.Departamento;') 
# Se observa que haciendo la agregación de la base muncipios_region a la de prestadores
# se mantiene la misma cantidad que tiene prestadores por tanto funciona bien 
# el pegado pensado.

# Se procede a crear tabla con los datos unidos. 
dbExecute(con, 'CREATE TABLE datos AS SELECT A.depa_nombre, A.muni_nombre,
                    A.nits_nit, A.razon_social, A.clpr_codigo, A.clpr_nombre, 
                    A.fecha_radicacion, A.fecha_vencimiento, A.clase_persona,
                    A.naju_codigo, A.naju_nombre, B.Region FROM 
                    Prestadores AS A LEFT JOIN municipios_region AS 
                    B ON A.depa_nombre = B.Departamento;')

# Se observa los primeros registros pegados y el estado de la tabla
dbGetQuery(con, "SELECT * FROM datos LIMIT 5;")
str(dbGetQuery(con, "SELECT * FROM datos;"))
# se obsercva que quedo bien la selección de las variables y el pegado


# Se crea la tabla/vista con la que se hará los análisis
dbExecute(con, 'CREATE VIEW datos_trabajar AS SELECT depa_nombre,  
          muni_nombre, clpr_nombre, fecha_radicacion, fecha_vencimiento, 
          clase_persona, naju_nombre,Region FROM datos;')


dbListTables(con) # Se observa que se creó satisfactoriamente


############### SECCIÓN DE ANÁLISIS DE LA INFORMACIÓN ##########################

# Se procede a realizar el análisis descriptivo directamente con lenguaje R

datos_trabajar <- dbGetQuery(con, "SELECT * FROM datos_trabajar;")

head(datos_trabajar)
summary(datos_trabajar)
# Con lo anterior se observa que se debe convertir a tipo Date las variables de fecha.


# Se procede a convertir a Date los campos fechas
datos_trabajar <- datos_trabajar %>% 
  mutate(fecha_radicacion = ymd(fecha_radicacion), 
         fecha_vencimiento = ymd(fecha_vencimiento))

# Se obtiene además una variable que obtiene la diferencia de días entre la 
# fecha de vencimiento y la fecha de radicación
datos_trabajar <- datos_trabajar %>%
  mutate(diferencia_dias = as.numeric(gsub('of ', '', 
                                           fecha_vencimiento - fecha_radicacion)))

# Se verifica lo anteriormente implementado
head(datos_trabajar)
summary(datos_trabajar)
datos_trabajar$indice <- 1:dim(datos_trabajar)[1] # se crea un indice único
# Se obtuvo los resultados esperados


for(m in 1:dim(datos_trabajar)[2]){
  
  print(names(datos_trabajar[m]))
  print(table(is.na(datos_trabajar[m])))
  
}
# Con lo anterior se observa que algunos registros no tienen la región y se 
# procede por tanto idenficar los departamentos para ajustarlos
datos_trabajar %>% filter(is.na(Region) == TRUE) %>% select(depa_nombre) %>% table()

datos_trabajar$muni_nombre[is.na(datos_trabajar$Region)] <- datos_trabajar$depa_nombre[is.na(datos_trabajar$Region)]

# Se ajusta los nombres de departamentos
datos_trabajar <- datos_trabajar %>% 
  mutate(depa_nombre = case_when(depa_nombre == 'Barranquilla' ~ 'Atlántico',
                                 depa_nombre == 'Bogotá D.C' ~ 'Bogotá. D.C.',
                                 depa_nombre == 'Buenaventura' ~ 'Valle Del Cauca',
                                 depa_nombre == 'Cali' ~ 'Valle Del Cauca',
                                 depa_nombre == 'Cartagena' ~ 'Bolívar',
                                 depa_nombre == 'Santa Marta' ~ 'Magdalena', 
                                 TRUE ~ depa_nombre))

# Se ajusta los nombres de las regiones
datos_trabajar <- datos_trabajar %>% 
  mutate(Region = case_when(depa_nombre == 'Atlántico' ~ 'Región Caribe',
                                 depa_nombre == 'Bogotá. D.C.' ~ 'Región Centro Oriente',
                                 depa_nombre == 'Valle Del Cauca' ~ 'Región Pacífico',
                                 depa_nombre == 'Valle Del Cauca' ~ 'Región Pacífico',
                                 depa_nombre == 'Bolívar' ~ 'Región Caribe',
                                 depa_nombre == 'Magdalena' ~ 'Región Caribe', 
                                 TRUE ~ Region))


for(m in 1:dim(datos_trabajar)[2]){
  
  print(names(datos_trabajar[m]))
  print(table(is.na(datos_trabajar[m])))
  
} # Quedó ajustado al resultado esperado


# Se crea una estratificación para poder dividir estadísticamente las diferencia 
# de días en 5 grupos distintos 
estratos <- stratification::strata.LH(x = datos_trabajar$diferencia_dias, CV = 0.05, Ls = 5, takeall = FALSE)
cortes <- c(min(datos_trabajar$diferencia_dias), estratos$bh, max(datos_trabajar$diferencia_dias))
nombres_cortes <- c(paste0('<= ', cortes[2]))

for (k in 2:(length(cortes) - 2)){
  
  nombres_cortes[k] <- c(paste0(cortes[k], ' - ', cortes[k + 1]))
  
}
nombres_cortes[length(cortes) - 1] <- c(paste0('> ', cortes[length(cortes) - 1]))

datos_trabajar$nivel_dias = cut(datos_trabajar$diferencia_dias, breaks = cortes,
                                include.lowest = TRUE, right = FALSE, labels = nombres_cortes)



############# SECCIÓN DE GRAFICACIÓN DE LAS VARIABLES DE INTERÉS ###############

fig <- plot_ly()
fig <- fig %>% add_pie(data = count(datos_trabajar, depa_nombre), 
                       labels = ~depa_nombre, values = ~n, 
                       name = 'Departamentos', domain = list(row = 0, col = 0))
fig


fig <- plot_ly()
fig <- fig %>% add_pie(data = count(datos_trabajar, clpr_nombre), 
                       labels = ~clpr_nombre, values = ~n, 
                       name = 'Clpr_nombre', domain = list(row = 0, col = 1))
fig

fig <- plot_ly()
fig <- fig %>% add_pie(data = count(datos_trabajar, clase_persona), 
                       labels = ~clase_persona, values = ~n, 
                       name = 'Clase_persona', domain = list(row = 1, col = 0))
fig

fig <- plot_ly()
fig <- fig %>% add_pie(data = count(datos_trabajar, naju_nombre), 
                       labels = ~naju_nombre, values = ~n, 
                       name = 'Naju_nombre', domain = list(row = 1, col = 1))
fig

fig <- plot_ly()
fig <- fig %>% add_pie(data = count(datos_trabajar, Region), 
                       labels = ~Region, values = ~n, 
                       name = 'Region', domain = list(row = 2, col = 0))
fig



fig <- plot_ly()
fig <- fig %>% add_trace(x = datos_trabajar$diferencia_dias, type = 'histogram',
                         histonorm = 'probability')
fig


fig <- plot_ly()
fig <- fig %>% add_trace(data = count(datos_trabajar, nivel_dias), 
                         x = ~nivel_dias, y = ~n, type = 'bar', 
                         color = I('darkblue'))
fig



# Se crea grafico combinado (se realiza el gráfico que permite relacionar todas 
# las variables de una forma amigable con el usuario)

u1 <- datos_trabajar %>% 
  group_by(clase_persona) %>%
  dplyr::summarise(Promedio_dias = mean(diferencia_dias))


u2 <- datos_trabajar %>% 
  group_by(clase_persona, naju_nombre) %>%
  dplyr::summarise(Promedio_dias = mean(diferencia_dias))

u3 <- datos_trabajar %>% 
  group_by(clase_persona, naju_nombre, clpr_nombre) %>%
  dplyr::summarise(Promedio_dias = mean(diferencia_dias))


u4 <- datos_trabajar %>% 
  group_by(clase_persona, naju_nombre, clpr_nombre, Region) %>%
  dplyr::summarise(Promedio_dias = mean(diferencia_dias))


u5 <- datos_trabajar %>% 
  group_by(clase_persona, naju_nombre, clpr_nombre, Region, 
           depa_nombre) %>%
  dplyr::summarise(Promedio_dias = mean(diferencia_dias))


fig <- plot_ly(
  
  labels = c(u1$clase_persona, paste0(u2$clase_persona, '<br>', u2$naju_nombre), 
             paste0(u3$clase_persona, '<br>', u3$naju_nombre, '<br>', 
                    u3$clpr_nombre), 
             paste0(u4$clase_persona, '<br>', u4$naju_nombre, '<br>', 
                    u4$clpr_nombre, '<br>', u4$Region), 
             paste0(u5$clase_persona, '<br>', u5$naju_nombre, '<br>', 
                    u5$clpr_nombre, '<br>', u5$Region,
                    '<br>', u5$depa_nombre)),
  parents = c(rep("", dim(u1)[1]), u2$clase_persona,  
              paste0(u3$clase_persona, '<br>', u3$naju_nombre), 
              paste0(u4$clase_persona, '<br>', u4$naju_nombre,
                     '<br>', u4$clpr_nombre), 
              paste0(u5$clase_persona, '<br>', u5$naju_nombre,
                     '<br>', u5$clpr_nombre, '<br>', u5$Region)
              ), 
  values = round(c(u1$Promedio_dias, u2$Promedio_dias, u3$Promedio_dias, 
             u4$Promedio_dias, u5$Promedio_dias), 0),
  type = 'sunburst'
  
)

fig

######## SECCIÓN DE ANÁLISIS MULTIVARIADO DE LAS VARIABLES DE INTERÉS ##########


# se procede a realizar un análisis de correspondencias multiple con los datos.

# se seleccionan las variables para el análisis
datos_multivariado_1 <- datos_trabajar %>% dplyr::select(clpr_nombre, 
                                                         clase_persona, naju_nombre,
                                                         Region, nivel_dias)


# se ajusta el análisis de correspondencia multiple
res.mca <- MCA(datos_multivariado_1, graph = FALSE)

eig.val <- get_eigenvalue(res.mca)

var <- get_mca_var(res.mca)

# Gráfico de la relación de las variables por las dos primeras componentes
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, 
             ggtheme = theme_minimal())

# Gráfico de la relación de los diferenes niveles de cada variable
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())

# Gráfico de la relación de los diferenes niveles de cada variable con su 
# contribución representativa
fviz_mca_var(res.mca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE, # Avoid text overlapping
             ggtheme = theme_minimal())

# Obtención de las coordenadas de los diferentes niveles de las variables
# para la segmentación de la información
salida_multivariado <- as.data.frame(var$coord)
salida_multivariado$variable <- rownames(salida_multivariado)


# Se exporta las coordenadas con los cuales se hacen los análisis de 
# segmentación por parte del experto.
OUT <- createWorkbook() 
addWorksheet(OUT, "salida")
writeData(OUT, sheet = "salida", x = salida_multivariado)
saveWorkbook(OUT, paste0(usuario,"/Prueba Adres/salida multivariado.xlsx"), overwrite = TRUE)
