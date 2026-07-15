
Biomasas_2025_2026_finales_mapa <- readRDS(
  "data/Biomasas_2025_2026_finales_mapa.rds"
)

comunas_filtradas <- readRDS(
  "data/comunas_filtradas.rds"
)

ACS_biomasa <- readRDS(
  "data/ACS_biomasa.rds"
)

Estado_ACS_final <- readRDS(
  "data/Estado_ACS_final.rds"
)

# ============================================================
# 0. PAQUETES
# ============================================================

library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(leaflet)
library(scales)
library(htmltools)
library(htmlwidgets)


# ============================================================
# 1. FUNCIONES AUXILIARES
# ============================================================

max_seguro <- function(x) {
  
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  max(x, na.rm = TRUE)
}


suma_segura <- function(x) {
  
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  
  sum(x, na.rm = TRUE)
}


# Estandarizar los nombres de los estados ACS.
normalizar_estado_acs <- function(x) {
  
  x_normalizado <- iconv(
    trimws(as.character(x)),
    from = "",
    to = "ASCII//TRANSLIT"
  )
  
  x_normalizado <- tolower(
    trimws(x_normalizado)
  )
  
  case_when(
    x_normalizado == "descanso sanitario" ~ "Descanso sanitario",
    x_normalizado == "siembra" ~ "Siembra",
    x_normalizado == "produccion" ~ "Produccion",
    x_normalizado == "cosecha" ~ "Cosecha",
    TRUE ~ NA_character_
  )
}


# Estandarizar codigos ACS.
normalizar_codigo_acs <- function(x) {
  
  toupper(
    trimws(
      as.character(x)
    )
  )
}


# ============================================================
# 2. CARGA DE DATOS
# ============================================================

Biomasas_2025_2026_finales_mapa <- readRDS(
  "data/Biomasas_2025_2026_finales_mapa.rds"
)

comunas_filtradas <- readRDS(
  "data/comunas_filtradas.rds"
)

ACS_biomasa <- readRDS(
  "data/ACS_biomasa.rds"
)

Estado_ACS_final <- readRDS(
  "data/Estado_ACS_final.rds"
)


# ============================================================
# 3. PREPARACIoN DE LA BASE DE BIOMASAS
# ============================================================

biomasas <- Biomasas_2025_2026_finales_mapa %>%
  transmute(
    
    Comuna = as.character(Comuna),
    
    Periodo,
    
    `Fecha de Inicio del Periodo` =
      as.Date(`Fecha de Inicio del Periodo`),
    
    `Fecha de termino del periodo` =
      as.Date(`Fecha de termino del periodo`),
    
    biomasa_activa =
      as.numeric(`Biomasa activa (ton)`),
    
    biomasa_muerta =
      as.numeric(`Biomasa muerta (ton)`),
    
    biomasa_fan =
      as.numeric(`Biomasa muerta FAN (ton)`),
    
    biomasa_oxigeno =
      as.numeric(`Biomasa muerta oxigeno (ton)`),
    
    biomasa_activa_concesion =
      as.numeric(`Biomasa activa (ton) por concesion`),
    
    biomasa_muerta_concesion =
      as.numeric(`Biomasa muerta (ton) por concesion`),
    
    biomasa_fan_concesion =
      as.numeric(`Biomasa muerta FAN (ton) por concesion`),
    
    biomasa_oxigeno_concesion =
      as.numeric(`Biomasa muerta oxigeno (ton) por concesion`)
  )


# ============================================================
# 4. PRECALCULAR BIOMASAS POR COMUNA Y PERIODO
# ============================================================

biomasas_resumen <- biomasas %>%
  group_by(
    Comuna,
    Periodo,
    `Fecha de Inicio del Periodo`,
    `Fecha de termino del periodo`
  ) %>%
  summarise(
    
    biomasa_activa =
      suma_segura(biomasa_activa),
    
    biomasa_muerta =
      suma_segura(biomasa_muerta),
    
    biomasa_fan =
      suma_segura(biomasa_fan),
    
    biomasa_oxigeno =
      suma_segura(biomasa_oxigeno),
    
    biomasa_activa_concesion =
      max_seguro(biomasa_activa_concesion),
    
    biomasa_muerta_concesion =
      suma_segura(biomasa_muerta_concesion),
    
    biomasa_fan_concesion =
      suma_segura(biomasa_fan_concesion),
    
    biomasa_oxigeno_concesion =
      suma_segura(biomasa_oxigeno_concesion),
    
    .groups = "drop"
  )


# ============================================================
# 5. PREPARACIoN DE LOS ESTADOS ACS
# ============================================================

columnas_estado_requeridas <- c(
  "ACS",
  "Fecha de Inicio del Periodo",
  "Fecha de termino del periodo",
  "Estado ACS"
)

columnas_estado_faltantes <- setdiff(
  columnas_estado_requeridas,
  names(Estado_ACS_final)
)

if (length(columnas_estado_faltantes) > 0) {
  
  stop(
    paste(
      "Faltan columnas en Estado_ACS_final:",
      paste(
        columnas_estado_faltantes,
        collapse = ", "
      )
    )
  )
}


estado_acs <- Estado_ACS_final %>%
  transmute(
    
    Region = if ("Region" %in% names(Estado_ACS_final)) {
      as.character(Region)
    } else {
      NA_character_
    },
    
    ACS = normalizar_codigo_acs(ACS),
    
    `Fecha de Inicio del Periodo` =
      as.Date(`Fecha de Inicio del Periodo`),
    
    `Fecha de termino del periodo` =
      as.Date(`Fecha de termino del periodo`),
    
    `Estado ACS` =
      normalizar_estado_acs(`Estado ACS`)
  ) %>%
  filter(
    !is.na(ACS),
    ACS != "",
    !is.na(`Fecha de Inicio del Periodo`),
    !is.na(`Fecha de termino del periodo`)
  )


# Verificar estados no reconocidos.
estados_no_reconocidos <- Estado_ACS_final %>%
  transmute(
    estado_original = as.character(`Estado ACS`),
    estado_normalizado =
      normalizar_estado_acs(`Estado ACS`)
  ) %>%
  filter(
    !is.na(estado_original),
    is.na(estado_normalizado)
  ) %>%
  distinct(estado_original)


if (nrow(estados_no_reconocidos) > 0) {
  
  warning(
    paste(
      "Se encontraron estados ACS no reconocidos:",
      paste(
        estados_no_reconocidos$estado_original,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 6. PREPARACIoN DE GEOMETRiAS COMUNALES
# ============================================================

if (!inherits(comunas_filtradas, "sf")) {
  stop(
    "El objeto comunas_filtradas no conserva la clase sf."
  )
}

if (!"Comuna_2" %in% names(comunas_filtradas)) {
  stop(
    "El objeto comunas_filtradas no contiene el campo Comuna_2."
  )
}


comunas_biomasas <- comunas_filtradas %>%
  mutate(
    Comuna_2 = as.character(Comuna_2)
  ) %>%
  st_make_valid()


if (is.na(st_crs(comunas_biomasas))) {
  
  warning(
    paste(
      "Las geometrias comunales no tenian CRS.",
      "Se asigno EPSG:4326."
    )
  )
  
  st_crs(comunas_biomasas) <- 4326
}


comunas_biomasas <- comunas_biomasas %>%
  st_transform(3857) %>%
  st_simplify(
    dTolerance = 300,
    preserveTopology = TRUE
  ) %>%
  st_transform(4326)


campos_comunales <- intersect(
  c(
    "Region",
    "Provincia",
    "Comuna_2",
    "cod_comuna",
    "geometry"
  ),
  names(comunas_biomasas)
)

comunas_biomasas <- comunas_biomasas %>%
  select(
    all_of(campos_comunales)
  )


# ============================================================
# 7. PREPARACIoN DE GEOMETRiAS ACS
# ============================================================

if (!inherits(ACS_biomasa, "sf")) {
  stop(
    "El objeto ACS_biomasa no conserva la clase sf."
  )
}

if (!"ACS" %in% names(ACS_biomasa)) {
  stop(
    "El objeto ACS_biomasa no contiene el campo ACS."
  )
}


acs_geometrias <- ACS_biomasa %>%
  mutate(
    ACS = normalizar_codigo_acs(ACS)
  ) %>%
  st_make_valid()


if (is.na(st_crs(acs_geometrias))) {
  
  warning(
    paste(
      "Las geometrias ACS no tenian CRS.",
      "Se asigno EPSG:4326."
    )
  )
  
  st_crs(acs_geometrias) <- 4326
}


acs_geometrias <- acs_geometrias %>%
  st_transform(3857) %>%
  st_simplify(
    dTolerance = 100,
    preserveTopology = TRUE
  ) %>%
  st_transform(4326)


campos_acs <- intersect(
  c(
    "ACS",
    "code",
    "Latitud",
    "Longitud",
    "geometry"
  ),
  names(acs_geometrias)
)

acs_geometrias <- acs_geometrias %>%
  select(
    all_of(campos_acs)
  )


# ============================================================
# 8. VALIDACIONES DE UNIoN
# ============================================================

comunas_sin_geometria <- setdiff(
  unique(biomasas_resumen$Comuna),
  unique(comunas_biomasas$Comuna_2)
)

if (length(comunas_sin_geometria) > 0) {
  
  warning(
    paste(
      "Las siguientes comunas no tienen geometria:",
      paste(
        comunas_sin_geometria,
        collapse = ", "
      )
    )
  )
}


acs_sin_geometria <- setdiff(
  unique(estado_acs$ACS),
  unique(acs_geometrias$ACS)
)

if (length(acs_sin_geometria) > 0) {
  
  warning(
    paste(
      "Las siguientes ACS no tienen geometria:",
      paste(
        acs_sin_geometria,
        collapse = ", "
      )
    )
  )
}


geometrias_acs_sin_estado <- setdiff(
  unique(acs_geometrias$ACS),
  unique(estado_acs$ACS)
)

if (length(geometrias_acs_sin_estado) > 0) {
  
  warning(
    paste(
      "Las siguientes geometrias ACS no tienen estado:",
      paste(
        geometrias_acs_sin_estado,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# 9. PARaMETROS GENERALES
# ============================================================

fecha_minima <- min(
  c(
    biomasas_resumen$`Fecha de Inicio del Periodo`,
    estado_acs$`Fecha de Inicio del Periodo`
  ),
  na.rm = TRUE
)

fecha_maxima <- max(
  c(
    biomasas_resumen$`Fecha de termino del periodo`,
    estado_acs$`Fecha de termino del periodo`
  ),
  na.rm = TRUE
)


bbox_comunas <- st_bbox(
  comunas_biomasas
)


formato_toneladas <- scales::label_number(
  accuracy = 0.1,
  big.mark = ".",
  decimal.mark = ","
)

formato_concesion <- scales::label_number(
  accuracy = 0.01,
  big.mark = ".",
  decimal.mark = ","
)


# Colores de biomasa:
# valores bajos = azul
# valores altos = rojo

colores_biomasa <- c(
  "#FFFFCC",
  "#FFEDA0",
  "#FED976",
  "#FEB24C",
  "#FD8D3C",
  "#FC4E2A",
  "#E31A1C",
  "#BD0026",
  "#800026"
)


# Colores de estados ACS.
colores_estado_acs <- c(
  "Descanso sanitario" = "#808080",
  "Siembra" = "#87CEEB",
  "Produccion" = "#458B00",
  "Cosecha" = "#FF69B4"
)


pal_estado_acs <- colorFactor(
  palette = colores_estado_acs,
  domain = names(colores_estado_acs),
  na.color = "#D9D9D9"
)


# ============================================================
# 10. INTERFAZ
# ============================================================

ui <- page_sidebar(
  
  title = "Mapa temporal de biomasas y estado de ACS",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  fillable = TRUE,
  
  sidebar = sidebar(
    
    width = 350,
    
    title = "Controles",
    
    sliderInput(
      inputId = "rango_fechas",
      label = "Periodo de analisis:",
      min = fecha_minima,
      max = fecha_maxima,
      value = c(
        fecha_minima,
        fecha_maxima
      ),
      timeFormat = "%d-%m-%Y",
      step = 7,
      animate = FALSE
    ),
    
    div(
      class = "text-muted mb-3",
      style = "font-size: 12px;",
      paste(
        "Las comunas consideran todos los periodos que se",
        "superponen con el rango seleccionado.",
        "El estado de cada ACS corresponde al intervalo que",
        "contiene la fecha final del rango seleccionado."
      )
    ),
    
    hr(),
    
    card(
      
      full_screen = FALSE,
      
      card_header(
        strong(
          "Resumen del periodo seleccionado"
        )
      ),
      
      div(
        style = paste(
          "padding: 8px;",
          "border-left: 5px solid #2C7FB8;",
          "margin-bottom: 10px;"
        ),
        
        div(
          "Sumatoria de biomasa activa maxima por comuna",
          style = "font-size: 13px;"
        ),
        
        div(
          textOutput(
            "biomasa_activa_total",
            inline = TRUE
          ),
          style = "font-size: 20px; font-weight: bold;"
        )
      ),
      
      div(
        style = paste(
          "padding: 8px;",
          "border-left: 5px solid #636363;",
          "margin-bottom: 10px;"
        ),
        
        div(
          "Biomasa muerta total",
          style = "font-size: 13px;"
        ),
        
        div(
          textOutput(
            "biomasa_muerta_total",
            inline = TRUE
          ),
          style = "font-size: 20px; font-weight: bold;"
        )
      ),
      
      div(
        style = paste(
          "padding: 8px;",
          "border-left: 5px solid #7A0177;",
          "margin-bottom: 10px;"
        ),
        
        div(
          "Biomasa muerta por FAN",
          style = "font-size: 13px;"
        ),
        
        div(
          textOutput(
            "biomasa_fan_total",
            inline = TRUE
          ),
          style = "font-size: 20px; font-weight: bold;"
        )
      ),
      
      div(
        style = paste(
          "padding: 8px;",
          "border-left: 5px solid #238B45;"
        ),
        
        div(
          "Biomasa muerta por oxigeno",
          style = "font-size: 13px;"
        ),
        
        div(
          textOutput(
            "biomasa_oxigeno_total",
            inline = TRUE
          ),
          style = "font-size: 20px; font-weight: bold;"
        )
      )
    )
  ),
  
  leafletOutput(
    outputId = "mapa_biomasas",
    height = "calc(100vh - 80px)"
  )
)


# ============================================================
# 11. SERVIDOR
# ============================================================

server <- function(input, output, session) {
  
  
  # ----------------------------------------------------------
  # Selector temporal con debounce para biomasa
  # ----------------------------------------------------------
  
  rango_fechas_debounced <- reactive({
    
    req(input$rango_fechas)
    
    input$rango_fechas
    
  }) %>%
    debounce(500)
  
  
  # ----------------------------------------------------------
  # Filtrar biomasa
  # ----------------------------------------------------------
  
  datos_biomasa_filtrados <- reactive({
    
    rango <- rango_fechas_debounced()
    
    fecha_inicio <- as.Date(rango[1])
    fecha_termino <- as.Date(rango[2])
    
    biomasas_resumen %>%
      filter(
        `Fecha de Inicio del Periodo` <= fecha_termino,
        `Fecha de termino del periodo` >= fecha_inicio
      )
  })
  
  
  # ----------------------------------------------------------
  # Resumen comunal
  # ----------------------------------------------------------
  
  resumen_comunal <- reactive({
    
    df <- datos_biomasa_filtrados()
    
    if (nrow(df) == 0) {
      
      return(
        tibble(
          Comuna = character(),
          biomasa_activa_max = numeric(),
          biomasa_activa_max_concesion = numeric(),
          biomasa_muerta_sum = numeric(),
          biomasa_fan_sum = numeric(),
          biomasa_oxigeno_sum = numeric(),
          biomasa_muerta_sum_concesion = numeric(),
          biomasa_fan_sum_concesion = numeric(),
          biomasa_oxigeno_sum_concesion = numeric()
        )
      )
    }
    
    
    df %>%
      group_by(Comuna) %>%
      summarise(
        
        biomasa_activa_max =
          max_seguro(biomasa_activa),
        
        biomasa_activa_max_concesion =
          max_seguro(biomasa_activa_concesion),
        
        biomasa_muerta_sum =
          suma_segura(biomasa_muerta),
        
        biomasa_fan_sum =
          suma_segura(biomasa_fan),
        
        biomasa_oxigeno_sum =
          suma_segura(biomasa_oxigeno),
        
        biomasa_muerta_sum_concesion =
          suma_segura(biomasa_muerta_concesion),
        
        biomasa_fan_sum_concesion =
          suma_segura(biomasa_fan_concesion),
        
        biomasa_oxigeno_sum_concesion =
          suma_segura(biomasa_oxigeno_concesion),
        
        .groups = "drop"
      )
  })
  
  
  mapa_comunas <- reactive({
    
    comunas_biomasas %>%
      left_join(
        resumen_comunal(),
        by = c(
          "Comuna_2" = "Comuna"
        )
      )
  })
  
  
  # ----------------------------------------------------------
  # Estado ACS en la fecha final del slider
  # ----------------------------------------------------------
  
  estado_acs_actual <- reactive({
    
    req(input$rango_fechas)
    
    # Fecha mas actual del rango elegido.
    fecha_corte <- as.Date(
      input$rango_fechas[2]
    )
    
    
    # Seleccionar el registro cuyo intervalo contiene
    # exactamente la fecha final elegida.
    candidatos <- estado_acs %>%
      filter(
        `Fecha de Inicio del Periodo` <= fecha_corte,
        `Fecha de termino del periodo` >= fecha_corte
      )
    
    
    # Comprobar si existen intervalos superpuestos para una ACS.
    intervalos_duplicados <- candidatos %>%
      count(ACS) %>%
      filter(n > 1)
    
    
    if (nrow(intervalos_duplicados) > 0) {
      
      warning(
        paste(
          "Hay mas de un intervalo vigente para estas ACS:",
          paste(
            intervalos_duplicados$ACS,
            collapse = ", "
          )
        )
      )
    }
    
    
    # Si existiera mas de un intervalo coincidente,
    # conservar el que comenzo mas recientemente.
    candidatos %>%
      arrange(
        ACS,
        desc(`Fecha de Inicio del Periodo`),
        desc(`Fecha de termino del periodo`)
      ) %>%
      group_by(ACS) %>%
      slice_head(n = 1) %>%
      ungroup()
  })
  
  
  mapa_acs <- reactive({
    
    acs_geometrias %>%
      left_join(
        estado_acs_actual(),
        by = "ACS"
      )
  })
  
  
  # ----------------------------------------------------------
  # Paleta dinamica de biomasa
  # ----------------------------------------------------------
  
  paleta_biomasa <- reactive({
    
    valores <- mapa_comunas()$
      biomasa_activa_max_concesion
    
    valores_validos <- valores[
      is.finite(valores)
    ]
    
    
    if (length(valores_validos) == 0) {
      
      return(
        colorNumeric(
          palette = colores_biomasa,
          domain = c(0, 1),
          na.color = "#D9D9D9"
        )
      )
    }
    
    
    rango <- range(
      valores_validos,
      na.rm = TRUE
    )
    
    
    if (rango[1] == rango[2]) {
      
      margen <- max(
        abs(rango[1]) * 0.01,
        0.5
      )
      
      rango <- c(
        rango[1] - margen,
        rango[2] + margen
      )
    }
    
    
    colorNumeric(
      palette = colores_biomasa,
      domain = rango,
      na.color = "#D9D9D9"
    )
  })
  
  
  # ----------------------------------------------------------
  # Mapa inicial
  # ----------------------------------------------------------
  
  output$mapa_biomasas <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(
        zoomControl = FALSE,
        preferCanvas = TRUE
      )
    ) %>%
      
      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Mapa base",
        options = providerTileOptions(
          updateWhenIdle = TRUE,
          keepBuffer = 2
        )
      ) %>%
      
      fitBounds(
        lng1 = bbox_comunas[["xmin"]],
        lat1 = bbox_comunas[["ymin"]],
        lng2 = bbox_comunas[["xmax"]],
        lat2 = bbox_comunas[["ymax"]]
      ) %>%
      
      addLayersControl(
        overlayGroups = c(
          "Comunas",
          "ACS"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        ),
        position = "topright"
      ) %>%
      
      addScaleBar(
        position = "bottomright",
        options = scaleBarOptions(
          metric = TRUE,
          imperial = FALSE
        )
      ) %>%
      
      htmlwidgets::onRender(
        "
        function(el, x) {
          L.control.zoom({
            position: 'bottomright'
          }).addTo(this);
        }
        "
      )
  })
  
  
  # ----------------------------------------------------------
  # Actualizar comunas
  # ----------------------------------------------------------
  
  observeEvent(
    list(
      mapa_comunas(),
      paleta_biomasa()
    ),
    {
      
      datos_mapa <- mapa_comunas()
      pal <- paleta_biomasa()
      rango <- rango_fechas_debounced()
      
      
      valores_validos <- datos_mapa$
        biomasa_activa_max_concesion[
          is.finite(
            datos_mapa$biomasa_activa_max_concesion
          )
        ]
      
      
      fecha_inicio_texto <- format(
        as.Date(rango[1]),
        "%d-%m-%Y"
      )
      
      fecha_termino_texto <- format(
        as.Date(rango[2]),
        "%d-%m-%Y"
      )
      
      
      etiquetas_comunas <- sprintf(
        paste0(
          "<div style='min-width: 280px;'>",
          
          "<strong style='font-size: 15px;'>%s</strong>",
          
          "<br>",
          
          "<span style='color: #666;'>%s</span>",
          
          "<hr style='margin: 6px 0;'>",
          
          "<strong>",
          "Biomasa activa maxima por concesion:",
          "</strong>",
          
          "<br>%s ton/concesion",
          
          "<br><br>",
          
          "<strong>",
          "Biomasa muerta acumulada por concesion:",
          "</strong>",
          
          "<br>%s ton/concesion",
          
          "<br><br>",
          
          "<strong>",
          "Biomasa muerta FAN acumulada por concesion:",
          "</strong>",
          
          "<br>%s ton/concesion",
          
          "<br><br>",
          
          "<strong>",
          "Biomasa muerta por oxigeno acumulada por concesion:",
          "</strong>",
          
          "<br>%s ton/concesion",
          
          "</div>"
        ),
        
        datos_mapa$Comuna_2,
        
        paste0(
          fecha_inicio_texto,
          " al ",
          fecha_termino_texto
        ),
        
        ifelse(
          is.na(datos_mapa$biomasa_activa_max_concesion),
          "Sin informacion",
          formato_concesion(
            datos_mapa$biomasa_activa_max_concesion
          )
        ),
        
        ifelse(
          is.na(datos_mapa$biomasa_muerta_sum_concesion),
          "Sin informacion",
          formato_concesion(
            datos_mapa$biomasa_muerta_sum_concesion
          )
        ),
        
        ifelse(
          is.na(datos_mapa$biomasa_fan_sum_concesion),
          "Sin informacion",
          formato_concesion(
            datos_mapa$biomasa_fan_sum_concesion
          )
        ),
        
        ifelse(
          is.na(datos_mapa$biomasa_oxigeno_sum_concesion),
          "Sin informacion",
          formato_concesion(
            datos_mapa$biomasa_oxigeno_sum_concesion
          )
        )
      ) %>%
        lapply(HTML)
      
      
      proxy <- leafletProxy(
        mapId = "mapa_biomasas",
        data = datos_mapa
      ) %>%
        
        clearGroup("Comunas") %>%
        
        removeControl("leyenda_biomasa") %>%
        
        addPolygons(
          
          fillColor = ~pal(
            biomasa_activa_max_concesion
          ),
          
          fillOpacity = 0.5,
          
          color = "#404040",
          weight = 1,
          opacity = 0.9,
          
          smoothFactor = 1,
          
          label = etiquetas_comunas,
          
          labelOptions = labelOptions(
            direction = "auto",
            opacity = 1,
            textsize = "13px",
            style = list(
              "font-family" = "Arial",
              "padding" = "8px"
            )
          ),
          
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#2B2B2B",
            fillOpacity = 0.8,
            bringToFront = TRUE
          ),
          
          group = "Comunas"
        )
      
      
      if (length(valores_validos) > 0) {
        
        proxy %>%
          addLegend(
            
            layerId = "leyenda_biomasa",
            
            position = "bottomleft",
            
            pal = pal,
            
            values = valores_validos,
            
            opacity = 0.85,
            
            title = HTML(
              paste0(
                "<strong>Biomasa activa maxima</strong>",
                "<br>ton por concesion"
              )
            ),
            
            labFormat = labelFormat(
              big.mark = ".",
              digits = 1
            ),
            
            na.label = "Sin informacion"
          )
        
      } else {
        
        proxy %>%
          addControl(
            
            layerId = "leyenda_biomasa",
            
            html = HTML(
              paste0(
                "<div style='",
                "background: white;",
                "padding: 8px;",
                "border: 1px solid #999;",
                "border-radius: 4px;",
                "'>",
                
                "<strong>Biomasa activa maxima</strong>",
                
                "<br>",
                
                "Sin informacion para el periodo",
                
                "</div>"
              )
            ),
            
            position = "bottomleft"
          )
      }
    },
    
    ignoreInit = FALSE
  )
  
  
  # ----------------------------------------------------------
  # Actualizar ACS
  # ----------------------------------------------------------
  
  observeEvent(
    mapa_acs(),
    {
      
      datos_acs <- mapa_acs()
      
      fecha_corte <- as.Date(
        input$rango_fechas[2]
      )
      
      fecha_corte_texto <- format(
        fecha_corte,
        "%d-%m-%Y"
      )
      
      
      etiquetas_acs <- sprintf(
        paste0(
          "<div style='min-width: 220px;'>",
          
          "<strong style='font-size: 15px;'>ACS %s</strong>",
          
          "<br>",
          
          "<span style='color: #666;'>",
          "Estado al %s",
          "</span>",
          
          "<hr style='margin: 6px 0;'>",
          
          "<strong>Estado ACS:</strong>",
          
          "<br>%s",
          
          "<br><br>",
          
          "<strong>Inicio del periodo:</strong>",
          
          "<br>%s",
          
          "<br><br>",
          
          "<strong>Termino del periodo:</strong>",
          
          "<br>%s",
          
          "</div>"
        ),
        
        datos_acs$ACS,
        
        fecha_corte_texto,
        
        ifelse(
          is.na(datos_acs$`Estado ACS`),
          "Sin informacion",
          datos_acs$`Estado ACS`
        ),
        
        ifelse(
          is.na(datos_acs$`Fecha de Inicio del Periodo`),
          "Sin informacion",
          format(
            datos_acs$`Fecha de Inicio del Periodo`,
            "%d-%m-%Y"
          )
        ),
        
        ifelse(
          is.na(datos_acs$`Fecha de termino del periodo`),
          "Sin informacion",
          format(
            datos_acs$`Fecha de termino del periodo`,
            "%d-%m-%Y"
          )
        )
      ) %>%
        lapply(HTML)
      
      
      leafletProxy(
        mapId = "mapa_biomasas",
        data = datos_acs
      ) %>%
        
        clearGroup("ACS") %>%
        
        removeControl("leyenda_estado_acs") %>%
        
        addPolygons(
          
          # El color se obtiene del estado correspondiente
          # al intervalo que contiene la fecha final del slider.
          fillColor = ~dplyr::case_when(
            `Estado ACS` == "Descanso sanitario" ~ "#808080",
            `Estado ACS` == "Siembra" ~ "#87CEEB",
            `Estado ACS` == "Produccion" ~ "#458B00",
            `Estado ACS` == "Cosecha" ~ "#FF69B4",
            TRUE ~ "#D9D9D9"
          ),
          
          fillOpacity = 0.8,
          
          color = "#303030",
          weight = 0.8,
          opacity = 0.9,
          
          smoothFactor = 1,
          
          label = etiquetas_acs,
          
          labelOptions = labelOptions(
            direction = "auto",
            opacity = 1,
            textsize = "13px",
            style = list(
              "font-family" = "Arial",
              "padding" = "8px"
            )
          ),
          
          highlightOptions = highlightOptions(
            weight = 2.5,
            color = "#000000",
            fillOpacity = 0.9,
            bringToFront = TRUE
          ),
          
          group = "ACS"
        ) %>%
        
        addLegend(
          layerId = "leyenda_estado_acs",
          position = "bottomright",
          colors = unname(
            colores_estado_acs
          ),
          labels = names(
            colores_estado_acs
          ),
          opacity = 0.85,
          title = HTML(
            "<strong>Estado ACS</strong>"
          )
        )
    },
    
    ignoreInit = FALSE
  )
  
  
  # ----------------------------------------------------------
  # MeTRICAS DEL PANEL LATERAL
  # ----------------------------------------------------------
  
  output$biomasa_activa_total <- renderText({
    
    total <- sum(
      resumen_comunal()$biomasa_activa_max,
      na.rm = TRUE
    )
    
    paste0(
      formato_toneladas(total),
      " ton"
    )
  })
  
  
  output$biomasa_muerta_total <- renderText({
    
    total <- sum(
      resumen_comunal()$biomasa_muerta_sum,
      na.rm = TRUE
    )
    
    paste0(
      formato_toneladas(total),
      " ton"
    )
  })
  
  
  output$biomasa_fan_total <- renderText({
    
    total <- sum(
      resumen_comunal()$biomasa_fan_sum,
      na.rm = TRUE
    )
    
    paste0(
      formato_toneladas(total),
      " ton"
    )
  })
  
  
  output$biomasa_oxigeno_total <- renderText({
    
    total <- sum(
      resumen_comunal()$biomasa_oxigeno_sum,
      na.rm = TRUE
    )
    
    paste0(
      formato_toneladas(total),
      " ton"
    )
  })
}


# ============================================================
# 12. EJECUTAR APLICACIoN
# ============================================================

shinyApp(
  ui = ui,
  server = server
)