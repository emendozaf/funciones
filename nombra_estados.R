#' Substituye los diferentes nombres posibles de los estados por un nombre estandarizado corto o código
#'
#'

nombra_estados <- function(nombre, tipo) {
  #' @param nombre Vector con el nombre de los estados
  #' @param tipo Si desea el "nombre" simple; el "codigo" del INEGI; el "nombreinegi", tal y como aparece en su católogo; "regiona" FIRA; "2" o "3" dígitos de la abreviación según la ISO 3166-2
  #'
  #'
  #'
  library(stringr)
  library(purrr)
  library(dplyr)
  
  n1 <- nombre
  
  # Elimina acentos y espacios extras
  nombre <- sapply(nombre, function(x) str_squish(x))
  nombre <- str_replace_all(nombre,
                            c(
                              "á" = "a",
                              "é" = "e",
                              "í" = "i",
                              "ó" = "o",
                              "ú" = "u"
                            ))
  nombre <- (tolower(nombre))
  n2 <- nombre
  
  #### Catálogo ####      
  catalogo <- list(
    "aguascalientes" = list(
      "nombre" = "Aguascalientes",
      "codigo" = "01",
      "nombreinegi" =  "Aguascalientes",
      "regional" = "Occidente",
      "2"="AG",
      "3"="AGU"
    ),
    "baja california" = list(
      "nombre" = "Baja California",
      "codigo" = "02",
      "nombreinegi" =  "Baja California",
      "regional" = "Noroeste",
      "2"="BC",
      "3"="BCN"
    ),
    "baja california sur" = list(
      "nombre" = "Baja California Sur",
      "codigo" = "03",
      "nombreinegi" =  "Baja California Sur",
      "regional" = "Noroeste",
      "2"="BS",
      "3"="BCS"
    ),
    "campeche" = list(
      "nombre" = "Campeche",
      "codigo" = "04",
      "nombreinegi" =  "Campeche",
      "regional" = "Sureste",
      "2"="CM",
      "3"="CAM"
    ),
    "chiapas" = list(
      "nombre" = "Chiapas",
      "codigo" = "07",
      "nombreinegi" =  "Chiapas",
      "regional" = "Sureste",
      "2"="CS",
      "3"="CHP"
    ),
    "chihuahua" = list(
      "nombre" = "Chihuahua",
      "codigo" = "08",
      "nombreinegi" =  "Chihuahua",
      "regional" = "Norte",
      "2"="CH",
      "3"="CHH"
    ),
    "distrito federal" = list(
      "nombre" = "Ciudad de México",
      "codigo" = "09",
      "nombreinegi" =  "Ciudad de México",
      "regional" = "Sur",
      "2"="CX",
      "3"="CMX"
    ),
    "ciudad de mexico" = list(
      "nombre" = "Ciudad de México",
      "codigo" = "09",
      "nombreinegi" =  "Ciudad de México",
      "regional" = "Sur",
      "2"="CX",
      "3"="CMX"
    ),
    "coahuila" = list(
      "nombre" = "Coahuila",
      "codigo" = "05",
      "nombreinegi" =  "Coahuila de Zaragoza",
      "regional" = "Norte",
      "2"="CO",
      "3"="COA"
    ),
    "coahuila de zaragoza" = list(
      "nombre" = "Coahuila",
      "codigo" = "05",
      "nombreinegi" =  "Coahuila de Zaragoza",
      "regional" = "Norte",
      "2"="CO",
      "3"="COA"
    ),
    "colima" = list(
      "nombre" = "Colima",
      "codigo" = "06",
      "nombreinegi" =  "Colima",
      "regional" = "Occidente",
      "2"="CL",
      "3"="COL"
    ),
    "durango" = list(
      "nombre" = "Durango",
      "codigo" = "10",
      "nombreinegi" =  "Durango",
      "regional" = "Occidente",
      "2"="DG",
      "3"="DUR"
    ),
    "guanajuato" = list(
      "nombre" = "Guanajuato",
      "codigo" = "11",
      "nombreinegi" =  "Guanajuato",
      "regional" = "Occidente",
      "2"="GT",
      "3"="GUA"
    ),
    "guerrero" = list(
      "nombre" = "Guerrero",
      "codigo" = "12",
      "nombreinegi" =  "Guerrero",
      "regional" = "Sur",
      "2"="GR",
      "3"="GRO"
    ),
    "hidalgo" = list(
      "nombre" = "Hidalgo",
      "codigo" = "13",
      "nombreinegi" =  "Hidalgo",
      "regional" = "Sur",
      "2"="HG",
      "3"="HID"
    ),
    "jalisco" = list(
      "nombre" = "Jalisco",
      "codigo" = "14",
      "nombreinegi" =  "Jalisco",
      "regional" = "Occidente",
      "2"="JC",
      "3"="JAL"
    ),
    "estado de mexico" = list(
      "nombre" = "México",
      "codigo" = "15",
      "nombreinegi" =  "México",
      "regional" = "Sur",
      "2"="EM",
      "3"="MEX"
    ),
    "mexico" = list(
      "nombre" = "México",
      "codigo" = "15",
      "nombreinegi" =  "México",
      "regional" = "Sur",
      "2"="EM",
      "3"="MEX"
    ),
    "michoacan" = list(
      "nombre" = "Michoacán",
      "codigo" = "16",
      "nombreinegi" =  "Michoacán de Ocampo",
      "regional" = "Occidente",
      "2"="MI",
      "3"="MIC"
    ),
    "michoacan de ocampo" = list(
      "nombre" = "Michoacán",
      "codigo" = "16",
      "nombreinegi" =  "Michoacán de Ocampo",
      "regional" = "Occidente",
      "2"="MI",
      "3"="MIC"
    ),
    "morelos" = list(
      "nombre" = "Morelos",
      "codigo" = "17",
      "nombreinegi" =  "Morelos",
      "regional" = "Sur",
      "2"="MO",
      "3"="MOR"
    ),
    "nayarit" = list(
      "nombre" = "Nayarit",
      "codigo" = "18",
      "nombreinegi" =  "Nayarit",
      "regional" = "Occidente",
      "2"="NA",
      "3"="NAY"
    ),
    "nuevo leon" = list(
      "nombre" = "Nuevo León",
      "codigo" = "19",
      "nombreinegi" =  "Nuevo León",
      "regional" = "Norte",
      "2"="NL",
      "3"="NLE"
    ),
    "oaxaca" = list(
      "nombre" = "Oaxaca",
      "codigo" = "20",
      "nombreinegi" =  "Oaxaca",
      "regional" = "Sur",
      "2"="OA",
      "3"="OAX"
    ),
    "puebla" = list(
      "nombre" = "Puebla",
      "codigo" = "21",
      "nombreinegi" =  "Puebla",
      "regional" = "Sur",
      "2"="PU",
      "3"="PUE"
    ),
    "queretaro arteaga" = list(
      "nombre" = "Querétaro",
      "codigo" = "22",
      "nombreinegi" =  "Querétaro",
      "regional" = "Occidente",
      "2"="QT",
      "3"="QUE"
    ),
    "queretaro" = list(
      "nombre" = "Querétaro",
      "codigo" = "22",
      "nombreinegi" =  "Querétaro",
      "regional" = "Occidente",
      "2"="QT",
      "3"="QUE"
    ),
    "quintana roo" = list(
      "nombre" = "Quintana Roo",
      "codigo" = "23",
      "nombreinegi" =  "Quintana Roo",
      "regional" = "Occidente",
      "2"="QR",
      "3"="ROO"
    ),
    "san luis potosi" = list(
      "nombre" = "San Luis Potosí",
      "codigo" = "24",
      "nombreinegi" =  "San Luis Potosí",
      "regional" = "Occidente",
      "2"="SL",
      "3"="SLP"
    ),
    "sinaloa" = list(
      "nombre" = "Sinaloa",
      "codigo" = "25",
      "nombreinegi" =  "Sinaloa",
      "regional" = "Noroeste",
      "2"="SI",
      "3"="SIN"
    ),
    "sonora" = list(
      "nombre" = "Sonora",
      "codigo" = "26",
      "nombreinegi" =  "Sonora",
      "regional" = "Noroeste",
      "2"="SO",
      "3"="SON"
    ),
    "tabasco" = list(
      "nombre" = "Tabasco",
      "codigo" = "27",
      "nombreinegi" =  "Tabasco",
      "regional" = "Sureste",
      "2"="TB",
      "3"="TAB"
    ),
    "tamaulipas" = list(
      "nombre" = "Tamaulipas",
      "codigo" = "28",
      "nombreinegi" =  "Tamaulipas",
      "regional" = "Norte",
      "2"="TM",
      "3"="TAM"
    ),
    "tlaxcala" = list(
      "nombre" = "Tlaxcala",
      "codigo" = "29",
      "nombreinegi" =  "Tlaxcala",
      "regional" = "Sur",
      "2"="TL",
      "3"="TLA"
    ),
    "veracruz" = list(
      "nombre" = "Veracruz",
      "codigo" = "30",
      "nombreinegi" =  "Veracruz de Ignacio de la Llave",
      "regional" = "Sur",
      "2"="VE",
      "3"="VER"
    ),
    "veracruz de ignacio de la llave" = list(
      "nombre" = "Veracruz",
      "codigo" = "30",
      "nombreinegi" =  "Veracruz de Ignacio de la Llave",
      "regional" = "Sur",
      "2"="VE",
      "3"="VER"
    ),
    "yucatan" = list(
      "nombre" = "Yucatán",
      "codigo" = "31",
      "nombreinegi" =  "Yucatán",
      "regional" = "Sureste",
      "2"="YU",
      "3"="YUC"
    ),
    "zacatecas" = list(
      "nombre" = "Zacatecas",
      "codigo" = "32",
      "nombreinegi" =  "Zacatecas",
      "regional" = "Occidente",
      "2"="ZA",
      "3"="ZAC"
    )
  )
  
  
  #### Ejecuta código ####
  A <- data.frame(sapply(catalogo, function(x)
    x[[tipo]]))
  A[,2] <-rownames(A)
  for (i in 1:nrow(A)){
    nombre <- replace(nombre,grep(paste0("^",A[i,2],"$"),nombre),A[i,1])
  }
  
  no_incluidos <- unique(n1[!(n2 %in% A[,2])])
  if (length(no_incluidos)>0) {
    print("Los siguientes nombres no están en el catálogo:")
    print(no_incluidos)
  }
  
  return(nombre)
}