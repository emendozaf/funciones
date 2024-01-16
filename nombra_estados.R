#' Substituye los diferentes nombres posibles de los estados por un nombre estandarizado corto o c�digo
#'
#'

nombra_estados <- function(nombre, tipo) {
   #' @param nombre Vector con el nombre de los estados
   #' @param tipo Si desea el "nombre" simple, el "codigo" del INEGI o el "nombreinegi", tal y como aparece en su cat�logo
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
                                "�" = "a",
                                "�" = "e",
                                "�" = "i",
                                "�" = "o",
                                "�" = "u"
                             ))
   nombre <- (tolower(nombre))
   n2 <- nombre

   #### Cat�logo ####      
   catalogo <- list(
      "aguascalientes" = list(
         "nombre" = "Aguascalientes",
         "codigo" = "01",
         "nombreinegi" =  "Aguascalientes",
         "regional" = "Occidente"
      ),
      "baja california" = list(
         "nombre" = "Baja California",
         "codigo" = "02",
         "nombreinegi" =  "Baja California",
         "regional" = "Noroeste"
      ),
      "baja california sur" = list(
         "nombre" = "Baja California Sur",
         "codigo" = "03",
         "nombreinegi" =  "Baja California Sur",
         "regional" = "Noroeste"
      ),
      "campeche" = list(
         "nombre" = "Campeche",
         "codigo" = "04",
         "nombreinegi" =  "Campeche",
         "regional" = "Sureste"
      ),
      "chiapas" = list(
         "nombre" = "Chiapas",
         "codigo" = "07",
         "nombreinegi" =  "Chiapas",
         "regional" = "Sureste"
      ),
      "chihuahua" = list(
         "nombre" = "Chihuahua",
         "codigo" = "08",
         "nombreinegi" =  "Chihuahua",
         "regional" = "Norte"
      ),
      "distrito federal" = list(
         "nombre" = "Ciudad de M�xico",
         "codigo" = "09",
         "nombreinegi" =  "Ciudad de M�xico",
         "regional" = "Sur"
      ),
      "ciudad de mexico" = list(
         "nombre" = "Ciudad de M�xico",
         "codigo" = "09",
         "nombreinegi" =  "Ciudad de M�xico",
         "regional" = "Sur"
      ),
      "coahuila" = list(
         "nombre" = "Coahuila",
         "codigo" = "05",
         "nombreinegi" =  "Coahuila de Zaragoza",
         "regional" = "Norte"
      ),
      "coahuila de zaragoza" = list(
         "nombre" = "Coahuila",
         "codigo" = "05",
         "nombreinegi" =  "Coahuila de Zaragoza",
         "regional" = "Norte"
      ),
      "colima" = list(
         "nombre" = "Colima",
         "codigo" = "06",
         "nombreinegi" =  "Colima",
         "regional" = "Occidente"
      ),
      "durango" = list(
         "nombre" = "Durango",
         "codigo" = "10",
         "nombreinegi" =  "Durango",
         "regional" = "Occidente"
      ),
      "guanajuato" = list(
         "nombre" = "Guanajuato",
         "codigo" = "11",
         "nombreinegi" =  "Guanajuato",
         "regional" = "Occidente"
      ),
      "guerrero" = list(
         "nombre" = "Guerrero",
         "codigo" = "12",
         "nombreinegi" =  "Guerrero",
         "regional" = "Sur"
      ),
      "hidalgo" = list(
         "nombre" = "Hidalgo",
         "codigo" = "13",
         "nombreinegi" =  "Hidalgo",
         "regional" = "Sur"
      ),
      "jalisco" = list(
         "nombre" = "Jalisco",
         "codigo" = "14",
         "nombreinegi" =  "Jalisco",
         "regional" = "Occidente"
      ),
      "estado de mexico" = list(
         "nombre" = "M�xico",
         "codigo" = "15",
         "nombreinegi" =  "M�xico",
         "regional" = "Sur"
      ),
      "mexico" = list(
         "nombre" = "M�xico",
         "codigo" = "15",
         "nombreinegi" =  "M�xico",
         "regional" = "Sur"
      ),
      "michoacan" = list(
         "nombre" = "Michoac�n",
         "codigo" = "16",
         "nombreinegi" =  "Michoac�n de Ocampo",
         "regional" = "Occidente"
      ),
      "michoacan de ocampo" = list(
         "nombre" = "Michoac�n",
         "codigo" = "16",
         "nombreinegi" =  "Michoac�n de Ocampo",
         "regional" = "Occidente"
      ),
      "morelos" = list(
         "nombre" = "Morelos",
         "codigo" = "17",
         "nombreinegi" =  "Morelos",
         "regional" = "Sur"
      ),
      "nayarit" = list(
         "nombre" = "Nayarit",
         "codigo" = "18",
         "nombreinegi" =  "Nayarit",
         "regional" = "Occidente"
      ),
      "nuevo leon" = list(
         "nombre" = "Nuevo Le�n",
         "codigo" = "19",
         "nombreinegi" =  "Nuevo Le�n",
         "regional" = "Norte"
      ),
      "oaxaca" = list(
         "nombre" = "Oaxaca",
         "codigo" = "20",
         "nombreinegi" =  "Oaxaca",
         "regional" = "Sur"
      ),
      "puebla" = list(
         "nombre" = "Puebla",
         "codigo" = "21",
         "nombreinegi" =  "Puebla",
         "regional" = "Sur"
      ),
      "queretaro arteaga" = list(
         "nombre" = "Quer�taro",
         "codigo" = "22",
         "nombreinegi" =  "Quer�taro",
         "regional" = "Occidente"
      ),
      "queretaro" = list(
         "nombre" = "Quer�taro",
         "codigo" = "22",
         "nombreinegi" =  "Quer�taro",
         "regional" = "Occidente"
      ),
      "quintana roo" = list(
         "nombre" = "Quintana Roo",
         "codigo" = "23",
         "nombreinegi" =  "Quintana Roo",
         "regional" = "Occidente"
      ),
      "san luis potosi" = list(
         "nombre" = "San Luis Potos�",
         "codigo" = "24",
         "nombreinegi" =  "San Luis Potos�",
         "regional" = "Occidente"
      ),
      "sinaloa" = list(
         "nombre" = "Sinaloa",
         "codigo" = "25",
         "nombreinegi" =  "Sinaloa",
         "regional" = "Noroeste"
      ),
      "sonora" = list(
         "nombre" = "Sonora",
         "codigo" = "26",
         "nombreinegi" =  "Sonora",
         "regional" = "Noroeste"
      ),
      "tabasco" = list(
         "nombre" = "Tabasco",
         "codigo" = "27",
         "nombreinegi" =  "Tabasco",
         "regional" = "Sureste"
      ),
      "tamaulipas" = list(
         "nombre" = "Tamaulipas",
         "codigo" = "28",
         "nombreinegi" =  "Tamaulipas",
         "regional" = "Norte"
      ),
      "tlaxcala" = list(
         "nombre" = "Tlaxcala",
         "codigo" = "29",
         "nombreinegi" =  "Tlaxcala",
         "regional" = "Sur"
      ),
      "veracruz" = list(
         "nombre" = "Veracruz",
         "codigo" = "30",
         "nombreinegi" =  "Veracruz de Ignacio de la Llave",
         "regional" = "Sur"
      ),
      "veracruz de ignacio de la llave" = list(
         "nombre" = "Veracruz",
         "codigo" = "30",
         "nombreinegi" =  "Veracruz de Ignacio de la Llave",
         "regional" = "Sur"
      ),
      "yucatan" = list(
         "nombre" = "Yucat�n",
         "codigo" = "31",
         "nombreinegi" =  "Yucat�n",
         "regional" = "Sureste"
      ),
      "zacatecas" = list(
         "nombre" = "Zacatecas",
         "codigo" = "32",
         "nombreinegi" =  "Zacatecas",
         "regional" = "Occidente"
      )
   )
   
   
   #### Ejecuta c�digo ####
   A <- data.frame(sapply(catalogo, function(x)
      x[[tipo]]))
   A[,2] <-rownames(A)
   for (i in 1:nrow(A)){
      nombre <- replace(nombre,grep(paste0("^",A[i,2],"$"),nombre),A[i,1])
   }

   no_incluidos <- unique(n1[!(n2 %in% A[,2])])
   if (length(no_incluidos)>0) {
      print("Los siguientes nombres no est�n en el cat�logo:")
      print(no_incluidos)
   }
   
   return(nombre)
}