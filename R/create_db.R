# R/create_db.R

#' Crear las bases de datos SQLite
#' @description
#' A partir del archivo "Metabolitos.xlsx", crea una base de datos
#' SQLite ("metabolites.sqlite") con dos tablas, "metabolitos" (que es equivalente
#' a la tabla de "Metabolitos.xlsx") y "metabolitosExp" (una versión
#' expandida de "metabolitos" en la que en la que las observaciones con valores
#' múltiples han sido desglosadas en varias filas, de tal forma que cada
#' combinación única de identificadores se encuentra en una fila separada).
#' @importFrom dplyr "%>%"
#' @export
create_metabolite_db <- function() {

  #Creamos `metabolitos`:
  ##Cargamos "Metabolitos.xlsx" en `metabolitos`:
  metabolitos <- readxl::read_excel(system.file("extdata","/Metabolitos.xlsx", package = "MetaboliteDbi"))

  ##Añadimos una llave primaria que identifique cada fila de forma única:
  metabolitos$Pr_Key <- paste0("K", seq_len(dim(metabolitos)[1]))
  metabolitos <- dplyr::select(metabolitos, Pr_Key, everything())


  #Creamos `metabolitos2`:
  ##Copiamos `metabolitos` en `metabolitos2`:
  metabolitos2 <- metabolitos

  ##Separamos los identificadores con valores múltiples:
  ###Recorremos cada columna de identificadores:
  id_columns <- c("HMDB_ID", "KEGG_ID", "InChI_Key", "FOBI_ID")
  for (col in id_columns) {
    metabolitos2 <- metabolitos2 %>%
      #Convertimos los valores de las columnas de los identificadores en listas,
      #indicando que los separadores son los saltos de fila (\r\n):
      dplyr::mutate(!!col := strsplit(!!dplyr::sym(col), "\\r\\n")) %>%

      #Dividimos las listas en filas individuales. Si una celda tiene dos identificadores, se crearán dos
      #filas idénticas excepto por el valor de esa columna, que contendrá un identificador diferente en
      #cada fila.
      tidyr::unnest(cols = all_of(col))
  }
  ###Al terminar el bucle, obtenemos una fila por cada combinación posible de los identificadores


  ###Transformamos en en " / " a los espacios (" ") que separan identificadores HMDB distintos:
  metabolitos2$HMDB_ID <- gsub(" HMDB", " / HMDB", metabolitos2$HMDB_ID)

  ###A continuación, repetimos el proceso de separación anterior con las columnas "metabolite", "origin"
  ###y "HMDB_ID", pero esta vez empleando el separador " / ".

  ###Sustituimos temporalmente la cadena " / wholegrain" de los valores de "origin" para protegerla al
  ###valor "food (fruits & vegetables / wholegrain - microbiota)" de la separación que se producirá a continuación:
  metabolitos2$origin <- gsub(" / wholegrain", "ESCUDOPROTECTOR", metabolitos2$origin)

  ###A continuación, repetimos el proceso de separación anterior con las columnas "metabolite", "origin"
  ###y "HMDB_ID", pero esta vez empleando el separador " / ".
  id_columns <- c("metabolite", "origin", "HMDB_ID")
  for (col in id_columns) {
    metabolitos2 <- metabolitos2 %>%
      dplyr::mutate(!!col := strsplit(!!dplyr::sym(col), " / ")) %>%
      tidyr::unnest(cols = all_of(col))
  }

  ###Revertimos la protección de "origin":
  metabolitos2$origin <- gsub("ESCUDOPROTECTOR", " / wholegrain", metabolitos2$origin)


  ##Limpieza de los valores:
  ###Eliminamos todos los caracteres de "metabolitos2" (excepto los presentes en la columna "metabolite")
  ###que no se encuentren en el rango ASCII comprendido entre " " y "~":
  metabolitos2 <- metabolitos2 %>%
    dplyr::mutate(across(-metabolite, ~ gsub("[^ -~]+", "", .)))

  ###Eliminamos los espacios blancos al principio y al final de cada celda de "metabolitos2":
  metabolitos2 <- metabolitos2 %>%
    dplyr::mutate(across(everything(), ~ trimws(.)))


  #Creamos la base de datos SQLite:
  db_path <- paste0(system.file("extdata", package = "MetaboliteDbi"), "/metabolites.sqlite")
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(db, "metabolitos", metabolitos, overwrite = TRUE)
  DBI::dbWriteTable(db, "metabolitosExp", metabolitos2, overwrite = TRUE)
  DBI::dbDisconnect(db)

  #Mostramos la dirección en la que se ha creado la base de datos:
  message("Base de datos creada: ", db_path)
}


#' Conexión a la base de datos
#' @description
#' Establece una conexión a la base de datos "metabolites.sqlite".
#'
#' @export
get_db_connection <- function() {
  db_path <- system.file("extdata", "metabolites.sqlite", package = "MetaboliteDbi")
  DBI::dbConnect(RSQLite::SQLite(), db_path)
}
