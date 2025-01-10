# R/query_db.R


#' Obtener los keytypes
#' @description
#' Muestra los nombres de las llaves que aceptan los parámetros "keytype", "columnas" y "filtro"
#' de las funciones de este paquete.
#' @export
keytypes <- function() {

  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con))
  DBI::dbListFields(con, "metabolitos")
}


#' Obtener identificadores
#' @description
#' A partir de una lista de identificadores, permite obtener una tabla con las filas de "metabolitos"
#' correspondientes. Además, se pueden elegir las columnas que se desean obtener.
#'
#' @param keys Lista de identificadores de la columna indicada en `keytype`.
#' @param keytype Cadena con el nombre de la columna que se desea usar como llave (por defecto, "R_Name").
#' @param columnas Lista de columnas que se desean extraer (por defecto,
#' `c("Pr_Key", "Order_Num", "R_Name", "metabolite", "HMDB_ID", "KEGG_ID", "InChI_Key", "FOBI_ID")`)
#' @seealso [get_id()]
#' @export
id_table <- function(keys, keytype = "R_Name",
                     columnas = c("Pr_Key", "Order_Num", "R_Name", "metabolite", "HMDB_ID", "KEGG_ID",
                                 "InChI_Key", "FOBI_ID")) {

  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con))

  #Formulamos una query para obtener los "Pr_Key" de las filas de "metabolitosExp" cuyo valor de
  #"keytype" coincida con alguno de los elementos de "keys":
  get_keys <- sprintf(
    "SELECT Pr_Key
    FROM metabolitosExp
    WHERE %s IN ('%s')",
    keytype,
    paste(keys, collapse = "','")
    )
  #Obtenemos dichas "Pr_Key" y eliminamos las repeticiones:
  pr_keys <- dplyr::distinct(DBI::dbGetQuery(con, get_keys))


  #Extraemos las "columnas" de "Metabolitos.xlsx" de las filas correspondientes a las "Pr_Key" obtenidas:
  query <- sprintf(
    "SELECT %s
    FROM metabolitos
    WHERE Pr_Key IN ('%s')",
    paste(unlist(columnas), collapse = ", "),
    paste(unlist(pr_keys$Pr_Key), collapse = "','")
    )


  #Mostramos un mensaje indicando que una misma celda puede contener más de un valor:
  message(
    "Puede haber más de un identificador en la misma celda. \nSi quiere obtener los valores separados de un identificador concreto, emplee la función 'get_id'."
    )


  #Devolvemos la tabla obtenida:
  return (DBI::dbGetQuery(con, query))
}

#' Obtener los valores separados de un identificador
#' @description
#' A partir de una lista de identificadores, permite obtener una tabla con las filas de "metabolitosExp"
#' correspondientes. Además, se pueden elegir las columnas que se desean obtener.
#'
#' De esta forma, se pueden extraer todos los identificadores de interés de los metabolitos indicados
#' separados individualmente, incluso cuando alguno de ellos tenga varios identificadores del mismo tipo.
#'
#' @param keys Lista de identificadores de la columna indicada en `keytype`.
#' @param keytype String con el nombre de la columna que se desea usar como llave ("R_Name" por defecto).
#' @param columnas Lista de columnas que se desean extraer (por defecto, la indicada en `keytype`).
#' @seealso [id_table()]
#' @export
get_id <- function(keys, keytype = "R_Name", columnas = keytype) {

  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con))

  #Obtenemos las "Pr_Key" (sin repeticiones) de "metabolitosExp" de las filas cuyo valor de "keytype"
  #coincida con algún elemento de "keys". Guardamos estas llaves en la variable `pr_keys`:
  get_keys <- sprintf(
    "SELECT Pr_Key
    FROM metabolitosExp
    WHERE %s IN ('%s')",
    keytype,
    paste(unlist(keys), collapse = "','")
    )
  pr_keys <- dplyr::distinct(DBI::dbGetQuery(con, get_keys))

  #Extraemos las "columnas" de las filas de "metabolitosExp" cuya "Pr_Key" coincida con algún valor de `pr_keys`.
  #Antes de devolver la tabla resultante, la filtramos para evitar que haya filas iguales:
  query <- sprintf(
    "SELECT %s
    FROM metabolitosExp
    WHERE Pr_Key IN ('%s')",
    paste(unlist(columnas), collapse = ", "),
    paste(unlist(pr_keys$Pr_Key), collapse = "','")
    )

  return(dplyr::distinct(DBI::dbGetQuery(con, query)))
}


#' Contar niveles de anotación
#' @description
#' A partir de una lista de identificadores, devuelve un *data frame* con el conteo de niveles de anotación
#' RefMet indicados en "Metabolitos.xlsx" para las filas correspondientes.
#'
#' @param keys Lista de identificadores de la columna indicada en `keytype`.
#' @param keytype Cadena con el nombre de la columna que se desea usar como llave ("R_Name" por defecto).
#' @export
count_levels <- function(keys, keytype = "R_Name") {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con))

  #Obtenemos las "Pr_Key" (sin repeticiones) de "metabolitosExp" de las filas cuyo valor de "keytype"
  #coincida con algún elemento de "keys". Guardamos estas llaves en la variable `pr_keys`:
  get_keys <- sprintf(
    "SELECT Pr_Key
    FROM metabolitosExp
    WHERE %s IN ('%s')",
    keytype,
    paste(unlist(keys), collapse = "','")
  )
  pr_keys <- dplyr::distinct(DBI::dbGetQuery(con, get_keys))

  #Contamos cuántos niveles RefMet ("RefMet") de cada tipo hay en las filas `pr_keys` de "Metabolitos.xlsx":
  query <- sprintf(
    "SELECT RefMet, COUNT(*) AS Count
     FROM metabolitos
     WHERE Pr_Key IN ('%s')
     GROUP BY RefMet",
     paste(unlist(pr_keys), collapse = "','")
  )
  DBI::dbGetQuery(con, query)
}

#' Filtrar por columna
#' @description
#' A partir de una lista de identificadores, permite filtrar las correspondientes filas de la base de datos
#' en función de la presencia o ausencia de un valor determinado de una columna determinada.
#'
#' @param keys Lista de identificadores de la columna indicada en `keytype`.
#' @param keytype Cadena con el nombre de la columna que se desea usar como llave ("R_Name" por defecto).
#' @param columnas Lista de columnas que se desean extraer (por defecto, "*", es decir, todas).
#' @param filtro Cadena con el nombre de la variable por la que se quiere filtrar.
#' @param valor Valor de la variable `filtro` por el que se quiere filtrar.
#' @param igual Booleano que indica si `valor` debe excluirse (`FALSE`) o si solo deben incluirse las filas que contenga `valor` (TRUE).
#' @export
filter_by <- function(keys, keytype = "R_Name", columnas = "*" , filtro, valor, igual = TRUE) {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con))

  #Obtenemos las filas de la base de datos original (valores únicos de Pr_Key) para las que se cumple
  #la condición del filtro:


  #Si "igual == FALSE":
  if (!igual) {
    #Obtenemos las "Pr_Key" de las filas de "metabolitosExp" correspondientes a la lista "keys"
    #en las que la variable "filtro" es igual a "valor". Guardamos estas "Pr_Key" (sin repeticiones) en
    #la variable "not_keys":
    get_keys <- sprintf("SELECT Pr_Key
                        FROM metabolitosExp
                        WHERE %s IN ('%s') AND %s == '%s'",
                        keytype,
                        paste(unlist(keys), collapse = "','"),
                        filtro,
                        valor)
    not_keys <- dplyr::distinct(DBI::dbGetQuery(con, get_keys))

    #Obtenemos las "Pr_Key" de las filas de "metabolitosExp" correspondientes a la lista "keys",
    #excluyendo aquellas cuya "Pr_Key" se encuentre en "not_keys":
    exc_keys <- sprintf("SELECT Pr_Key
                        FROM metabolitosExp
                        WHERE %s IN ('%s') AND Pr_Key NOT IN ('%s')",
                        keytype,
                        paste(unlist(keys), collapse = "','"),
                        paste(unlist(not_keys), collapse = "','"))
    pr_keys <- dplyr::distinct(DBI::dbGetQuery(con, exc_keys))

    #Mostramos las filas de "Metabolitos.xlsx" correspondientes a las "Pr_Key" resultantes:
    query <- sprintf("SELECT %s FROM metabolitos WHERE Pr_Key IN ('%s')",
                     paste(columnas, collapse = ","),
                     paste(unlist(pr_keys), collapse = "','"))
    DBI::dbGetQuery(con, query)
  }

  #Si "igual != FALSE":
  else{
    #Obtenemos las "Pr_Key" de las filas de "metabolitosExp" correspondientes a la lista "keys"
    #en las que la variable "filtro" es igual a "valor".
    get_keys <- sprintf("SELECT Pr_Key FROM metabolitosExp WHERE %s IN ('%s') AND %s = '%s'", keytype,
                           paste(unlist(keys), collapse = "','"), filtro, valor)
    pr_keys <- dplyr::distinct(DBI::dbGetQuery(con, get_keys))

    #Mostramos las filas correspondientes de "Metabolitos.xlsx":
    query <- sprintf("SELECT %s FROM metabolitos WHERE Pr_Key IN ('%s')",
                     paste(columnas, collapse = ","),
                     paste(unlist(pr_keys), collapse = "','"))
    DBI::dbGetQuery(con, query)
  }
}
