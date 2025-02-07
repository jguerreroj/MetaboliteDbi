#' MetaboliteDbi: Exploración de la Base de Datos de Metabolitos
#'
#'Permite resolver las ambigüedades presentes en la tabla de metabolitos "Metabolitos.xlsx" y analizarla
#'mediante consultas SQL.
#'
#' Este paquete permite transformar la tabla de metabolitos "Metabolitos.xlsx" en una base de datos SQLite
#' con dos tablas: "metabolitos" (equivalente a "Metabolitos.xlsx" más una llave primaria) y "metabolitosExp",
#' una versión expandida de "metabolitos" en la que los identificadores múltiples han sido desglosados en filas
#' individuales.
#' Mediante consultas SQL a ambas tablas relacionadas, se pueden resolver las ambigüedades presentes en la tabla
#' de metabolitos. Para facilitar estas consultas, se incluyen funciones que realizan de forma automatizada algunas
#' de las consultas más comúnes: búsqueda de metabolitos, filtrados y el contaje de niveles de anotación (RefMet).
#' Estas funciones están inspiradas en los métodos de consultas de `AnnotationDbi`.
#'
#' @name MetaboliteDbi
"_PACKAGE"

NULL
