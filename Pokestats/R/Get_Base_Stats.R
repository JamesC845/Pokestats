#' Title
#'
#' @param name_list List of Pokemon names. 
#'
#' @return
#' @export
#'
#' @examples
dex <- function(name_list){
  Pokedex <- pokedex
  row.names(Pokedex) <- Pokedex$Name
  dataframe <- data.frame()
  dataframe <- Pokedex[name_list,]
  return(dataframe)
}