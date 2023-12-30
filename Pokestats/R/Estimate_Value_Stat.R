#' Estimate single stat using specified values and nature (values will be applied to all 6 stats)
#'
#' @param name_list 
#' @param IV 
#' @param EV 
#' @param level 
#' @param Nature 
#'
#' @return
#' @export
#'
#' @examples
est_val <- function(name_list,
                    IV = 15, 
                    EV = 100, 
                    level = 50, 
                    Nature = 1){
  
  #
  Pokestats <- dex(name_list)
  Pokestats
  
  #
  calculations <- data.frame(row.names = row.names(Pokestats))
  
  #
  Base_HP <- Pokestats[row.names(Pokestats),]$HP
  Base_Attack <- Pokestats[row.names(Pokestats),]$Attack
  Base_Defense <- Pokestats[row.names(Pokestats),]$Defense
  Base_Sp..Atk <- Pokestats[row.names(Pokestats),]$Sp..Atk
  Base_Sp..Def <- Pokestats[row.names(Pokestats),]$Sp..Def
  Base_Speed <- Pokestats[row.names(Pokestats),]$Speed
  
  ### HP
  calculations$HP <- floor(0.01 * (2 * Base_HP + IV + (0.25 * EV)) * level) + level + 10
  ### Attack
  calculations$Attack <-floor((floor(0.01 * (2 * Base_Attack + IV + floor(0.25 * EV)) * level) + 5) * Nature)
  ### Defense
  calculations$Defense <-floor((floor(0.01 * (2 * Base_Defense + IV + floor(0.25 * EV)) * level) + 5) * Nature)
  ### Sp..Atk
  calculations$Sp..Atk <-floor((floor(0.01 * (2 * Base_Sp..Atk + IV + floor(0.25 * EV)) * level) + 5) * Nature)
  ### Sp..Def
  calculations$Sp..Def <-floor((floor(0.01 * (2 * Base_Sp..Def + IV + floor(0.25 * EV)) * level) + 5) * Nature)
  ### Speed
  calculations$Speed <-floor((floor(0.01 * (2 * Base_Speed + IV + floor(0.25 * EV)) * level) + 5) * Nature)
  
  return(calculations)
  
}
