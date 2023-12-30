#' Estimate the minimum possible values of any Pokemon.
#'
#' @param name_list List of Pokemon names. 
#' @param reduce Logical value to state whether to reduce the stats shown.
#'
#' @return A dataframe of estimate battle stats.
#' @export
#'
#' @examples
est_min <- function(name_list, reduce = FALSE, level = 50){
  
  #
  Pokestats <- dex(name_list)
  Pokestats
  
  
  #
  IV_min <- 31
  IV_max <- 31
  
  Nature_mid <- 1
  Nature_max <- 1.1
  
  EV_min <- 0
  EV_max <- 252
  
  
  #
  calculations <- data.frame(row.names = row.names(Pokestats))
  
  
  #
  Base_HP <- Pokestats[row.names(Pokestats),]$HP
  Base_Attack <- Pokestats[row.names(Pokestats),]$Attack
  Base_Defense <- Pokestats[row.names(Pokestats),]$Defense
  Base_Sp..Atk <- Pokestats[row.names(Pokestats),]$Sp..Atk
  Base_Sp..Def <- Pokestats[row.names(Pokestats),]$Sp..Def
  Base_Speed <- Pokestats[row.names(Pokestats),]$Speed
  
  
  if(reduce == FALSE){
    ### HP
    calculations$HP_min <- floor(0.01 * (2 * Base_HP + IV_min + (0.25 * EV_min)) * level) + level + 10
    #  calculations$HP_max <- floor(0.01 * (2 * Base_HP + IV_max + (0.25 * EV_max)) * level) + level + 10
    ### Attack
    calculations$Attack_min <- floor((floor(0.01 * (2 * Base_Attack + IV_min + floor(0.25 * EV_min)) * level) + 5) * Nature_mid)
    #  calculations$Attack_max <-floor((floor(0.01 * (2 * Base_Attack + IV_max + floor(0.25 * EV_max)) * level) + 5) * Nature_max)
    ### Defense
    calculations$Defense_min <- floor((floor(0.01 * (2 * Base_Defense + IV_min + floor(0.25 * EV_min)) * level) + 5) * Nature_mid)
    #  calculations$Defense_max <-floor((floor(0.01 * (2 * Base_Defense + IV_max + floor(0.25 * EV_max)) * level) + 5) * Nature_max)
    ### Sp..Atk
    calculations$Sp..Atk_min <- floor((floor(0.01 * (2 * Base_Sp..Atk + IV_min + floor(0.25 * EV_min)) * level) + 5) * Nature_mid)
    #  calculations$Sp..Atk_max <-floor((floor(0.01 * (2 * Base_Sp..Atk + IV_max + floor(0.25 * EV_max)) * level) + 5) * Nature_max)
    ### Sp..Def
    calculations$Sp..Def_min <- floor((floor(0.01 * (2 * Base_Sp..Def + IV_min + floor(0.25 * EV_min)) * level) + 5) * Nature_mid)
    #  calculations$Sp..Def_max <-floor((floor(0.01 * (2 * Base_Sp..Def + IV_max + floor(0.25 * EV_max)) * level) + 5) * Nature_max)
    ### Speed
    calculations$Speed_min <- floor((floor(0.01 * (2 * Base_Speed + IV_min + floor(0.25 * EV_min)) * level) + 5) * Nature_mid)
    #  calculations$Speed_max <-floor((floor(0.01 * (2 * Base_Speed + IV_max + floor(0.25 * EV_max)) * level) + 5) * Nature_max)
  }
  
  
  
  if(reduce == TRUE){
    ### Speed
    calculations$Speed_min <- floor((floor(0.01 * (2 * Base_Speed + IV_min + floor(0.25 * EV_min)) * level) + 5) * Nature_mid)
    #    calculations$Speed_max <-floor((floor(0.01 * (2 * Base_Speed + IV_max + floor(0.25 * EV_max)) * level) + 5) * Nature_max)
    ### HP
    calculations$HP_min <- floor(0.01 * (2 * Base_HP + IV_min + (0.25 * EV_min)) * level) + level + 10
    #    calculations$HP_max <- floor(0.01 * (2 * Base_HP + IV_max + (0.25 * EV_max)) * level) + level + 10
  }
  
  
  return(calculations)
  
}
