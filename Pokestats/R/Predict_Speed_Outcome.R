#' Predict Speed Outcomes
#'
#' Minimum speed means 0 EV and neutral nature. Maximum Speed means 252 EV and beneficial nature.
#'
#' @param attacker 
#' @param defender 
#'
#' @return
#' @export
#'
#' @examples
outsp <- function(attacker, defender, attacker_level = 50, defender_level = 50){
  
  #
  atk_min <- est_min(c(attacker), level = attacker_level)$Speed_min 
  atk_max <- est_max(c(attacker), level = attacker_level)$Speed_max 
  
  def_min <- est_min(c(defender), level = defender_level)$Speed_min 
  def_max <- est_max(c(defender), level = defender_level)$Speed_max 
  
  #
  outspeed_calc <- function(attacker_speed_stat, defender_speed_stat){
    if(defender_speed_stat - attacker_speed_stat <= 0) 
      return("Yes") else return("No")  
  }
  
  #
  speed_outcomes_matrix <- matrix(c(
    outspeed_calc(atk_min, def_min),
    outspeed_calc(atk_min, def_max),
    outspeed_calc(atk_max, def_min),
    outspeed_calc(atk_max, def_max) 
  ), nrow = 2, ncol = 2)
  
  #
  row.names(speed_outcomes_matrix) <- c("Defender Min Speed", "Defender Max Speed")
  colnames(speed_outcomes_matrix) <- c("Attacker Min Speed", "Attacker Max Speed")
  
  #
  print(speed_outcomes_matrix)
  print("")
  print("")
  print("")
  print(paste("Attacker Min Speed: ", atk_min)) 
  print(paste("Attacker Max Speed: ", atk_max))
  print("")
  print(paste("Defender Min Speed: ", def_min))  
  print(paste("Defender Max Speed: ", def_max)) 

}
