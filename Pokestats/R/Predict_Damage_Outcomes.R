#' Predict Damage Outcomes
#' 
#' Predict damage outcomes of defenders with mid defensive and HP EVs. Assumes the attacker has max attacking EVs and a beneficial nature. 
#'
#' @param attacker  
#' @param move 
#' @param defender 
#' @param level 
#' @param qpredic 
#'
#' @return
#' @export
#'
#' @examples
predic <- function(
    attacker, move, defender, 
    attacker_level = 50, 
    defender_level = 50,
    q = TRUE
    
){
  
  
  ######################################
  ######################################
  ######################################
  
  Turn_Outcome <- function(attacker, move, defender,
                           
                           attacker_level = attacker_level,
                           defender_level = defender_level,
                           
                           Attacker_Nature_Attack = 1,
                           Attacker_Nature_Speed = 1,
                           Defender_Nature_Speed = 1,
                           Defender_Nature_Defense = 1,
                           
                           Attacker_Attack_IV = 31,        # CAN MOVE TO predic() ARGUMENTS
                           Attacker_Attack_EV = 252,       # CAN MOVE TO predic() ARGUMENTS
                           # CAN MOVE TO predic() ARGUMENTS
                           Attacker_Speed_IV = 31,         # CAN MOVE TO predic() ARGUMENTS
                           Attacker_Speed_EV = 252,        # CAN MOVE TO predic() ARGUMENTS
                           
                           Defender_Defense_IV = 0,
                           Defender_Defense_EV = 0,
                           
                           Defender_Speed_IV = 0,
                           Defender_Speed_EV = 0,
                           
                           Defender_HP_IV = 0,
                           Defender_HP_EV = 0
                           
  ){
    
    
    #
    critical <- 1
    
    
    
    
    #
    attack_move <- subset(pokemoves, Name == move)
    power <- as.numeric(attack_move$Power)
    
    
    #
    if(attack_move$Category == "Physical"){
      A <-  est_val(attacker, IV = Attacker_Attack_IV, EV = Attacker_Attack_EV, level = attacker_level, Nature = Attacker_Nature_Attack)$Attack          # Nature = att_nat_Attack
      D <-  est_val(defender, IV = Defender_Defense_IV, EV = Defender_Defense_EV, level = defender_level, Nature = Defender_Nature_Defense)$Defense         # Nature = att_nat_Defense
    }
    
    if(attack_move$Category == "Special"){
      A <-  est_val(attacker, IV = Attacker_Attack_IV, EV = Attacker_Attack_EV, level = attacker_level, Nature = Attacker_Nature_Attack)$Sp..Atk          # Nature = att_nat_Attack
      D <-  est_val(defender, IV = Defender_Defense_IV, EV = Defender_Defense_EV, level = defender_level, Nature = Defender_Nature_Defense)$Sp..Def         # Nature = att_nat_Defense
    }
    
    
    #
    attacking_mon_stats <- dex(attacker)
    defending_mon_stats <- dex(defender)
    
    #
    if(attack_move$Type == attacking_mon_stats$Type.1 || attack_move$Type == attacking_mon_stats$Type.2 ){
      STAB <- 1.5
      
    } else STAB <- 1
    
    
    #
    defending_mon_resistances <- subset(resistances, name == defender)
    Resistance_Multiplier <- eval(parse(text = gsub(" ", "", paste("defending_mon_resistances$against_", tolower(attack_move$Type)))))
    
    
    # damage formula
    #  damage <- ((((((2 * attacker_level) / 5) + 2) * power * (A/D)) / 50) + 2) * critical * STAB * Resistance_Multiplier # gen 4
    damage <- ((((((2 * attacker_level) / 5) + 2) * power * (A/D)) / 50) + 2) * STAB * Resistance_Multiplier # gen 5
    
    
    
    
    ############################################################################
    
    
    
    calculate_damage_percentage <- function(HP_dam, HP_max) {
      percentage <- (HP_dam / HP_max) * 100
      return(percentage)
    }
    
    
    
    
    
    battle_mons_Turn_Outcome <- data.frame(
      Damage = damage,
      #    Attacker_Speed = est_val(attacker, IV = Attacker_Speed_IV, EV = Attacker_Speed_EV, Nature = Attacker_Nature_Speed)$Speed, 
      Defender_HP = est_val(defender, IV = Defender_HP_IV, EV = Defender_HP_EV, Nature = 1)$HP,                                          
      #    Defender_Speed = est_val(defender, IV = Defender_Speed_IV, EV = Defender_Speed_EV, Nature = Defender_Nature_Speed)$Speed,
      
      OHKO = ifelse(
        est_val(defender, IV = Defender_HP_IV, EV = Defender_HP_EV, Nature = 1)$HP - damage <= 0, 
        "Yes", 
        "No"),
      
#      Outspeed = ifelse(
#        est_val(attacker, IV = Attacker_Speed_IV, EV = Attacker_Speed_EV, level = attacker_level, Nature = Attacker_Nature_Speed)$Speed - 
#          est_val(defender, IV = Defender_Speed_IV, EV = Defender_Speed_EV, level = defender_level, Nature = Defender_Nature_Speed)$Speed >= 0,
#        "Yes", 
#        "No"),
      
      Remaining_HP = est_val(defender, IV = Defender_HP_IV, EV = Defender_HP_EV, Nature = 1)$HP - damage,
      
      Damage_Percentage = calculate_damage_percentage(HP_dam = damage, HP_max = est_val(defender, IV = Defender_HP_IV, EV = Defender_HP_EV, Nature = 1)$HP)
      
    )
    
    
    
    
    
    #  print("default function values assume IVs, EVs, and natures are maximised for both Pokemon.")
    return(battle_mons_Turn_Outcome)
    
    ##############################################################################
    
    #
    #  return(damage)
    
  }
  
  ######################################
  ######################################
  ######################################
  
  Nature_Outcomes <- function(attacker, move, defender,
                              
                              IV_Def = 0,          
                              EV_Def = 0,
                              
                              IV_Spd = 0,
                              EV_Spd = 0,
                              
                              IV_HP = 0,
                              EV_HP = 0,                       
                              
                              attacker_level = attacker_level,
                              defender_level = defender_level
                              
  ){
    
    NEU <- 1
    INC <- 1.1
    
    rbind(
      # Both NEU
      Turn_Outcome(
        attacker = attacker, move = move, defender = defender,
        Attacker_Nature_Attack = NEU, Attacker_Nature_Speed = NEU,
        Defender_Nature_Defense = NEU, Defender_Nature_Speed = NEU,
        
        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
        attacker_level = attacker_level, defender_level = defender_level
      ),
      # A Atk INC | D NEU
      Turn_Outcome(
        attacker = attacker, move = move, defender = defender,
        Attacker_Nature_Attack = INC, Attacker_Nature_Speed = NEU,
        Defender_Nature_Defense = NEU, Defender_Nature_Speed = NEU,
        
        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
        attacker_level = attacker_level, defender_level = defender_level
      ),
#      # A Spd INC | D NEU
#      Turn_Outcome(
#        attacker = attacker, move = move, defender = defender,
#        Attacker_Nature_Attack = NEU, Attacker_Nature_Speed = INC,
#        Defender_Nature_Defense = NEU, Defender_Nature_Speed = NEU,
#        
#        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
#        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
#        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
#        attacker_level = attacker_level, defender_level = defender_level
#      ),
      # A NEU | D Def INC
      Turn_Outcome(
        attacker = attacker, move = move, defender = defender,
        Attacker_Nature_Attack = NEU, Attacker_Nature_Speed = NEU,
        Defender_Nature_Defense = INC, Defender_Nature_Speed = NEU,
        
        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
        attacker_level = attacker_level, defender_level = defender_level
      ),
#      # A NEU | D Spd INC
#      Turn_Outcome(
#        attacker = attacker, move = move, defender = defender,
#        Attacker_Nature_Attack = NEU, Attacker_Nature_Speed = NEU,
#        Defender_Nature_Defense = NEU, Defender_Nature_Speed = INC,
#        
#        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
#        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
#        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
#        attacker_level = attacker_level, defender_level = defender_level
#      ),
      
      
      # A Atk INC | D Def INC
      Turn_Outcome(
        attacker = attacker, move = move, defender = defender,
        Attacker_Nature_Attack = INC, Attacker_Nature_Speed = NEU,
        Defender_Nature_Defense = INC, Defender_Nature_Speed = NEU,
        
        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
        attacker_level = attacker_level, defender_level = defender_level
      )#,
      
      
#      # A Spd INC | D SPD INC
#      Turn_Outcome(
#        attacker = attacker, move = move, defender = defender,
#        Attacker_Nature_Attack = NEU, Attacker_Nature_Speed = INC,
#        Defender_Nature_Defense = NEU, Defender_Nature_Speed = INC,
#        
#        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
#        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
#        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
#        attacker_level = attacker_level, defender_level = defender_level
#      ),
      
      
#      # A Atk INC | D Spd INC
#      Turn_Outcome(
#        attacker = attacker, move = move, defender = defender,
#        Attacker_Nature_Attack = INC, Attacker_Nature_Speed = NEU,
#        Defender_Nature_Defense = NEU, Defender_Nature_Speed = INC,
#        
#        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
#        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
#        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
#        attacker_level = attacker_level, defender_level = defender_level
#      ),
      
#      # A Spd INC | D Def INC
#      Turn_Outcome(
#        attacker = attacker, move = move, defender = defender,
#        Attacker_Nature_Attack = NEU, Attacker_Nature_Speed = INC,
#        Defender_Nature_Defense = INC, Defender_Nature_Speed = NEU,
#        
#        Defender_Defense_IV = IV_Def, Defender_Defense_EV = EV_Def,
#        Defender_Speed_IV = IV_Spd,  Defender_Speed_EV = EV_Spd,
#        Defender_HP_IV = IV_HP, Defender_HP_EV = EV_HP,                       
#        attacker_level = attacker_level, defender_level = defender_level
#      )
      
    ) -> outcomes_dataframe
    
    row.names(outcomes_dataframe) <- c(
      "A NEU | D NEU",
      "A Atk INC",
#      "A Spd INC",
      "D Def INC",
#      "D Spd INC",
      "A Atk INC | D Def INC"#,
#      "A Spd INC | D Spd INC",
#      "A Atk INC | D Spd INC",
#      "A Spd INC | D Def INC"
    )
    
    return(outcomes_dataframe) 
    
  }
  
  ######################################
  ######################################
  ######################################
  
  # "DEFENDER WITH LOW EVs
  Nature_Outcomes(attacker, move, defender,
                  IV_Def = 31, EV_Def = 0,  
                  IV_Spd = 31, EV_Spd = 0,  
                  IV_HP = 31, EV_HP = 0,
                  attacker_level = attacker_level, 
                  defender_level = defender_level  
  ) -> Defender_Low_Stats
  
  
  
  # DEFENDER WITH MID EVs
  Nature_Outcomes(attacker, move, defender,
                  IV_Def = 31, EV_Def = 126,
                  IV_Spd = 31, EV_Spd = 126,
                  IV_HP = 31, EV_HP = 126,  
                  attacker_level = attacker_level, 
                  defender_level = defender_level
  ) -> Defender_Mid_Stats
  
  
  
  # "DEFENDER WITH HIGH EVs
  Nature_Outcomes(
    attacker, move, defender,
    IV_Def = 31, EV_Def = 252,
    IV_Spd = 31, EV_Spd = 252,
    IV_HP = 31, EV_HP = 252,   
    attacker_level = attacker_level, 
    defender_level = defender_level
  ) -> Defender_High_Stats
  
  
  
  if(q == FALSE){
    
    print("")
    print("")
    print("")
    print("DEFENDER WITH LOW EVs:")
    print("")
    print(Defender_Low_Stats)
    print("")
    print("")
    print("")
    print("DEFENDER WITH MID EVs:")
    print("")
    print(Defender_Mid_Stats)
    print("")
    print("")
    print("")
    print("DEFENDER WITH HIGH EVs:")
    print("")
    print(Defender_High_Stats)
    
  }
  
  
  if(q == TRUE){
    
    quick_prediction <- data.frame(

#      row.names = c("Defender Low Stats", "Defender Mid Stats", "Defender High Stats"),
      Defender_Stats = c("Low Stats", "Mid Stats", "High Stats"),
      
      Min_Damage_Percentage = c(round(range(Defender_Low_Stats$Damage_Percentage)[1], 2), 
                                round(range(Defender_Mid_Stats$Damage_Percentage)[1], 2), 
                                round(range(Defender_High_Stats$Damage_Percentage)[1], 2))
      ,
      Max_Damage_Percentage = c(round(range(Defender_Low_Stats$Damage_Percentage)[2], 2), 
                                round(range(Defender_Mid_Stats$Damage_Percentage)[2], 2), 
                                round(range(Defender_High_Stats$Damage_Percentage)[2], 2))
#     ,
#      N_Outspeed_Outcomes = c(
#        paste("Outspeed", table(Defender_Low_Stats$Outspeed)[["Yes"]], "of 9"),
#        paste("Outspeed", table(Defender_Mid_Stats$Outspeed)[["Yes"]], "of 9"),
#        paste("Outspeed", table(Defender_High_Stats$Outspeed)[["Yes"]], "of 9")
#        )
    )
    
    return(quick_prediction)
    
  }
  
  
}


