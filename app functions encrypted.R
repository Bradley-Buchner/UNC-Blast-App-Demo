# Some lines of code are removed from this file for proprietary reasons

userpassword <- "encrypted"

# load fullteam_joinedBP query
table_name = "fullteam_joinedBP"
load_all_query <- paste0("SELECT * FROM ", table_name, ";")
# print(load_all_query)
blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
rs = dbSendQuery(blastDb, load_all_query)
fulleam_joinedBP <- dbFetch(rs)
# Clear the result
dbClearResult(rs)
# Disconnect to clean up the connection to the database.
dbDisconnect(blastDb)

# load fullteam_joinedLive query
table_name = "fullteam_joinedLive"
load_all_query <- paste0("SELECT * FROM ", table_name, ";")
# print(load_all_query)
blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
rs = dbSendQuery(blastDb, load_all_query)
fulleam_joinedLive <- dbFetch(rs)
# Clear the result
dbClearResult(rs)
# Disconnect to clean up the connection to the database.
dbDisconnect(blastDb)

colors <- data.frame(Status = c("Very Low", "Low", "Normal", "High", "Very High"), hex = c("#3333FF", "#CCCCFF", NA, "#CCFFCC", "#00FF00"))

sd_table_live <- fullteam_joinedLive %>% group_by(Hitter = tm_name) %>% summarise(
  sd_attack_aa = sd(attack_aa, na.rm = T),
  n = n(),
  avg_bat_speed = mean(`Bat Speed (mph)`, na.rm=T),
  sd_bat_speed = sd(`Bat Speed (mph)`, na.rm=T), 
  avg_hand_speed = mean(`Peak Hand Speed (mph)`, na.rm=T), 
  sd_hand_speed = sd(`Peak Hand Speed (mph)`, na.rm=T), 
  avg_timetocontact= mean(`Time to Contact (sec)`, na.rm=T), 
  sd_timetocontact= sd(`Time to Contact (sec)`, na.rm=T),
  aev = mean(ExitSpeed, na.rm = T), 
  sd_ev = sd(ExitSpeed, na.rm = T), 
  ala = mean(Angle, na.rm = T),
  sd_la = sd(Angle, na.rm = T),
  smash_factor = mean(smash_factor_score, na.rm=T), 
  sd_smash_factor = sd(smash_factor_score, na.rm=T), 
  avg_rot_score = mean(rotation_score, na.rm=T), 
  sd_rot_score = sd(rotation_score, na.rm=T), 
  attack_aa = mean(attack_aa, na.rm=T), 
  avg_ope = mean(`On Plane Efficiency (%)`, na.rm = T),
  sd_ope = sd(`On Plane Efficiency (%)`, na.rm = T),
  avg_vba = mean(`Vertical Bat Angle (deg)`, na.rm = T), 
  sd_vba = sd(`Vertical Bat Angle (deg)`, na.rm = T), 
  avg_ec = mean(`Early Connection (deg)`, na.rm = T), 
  sd_ec = sd(`Early Connection (deg)`, na.rm = T), 
  avg_ic = mean(`Connection at Impact (deg)`, na.rm = T), 
  sd_ic = sd(`Connection at Impact (deg)`, na.rm = T)
) %>% filter(!is.na(Hitter))


sd_table_bp <- fullteam_joinedBP %>% group_by(Hitter = tm_name) %>% dplyr::summarise(
  n = n(),
  sd_attack_aa = sd(attack_aa, na.rm = T),
  avg_bat_speed = mean(`Bat Speed (mph)`, na.rm=T),
  sd_bat_speed = sd(`Bat Speed (mph)`, na.rm=T),
  avg_hand_speed = mean(`Peak Hand Speed (mph)`, na.rm=T),
  sd_hand_speed = sd(`Peak Hand Speed (mph)`, na.rm=T),
  avg_timetocontact= mean(`Time to Contact (sec)`, na.rm=T),
  sd_timetocontact= sd(`Time to Contact (sec)`, na.rm=T),
  aev = mean(ExitSpeed, na.rm = T),
  sd_ev = sd(ExitSpeed, na.rm = T),
  ala = mean(Angle, na.rm = T),
  sd_la = sd(Angle, na.rm = T),
  smash_factor = mean(smash_factor_score, na.rm=T),
  sd_smash_factor = sd(smash_factor_score, na.rm=T),
  avg_rot_score = mean(rotation_score, na.rm=T),
  sd_rot_score = sd(rotation_score, na.rm=T),
  attack_aa = mean(attack_aa, na.rm = T),
  avg_ope = mean(`On Plane Efficiency (%)`, na.rm = T),
  sd_ope = sd(`On Plane Efficiency (%)`, na.rm = T),
  avg_vba = mean(`Vertical Bat Angle (deg)`, na.rm = T),
  sd_vba = sd(`Vertical Bat Angle (deg)`, na.rm = T),
  avg_ec = mean(`Early Connection (deg)`, na.rm = T),
  sd_ec = sd(`Early Connection (deg)`, na.rm = T),
  avg_ic = mean(`Connection at Impact (deg)`, na.rm = T),
  sd_ic = sd(`Connection at Impact (deg)`, na.rm = T)
) %>% filter(!is.na(Hitter))

conditional.formatting.film.room <- function(tbl, name, env) {
  
  df = tbl %>% 
    formatStyle(
      "Bat Speed (mph)",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "bs"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Hand Speed (mph)",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "hs"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Time to Contact (sec)",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "ttc"), c("#00FF00", "#CCFFCC", NA, NA, "#CCCCFF", "#3333FF"))
    ) %>% 
    formatStyle(
      "Rotation Score",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "rs"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Attack AA (deg)",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "attack_aa"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "On Plane Efficiency (%)",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "ope"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Vertical Bat Angle (deg)",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "vba"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Early Connection (deg)",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "ec"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Connection at Impact (deg)",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "ic"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Smash Factor",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "smash"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Exit Velo",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "ev"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    ) %>% 
    formatStyle(
      "Launch Angle",
      backgroundColor = styleInterval(style.interval(name = name, env = env, metric = "angle"), c("#3333FF", "#CCCCFF", NA, NA, "#CCFFCC", "#00FF00"))
    )
  
  return(df)
}

key.definition <- function(metric) {
  
  if (metric == "Bat Speed (mph)") {
    def = "Speed of the sweet spot at contact. Bat Speed is directly correlated with exit velocity. Bat Speed will be higher for pitches hit
    more out in front since contact is made later in the swing and the swing has more time to generate bat speed."
  } else if (metric == "Hand Speed (mph)") {
    def = "Peak speed of the bat's handle."
  } else if (metric == "Time to Contact (sec)") {
    def = "Time between the start of the downswing and contact."
  } else if (metric == "Smash Factor") {
    def = "Smash Factor is a measure for how well a ball is squared-up. It uses bat speed and exit velocity to measure how much
    bat speed is transferred into the ball. The more a ball is squared-up, the more bat speed 
    is transferred into the ball. So, Smash Factor is also a measure of how well you can turn bat speed into exit velocity.
    An average score is 100, and a score of 110 means the ball was squared-up 10% more than average. Smash Factor is NOT a 
    measure of power, only contact quality."
  } else if (metric == "Rotational Acceleration (g)") {
    def = "How fast the bat accelerates into the swing plane."
  } else if (metric == "Rotation Score") {
    def = "Rotation Score is a measure of rotational acceleration that accounts for where contact is made. Rotational acceleration will be higher for 
    inside pitches and lower for outside pitches, so it is difficult to compare the rotational acceleration of two swings if they are hit in two different locations. 
    Rotation Score allows you to compare between swings, since it is calculated by comparing a swing's actual rotational acceleration with 
    the expected rotational acceleration for that contact location. If a swing has a rotational acceleration of 15 and the expected rotational
    acceleration for that exact contact location is 10, then the swing's Rotation Score is (15/10)*100 = 1.5*100 = 150. An average score is 100, 
    and a score of 150 is 50% more rotational acceleration than average."
  } else if (metric == "Attack Angle (deg)") {
    def = "The angle of the bat's path with respect to horizontal at contact. A value of 0 degrees means the bat's path towards the ball is parallel with the ground at contact, 
    and a value of 10 means that bat's path is 10 degrees uphill at contact. Attack Angle increases (gets more uphill) as contact is made later in the swing,
    since contact is made in the upswing."
  } else if (metric == "Attack AA (deg)") {
    def = "Attack AA (deg) is a measure of attack angle that accounts for where contact is made. It's full name is `Attack Angle Above Average`. 
    Regular Attack Angle will be higher (more uphill) for pitches hit further out in front since contact is made later in the swing and during the upswing.  
    Similar to rotational acceleration, this makes it difficult to compare the bat path of two swings if they are hit in two different locations. 
    Attack AA (deg) allows you to compare between swings, since it is calculated by comparing a swing's actual attack angle with 
    the expected attack angle for that contact location. If a swing has an attack angle of 5 and the expected attack
    angle for that exact contact location is 8, then the swing's Attack AA (deg) is 5 - 8 = -3, so 3 degrees more downhill than expected (or average). 
    An average score is 0, which means the swing's attack angle is exactly the same as the expected/average attack angle for that contact location. An Attack AA of 10 
    means the swing has an attack angle that is 10 degrees more uphill than expected/average for that contact location."
  } else if (metric == "On Plane Efficiency (%)") {
    def = "The percentage of your swing that was on the Swing Plane. Imagine the Swing Plane as a disc around your body that
    is defined by your Vertical Bat Angle at contact. The sensor works backwards from contact to create your Swing Plane. 
    If the hitter maintains that Vertical Bat Angle the entire swing, they would be 100% On Plane."
  } else if (metric == "Vertical Bat Angle (deg)") {
    def = "The angle of the bat with respect to horizontal at contact. A value of 0 degrees means the bat is parallel with the ground at contact, 
    and a value of 90 means that bat is straight up and down at contact. Vertical Bat Angle will be closer to 90 for low pitches, and closer to 0
    for high pitches."
  } else if (metric == "Early Connection (deg)") {
    def = "The angle between the bat and torso at the start of the downswing."
  } else if (metric == "Connection at Impact (deg)") {
    def = "The angle between the bat and torso at contact."
  }
  
  # return(paste0("Definition: ", def))
  return(def)
  
  
}

key.visual <- function(metric) {
  
  if (metric == "Bat Speed (mph)") {
    width = 400
    height = 100
    src1 = "bat_speed_visual.png"
    src2 = "high_bat_speed_video.mov"
    src3 = NA
    statement1 = "High Bat Speed: 87 mph"
    statement2 = ""
  } 
  else if (metric == "Hand Speed (mph)") {
    width = 400
    height = 100
    src1 = ""
    src2 = "high_hand_speed_video.mov"
    src3 = ""
    statement1 = "High Hand Speed: 25 mph"
    statement2 = ""
  } else if (metric == "Time to Contact (sec)") {
    width = 400
    height = 100
    src1 = ""
    src2 = "fast_ttc_video.mov"
    src3 = ""
    statement1 = "Fast Time to Contact: 0.12 sec"
    statement2 = ""
  } else if (metric == "Smash Factor") {
    width = 400
    height = 100
    src1 = ""
    src2 = "high_smash_factor_video.mov"
    src3 = ""
    statement1 = "High Smash Factor: 113"
    statement2 = ""
  } else if (metric == "Rotational Acceleration (g)") {
    width = 400
    height = 100
    src1 = ""
    src2 = "high_ra_video.mov"
    src3 = ""
    statement1 = "High Rotational Acceleration: 24 g"
    statement2 = ""
  } else if (metric == "Rotation Score") {
    width = 400
    height = 100
    src1 = ""
    src2 = "high_rs_video.mov"
    src3 = ""
    statement1 = "High Rotation Score: 174"
    statement2 = ""
  } else if (metric == "Attack Angle (deg)") {
    width = 400
    height = 100
    src1 = "attack_angle_visual.png"
    src2 = "high_aa_video.mov"
    src3 = "low_aa_video.mov"
    statement1 = "High Attack Angle: 23 deg"
    statement2 = "Low Attack Angle: -1 deg"
  } else if (metric == "Attack AA (deg)") {
    width = 400
    height = 100
    src1 = ""
    src2 = "high_attack_aa_video.mov"
    src3 = "low_attack_aa_video.mov"
    statement1 = "High Attack AA: 16 deg AA"
    statement2 = "Low Attack AA: -20 deg AA"
  } else if (metric == "On Plane Efficiency (%)") {
    width = 400
    height = 100
    src1 = "plane_visual.png"
    src2 = "high_ope_video.mov"
    src3 = "low_ope_video.mov"
    statement1 = "High On Plane Efficiency: 73%"
    statement2 = "Low On Plane Efficiency: 42%"
  } else if (metric == "Vertical Bat Angle (deg)") {
    width = 400
    height = 100
    src1 = "vba_visual.png"
    src2 = "steep_vba_video.mov"
    src3 = "flat_vba_video.mov"
    statement1 = "Steep Vertical Bat Angle: -50 deg"
    statement2 = "Flat Vertical Bat Angle: -13 deg"
  } else if (metric == "Early Connection (deg)") {
    width = 400
    height = 100
    src1 = "ec_visual.png"
    src2 = "high_ec_video.mov"
    src3 = "low_ec_video.mov"
    statement1 = "High Early Connection: 129 deg"
    statement2 = "Low Early Connection: 82 deg"
  } else if (metric == "Connection at Impact (deg)") {
    width = 400
    height = 100
    src1 = "ic_visual.png"
    src2 = "high_ic_video.mov"
    src3 = "low_ic_video.mov"
    statement1 = "High Connection at Impact: 100 deg"
    statement2 = "Low Connection at Impact: 67 deg"
  }
  
  return(list(height, width, src1, src2, src3, statement1, statement2))
  
  
}

recent.session.summary <- function(name, env) {
  
  # env = 2
  # name = "blaser"
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data1 <- loadedData
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data1 <- loadedData
  }
  
  date = as.character(max(as.Date(unique(data1$tmDate))))
  summary_session <- data1 %>% filter(as.character(Date) == date) %>% summarise(
    n = n(),
    avg_bat_speed = round(mean(`Bat Speed (mph)`, na.rm=T), 1),
    avg_hand_speed = round(mean(`Peak Hand Speed (mph)`, na.rm=T), 1),
    avg_bat_eff = round(mean(`Bat Speed (mph)`/`Peak Hand Speed (mph)`, na.rm=T), 2),
    avg_rot = round(mean(`Rotational Acceleration (g)`, na.rm=T), 1),
    avg_timetocontact= round(mean(`Time to Contact (sec)`, na.rm=T), 2),
    avg_attack_angle = round(mean(`Attack Angle (deg)`, na.rm=T), 1),
    ev90th = round(quantile(ExitSpeed, .9, na.rm = T), 1),
    smash_factor = round(mean(smash_factor_score, na.rm=T), 1),
    avg_rot_score = round(mean(rotation_score, na.rm=T), 1),
    attack_aa = round(mean(attack_aa, na.rm=T), 1),
    middle_pct = round((sum(middle_of_field, na.rm = T)/n())*100, 1),
    good_angle_pct = round((sum(good_angle, na.rm=T)/n())*100, 1)
  )
  colnames(summary_session) <- c(
    "# Swings", "Bat Speed (mph)", "Hand Speed (mph)", "Bat Speed Efficiency", 
    "Rotational Acceleration (g)", "Time to Contact (sec)", "Attack Angle (deg)",
    "90th percentile Exit Velo", "Smash Factor", "Rotation Score", "Attack AA (deg)", "Gap 2 Gap %", "Sweet Spot %")
  summary_season <- data1 %>% summarise(
    n = n(),
    avg_bat_speed = round(mean(`Bat Speed (mph)`, na.rm=T), 1),
    avg_hand_speed = round(mean(`Peak Hand Speed (mph)`, na.rm=T), 1),
    avg_bat_eff = round(mean(`Bat Speed (mph)`/`Peak Hand Speed (mph)`, na.rm=T), 2),
    avg_rot = round(mean(`Rotational Acceleration (g)`, na.rm=T), 1),
    avg_timetocontact= round(mean(`Time to Contact (sec)`, na.rm=T), 2),
    avg_attack_angle = round(mean(`Attack Angle (deg)`, na.rm=T), 1),
    ev90th = round(quantile(ExitSpeed, .9, na.rm = T), 1),
    smash_factor = round(mean(smash_factor_score, na.rm=T), 1),
    avg_rot_score = round(mean(rotation_score, na.rm=T), 1),
    attack_aa = round(mean(attack_aa, na.rm=T), 1),
    middle_pct = round((sum(middle_of_field, na.rm = T)/n())*100, 1),
    good_angle_pct = round((sum(good_angle, na.rm=T)/n())*100, 1)
  )
  colnames(summary_season) <- c(
    "# Swings", "Bat Speed (mph)", "Hand Speed (mph)", "Bat Speed Efficiency", 
    "Rotational Acceleration (g)", "Time to Contact (sec)", "Attack Angle (deg)",
    "90th percentile Exit Velo", "Smash Factor", "Rotation Score", "Attack AA (deg)", "Gap 2 Gap %", "Sweet Spot %")
  
  df=bind_rows(summary_session, summary_season)
  df[is.na(df)] <- ""
  rownames(df) <- c(format(as.Date(date), "%h %d %Y"), "Season")
  
  df_final <- df[,c(1:5, 10, 6, 7, 11, 8, 9, 12, 13)]
  
  return(df_final)
}

hitter.summary <- function(name, env) {
  
  # env = 2
  # name = "long"
  
  tm_name = athletes[which(athletes$blast_name == name), "tm_name"]
  
  if (env == 1) {
    # load all of table query
    table_name = "fullteam_joinedBP"
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data1 <- loadedData
  } else {
    # load all of table query
    table_name = "fullteam_joinedLive"
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data1 <- loadedData
  }
  
  summary_season <- data1 %>% group_by(Hitter = tm_name) %>% summarise(
    n = sum(!is.na(`Bat Speed (mph)`)),
    avg_bat_speed = round(mean(`Bat Speed (mph)`, na.rm=T), 1),
    avg_hand_speed = round(mean(`Peak Hand Speed (mph)`, na.rm=T), 1),
    avg_timetocontact= round(mean(`Time to Contact (sec)`, na.rm=T), 2),
    aev = round(mean(ExitSpeed, na.rm = T), 1),
    smash_factor = round(mean(smash_factor_score, na.rm=T), 1),
    avg_rot_score = round(mean(rotation_score, na.rm=T), 1),
    attack_aa = round(mean(attack_aa, na.rm=T), 1),
    avg_ope = round(mean(`On Plane Efficiency (%)`, na.rm = T), 1),
    avg_vba = round(mean(`Vertical Bat Angle (deg)`, na.rm = T), 1),
    avg_ec = round(mean(`Early Connection (deg)`, na.rm = T), 1),
    avg_ic = round(mean(`Connection at Impact (deg)`, na.rm = T), 1)
  ) %>% filter(!is.na(Hitter))
  colnames(summary_season) <- c(
    "Hitter", "# Swings", "Bat Speed (mph)", "Hand Speed (mph)", "Time to Contact (sec)", 
    "Exit Velo", "Smash Factor", "Rotation Score", "Attack AA (deg)", "On Plane Efficiency (%)", 
    "Vertical Bat Angle (deg)", "Early Connection (deg)",
    "Connection at Impact (deg)")
  
  summary_season2 <- summary_season %>% mutate(
    bs_rank = rank(-`Bat Speed (mph)`, ties.method = "min"),
    hs_rank = rank(-`Hand Speed (mph)`, ties.method = "min"),
    ttc_rank = rank(`Time to Contact (sec)`, ties.method = "min"),
    ev_rank = rank(-`Exit Velo`, ties.method = "min"),
    smash_rank = rank(-`Smash Factor`, ties.method = "min"),
    rs_rank = rank(-`Rotation Score`, ties.method = "min"),
    attack_aa_rank = rank(`Attack AA (deg)`, ties.method = "min"),
    ope_rank = rank(-`On Plane Efficiency (%)`, ties.method = "min"),
    vba_rank = rank(`Vertical Bat Angle (deg)`, ties.method = "min"),
    ec_rank = rank(`Early Connection (deg)`, ties.method = "min"),
    ic_rank = rank(`Connection at Impact (deg)`, ties.method = "min")
  )
  
  player_summary <- summary_season2 %>% filter(Hitter == tm_name)
  player_summary_t = as.data.frame(t(player_summary))
  
  player_summary_t$Rank = NA
  player_summary_t$Rank = c("", "", player_summary_t$V1[14:24], rep("", 11))
  
  colnames(player_summary_t) <- c("", "Rank")
  
  player_final <- player_summary_t[2:13,]
  
  return(player_final)
}

best.swings <- function(name, metric, date, env) {
  
  # env = 1
  # name = "blaser"
  # date = "2023-01-15"
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    table <- data %>% filter(Date == date) %>% select(id, Date, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Rotational Acceleration (g)`,
                                                      `Time to Contact (sec)`, `Attack Angle (deg)`, ExitSpeed, smash_factor_score, 
                                                      attack_aa) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
    
    table <- table %>% mutate(
      `Video Link` = "No Video"
    )
    
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData2 <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData2
    
    table <- data %>% filter(Date == date) %>% select(id, Date, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Rotational Acceleration (g)`,
                                                      `Time to Contact (sec)`, `Attack Angle (deg)`, ExitSpeed, smash_factor_score, 
                                                      attack_aa, HitterLink) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
  }
  
  table$bs_efficiency = round(table$`Bat Speed (mph)`/table$`Peak Hand Speed (mph)`, 2)
  table$smash_factor_score = round(table$smash_factor_score, 2)
  table$attack_aa = round(table$attack_aa, 1)
  
  table$ExitSpeed <- round(table$ExitSpeed, 1)
  
  # table[is.na(table)] <- ""
  table$ExitSpeed <- as.numeric(table$ExitSpeed)
  
  
  table <- table[, c(1, 2, 3, 4, 12, 5, 6, 7, 10, 8, 9, 11)]
  
  colnames(table) <- c("Swing ID", "Date", "Bat Speed (mph)", 
                       "Hand Speed (mph)", "Bat Speed Efficiency", "Rotational Acceleration (g)", 
                       "Time to Contact (sec)", "Attack Angle (deg)", "Attack AA (deg)", 
                       "Exit Velo", "Smash Factor", "Video Link")
  
  if (metric %in% c("Bat Speed (mph)", 
                    "Hand Speed (mph)", "Bat Speed Efficiency", "Rotational Acceleration (g)", 
                    "Attack Angle (deg)", "Attack AA (deg)","Exit Velo", "Smash Factor")) {
    ret <- table %>% select(`metric`, `Video Link`)
    return(ret[order(-ret[,`metric`]), ])
  }
  if (metric == "Time to Contact (sec)") {
    ret <- table %>% select(`metric`, `Video Link`)
    return(ret[order(ret[,`metric`]), ])
  }
}

worst.swings <- function(name, metric, date, env) {
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    table <- data %>% filter(Date == date) %>% select(id, Date, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Rotational Acceleration (g)`,
                                                      `Time to Contact (sec)`, `Attack Angle (deg)`, ExitSpeed, smash_factor_score, 
                                                      attack_aa) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
    
    table <- table %>% mutate(
      `Video Link` = "No Video"
    )
    
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    table <- data %>% filter(Date == date) %>% select(id, Date, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Rotational Acceleration (g)`,
                                                      `Time to Contact (sec)`, `Attack Angle (deg)`, ExitSpeed, smash_factor_score, 
                                                      attack_aa, HitterLink) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
  }
  
  table$bs_efficiency = round(table$`Bat Speed (mph)`/table$`Peak Hand Speed (mph)`, 2)
  table$smash_factor_score = round(table$smash_factor_score, 2)
  table$attack_aa = round(table$attack_aa, 1)
  
  table$ExitSpeed <- round(table$ExitSpeed, 1)
  
  # table[is.na(table)] <- ""
  table$ExitSpeed <- as.numeric(table$ExitSpeed)
  
  
  table <- table[, c(1, 2, 3, 4, 12, 5, 6, 7, 10, 8, 9, 11)]
  
  colnames(table) <- c("Swing ID", "Date", "Bat Speed (mph)", 
                       "Hand Speed (mph)", "Bat Speed Efficiency", "Rotational Acceleration (g)", 
                       "Time to Contact (sec)", "Attack Angle (deg)", "Attack AA (deg)", 
                       "Exit Velo", "Smash Factor", "Video Link")
  
  if (metric %in% c("Bat Speed (mph)", 
                    "Hand Speed (mph)", "Bat Speed Efficiency", "Rotational Acceleration (g)", 
                    "Attack Angle (deg)", "Attack AA (deg)","Exit Velo", "Smash Factor")) {
    ret <- table %>% select(`metric`, `Video Link`)
    return(ret[order(ret[,`metric`]), ])
  }
  if (metric == "Time to Contact (sec)") {
    ret <- table %>% select(`metric`, `Video Link`)
    return(ret[order(-ret[,`metric`]), ])
  }
  
}

session.summary <- function(name, date, env) {
  
  # env = 2
  # name = "blaser"
  # date = "2023-01-15"
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data1 <- loadedData
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data1 <- loadedData
  }
  
  summary_session <- data1 %>% filter(as.character(Date) == date) %>% summarise(
    n = sum(!is.na(`Bat Speed (mph)`)),
    avg_bat_speed = round(mean(`Bat Speed (mph)`, na.rm=T), 1),
    avg_hand_speed = round(mean(`Peak Hand Speed (mph)`, na.rm=T), 1),
    avg_timetocontact= round(mean(`Time to Contact (sec)`, na.rm=T), 2),
    avg_rot_score = round(mean(rotation_score, na.rm=T), 1),
    attack_aa = round(mean(attack_aa, na.rm=T), 1),
    avg_ope = round(mean(`On Plane Efficiency (%)`, na.rm = T), 1),
    avg_vba = round(mean(`Vertical Bat Angle (deg)`, na.rm = T), 1),
    avg_ec = round(mean(`Early Connection (deg)`, na.rm = T), 1),
    avg_ic = round(mean(`Connection at Impact (deg)`, na.rm = T), 1),
    aev = round(mean(ExitSpeed, na.rm = T), 1),
    smash_factor = round(mean(smash_factor_score, na.rm=T), 1)
  ) 
  colnames(summary_session) <- c(
    "# Swings Captured", "Bat Speed (mph)", "Hand Speed (mph)", "Time to Contact (sec)", 
    "Rotation Score", "Attack AA (deg)", "On Plane Efficiency (%)", 
    "Vertical Bat Angle (deg)", "Early Connection (deg)",
    "Connection at Impact (deg)", "Exit Velo", "Smash Factor")
  
  summary_season <- data1 %>% filter(as.Date(Date) < date) %>% summarise(
    n = sum(!is.na(`Bat Speed (mph)`)),
    avg_bat_speed = round(mean(`Bat Speed (mph)`, na.rm=T), 1),
    avg_hand_speed = round(mean(`Peak Hand Speed (mph)`, na.rm=T), 1),
    avg_timetocontact= round(mean(`Time to Contact (sec)`, na.rm=T), 2),
    avg_rot_score = round(mean(rotation_score, na.rm=T), 1),
    attack_aa = round(mean(attack_aa, na.rm=T), 1),
    avg_ope = round(mean(`On Plane Efficiency (%)`, na.rm = T), 1),
    avg_vba = round(mean(`Vertical Bat Angle (deg)`, na.rm = T), 1),
    avg_ec = round(mean(`Early Connection (deg)`, na.rm = T), 1),
    avg_ic = round(mean(`Connection at Impact (deg)`, na.rm = T), 1),
    aev = round(mean(ExitSpeed, na.rm = T), 1),
    smash_factor = round(mean(smash_factor_score, na.rm=T), 1)
  ) 
  colnames(summary_season) <- c(
    "# Swings Captured", "Bat Speed (mph)", "Hand Speed (mph)", "Time to Contact (sec)", 
    "Rotation Score", "Attack AA (deg)", "On Plane Efficiency (%)", 
    "Vertical Bat Angle (deg)", "Early Connection (deg)",
    "Connection at Impact (deg)", "Exit Velo", "Smash Factor")
  
  df=bind_rows(summary_season, summary_session)
  df[is.na(df)] <- ""
  df[, c(1:12)] <- lapply(df[, c(1:12)], as.character)
  rownames(df) <- c("Season", format(as.Date(date), "%h %d"))
  
  df_final <- as.data.frame(t(df))
  
  for (i in 2:nrow(df_final)){
    # i = 2
    if (as.numeric(df_final[i,2]) > as.numeric(df_final[i,1])){
      df_final[i,2] <- paste0('<div style="background-color: #CCFFCC;"><span>', df_final[i,2], '</span></div>')
    } else if (as.numeric(df_final[i,2]) < as.numeric(df_final[i,1])) {
      df_final[i,2] <- paste0('<div style="background-color: #CCCCFF;"><span>', df_final[i,2], '</span></div>')
    }
  }
  
  
  return(df_final)
  
}

notebook.table <- function(name, env) {
  
  # env = 2
  # name = "blaser"
  
  tm_name = athletes[which(athletes$blast_name == name), "tm_name"]
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    table <- data %>% select(id, Date, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Time to Contact (sec)`, rotation_score,
                             attack_aa, `On Plane Efficiency (%)`, `Vertical Bat Angle (deg)`, `Early Connection (deg)`, 
                             `Connection at Impact (deg)`, smash_factor_score, ExitSpeed, Angle) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
    
    table$`Video Link` <- "No Video"
    
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    table <- data %>% select(id, Date, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Time to Contact (sec)`, rotation_score,
                             attack_aa, `On Plane Efficiency (%)`, `Vertical Bat Angle (deg)`, `Early Connection (deg)`, 
                             `Connection at Impact (deg)`, smash_factor_score, ExitSpeed, Angle, HitterLink) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
    
  }
  
  table$ExitSpeed <- round(as.numeric(table$ExitSpeed), 1)
  table$Angle <- round(as.numeric(table$Angle))
  
  table$smash_factor_score = round(table$smash_factor_score, 1)
  table$rotation_score = round(table$rotation_score, 1)
  table$attack_aa = round(table$attack_aa, 2)
  
  colnames(table) <- c("Swing ID", "Date", "Bat Speed (mph)", 
                       "Hand Speed (mph)", "Time to Contact (sec)", "Rotation Score", "Attack AA (deg)", "On Plane Efficiency (%)", 
                       "Vertical Bat Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)",
                       "Smash Factor", "Exit Velo", "Launch Angle", "Video Link")
  
  table2 <- table[,c(1, 2, 15, 3:14)]
  
  return(table2)
  
}

style.interval <- function(name, env, metric) {
  
  tm_name = athletes[which(athletes$blast_name == name), "tm_name"]
  
  # x = 65
  # tm_name = "Blaser, Bryce"
  # metric = "attack_aa"
  # env = 1
  
  if (env == 1) {
    if (metric == "bs") {
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "avg_bat_speed"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_bat_speed"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "hs") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "avg_hand_speed"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_hand_speed"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ttc") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "avg_timetocontact"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_timetocontact"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ev") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "aev"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_ev"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "angle") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "ala"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_la"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "smash") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "smash_factor"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_smash_factor"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "rs") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "avg_rot_score"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_rot_score"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "attack_aa") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "attack_aa"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_attack_aa"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ope") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "avg_ope"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_ope"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "vba") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "avg_vba"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_vba"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ec") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "avg_ec"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_ec"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ic") {
      
      mean = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "avg_ic"])
      sd = as.numeric(sd_table_bp[which(sd_table_bp$Hitter == tm_name), "sd_ic"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } 
  } else if (env == 2) {
    if (metric == "bs") {
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "avg_bat_speed"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_bat_speed"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "hs") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "avg_hand_speed"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_hand_speed"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ttc") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "avg_timetocontact"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_timetocontact"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ev") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "aev"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_ev"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "angle") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "ala"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_la"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "smash") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "smash_factor"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_smash_factor"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "rs") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "avg_rot_score"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_rot_score"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "attack_aa") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "attack_aa"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_attack_aa"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ope") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "avg_ope"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_ope"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "vba") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "avg_vba"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_vba"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ec") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "avg_ec"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_ec"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } else if (metric == "ic") {
      
      mean = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "avg_ic"])
      sd = as.numeric(sd_table_live[which(sd_table_live$Hitter == tm_name), "sd_ic"])
      
      intervals = c(mean-2*sd, mean-sd, mean, mean+sd, mean+2*sd)
      
      return(intervals)
      
    } 
  }
  
}

notebook.table.formatter <- function(table, name, env) {
  
  tm_name = athletes[which(athletes$blast_name == name), "tm_name"]
  
  bs_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "bs", env)
              ))
  
  hs_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "hs", env)))
  
  ttc_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "ttc", env)))
  
  ev_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "ev", env)))
  
  smash_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "smash", env)))
  
  rs_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "rs", env)))
  
  attack_aa_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "attack_aa", env)))
  
  ope_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "ope", env)))
  
  vba_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "vba", env)))
  
  ec_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(x, tm_name, metric = "ec", env)
              ))
  
  ic_formatter <-
    formatter("span",
              style = x ~ style(
                `background-color` = conditional.formatting.film.room(as.numeric(x), tm_name, metric = "ic", env)))
  
  
  table2 <- formattable(table, list(
    "Bat Speed (mph)" = bs_formatter,
    "Hand Speed (mph)" = hs_formatter,
    "Time to Contact (sec)" = ttc_formatter,
    "Rotation Score" = rs_formatter,
    "Attack AA (deg)" = attack_aa_formatter,
    "On Plane Efficiency (%)" = ope_formatter,
    "Vertical Bat Angle (deg)" = vba_formatter,
    "Early Connection (deg)" = ec_formatter,
    "Connection at Impact (deg)" = ic_formatter,
    "Exit Velo" = ev_formatter,
    "Smash Factor" = smash_formatter
  ))
  
  # as.datatable(table2, selection = 'single', options = list(pageLength = 15, dom = "tpi"), rownames = F, escape = F)
  # as.datatable(table2)
  
  
  # df = as.datatable(table2)
  
  return(table2)
}

fullteam.notebook.table <- function(name = "fullteam", env) {
  
  # env = 2
  # name = "blaser"
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    table <- data %>% select(id, tm_name, Date, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Time to Contact (sec)`, rotation_score,
                             attack_aa, `On Plane Efficiency (%)`, `Vertical Bat Angle (deg)`, `Early Connection (deg)`, 
                             `Connection at Impact (deg)`, smash_factor_score, ExitSpeed, Angle) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
    
    table$`Video Link` <- "No Video"
    
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    table <- data %>% select(id, tm_name, Date, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Time to Contact (sec)`, rotation_score,
                             attack_aa, `On Plane Efficiency (%)`, `Vertical Bat Angle (deg)`, `Early Connection (deg)`, 
                             `Connection at Impact (deg)`, smash_factor_score, ExitSpeed, Angle, HitterLink) %>%
      filter(!(is.na(`Bat Speed (mph)`)))
    
  }
  
  table$ExitSpeed <- round(as.numeric(table$ExitSpeed), 1)
  table$Angle <- round(as.numeric(table$Angle))
  
  table$smash_factor_score = round(table$smash_factor_score, 1)
  table$rotation_score = round(table$rotation_score, 1)
  table$attack_aa = round(table$attack_aa, 2)
  
  colnames(table) <- c("Swing ID", "Batter", "Date", "Bat Speed (mph)", 
                       "Hand Speed (mph)", "Time to Contact (sec)", "Rotation Score", "Attack AA (deg)", "On Plane Efficiency (%)", 
                       "Vertical Bat Angle (deg)", "Early Connection (deg)", "Connection at Impact (deg)",
                       "Smash Factor", "Exit Velo", "Launch Angle", "Video Link")
  
  table2 <- table[,c(1:3, 16, 4:15)]
  
  return(table2)
  
}

notebook.test <- function(name) {
  
  # name = "alvarez"
  # date = "2022-09-14"
  
  # load all of table query
  table_name = paste0(name, "_joinedBP")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  # print(load_all_query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  # Clear the result
  dbClearResult(rs)
  # Disconnect to clean up the connection to the database.
  dbDisconnect(blastDb)
  data <- loadedData
  
  return(data)
  
}

fullteam_summary <- function(name = "fullteam", env) {
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    unique(data$tm_name)
    
    table <- data %>% select(tm_name, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Rotational Acceleration (g)`, rotation_score,
                             `Time to Contact (sec)`, `Attack Angle (deg)`, ExitSpeed, smash_factor_score, 
                             attack_aa, middle_of_field, good_angle) #%>%
    # filter(!(is.na(`Bat Speed (mph)`)))
    
    
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
    table <- data %>% select(tm_name, `Bat Speed (mph)`, `Peak Hand Speed (mph)`, `Rotational Acceleration (g)`, rotation_score,
                             `Time to Contact (sec)`, `Attack Angle (deg)`, ExitSpeed, smash_factor_score, 
                             attack_aa, middle_of_field, good_angle) #%>%
    # filter(!(is.na(`Bat Speed (mph)`)))
    
  }
  
  # table$ExitSpeed <- round(table$ExitSpeed, 1)
  
  table$ExitSpeed <- as.numeric(table$ExitSpeed)
  table$smash_factor_score = round(table$smash_factor_score, 1)
  table$rotation_score = round(table$rotation_score, 1)
  table$attack_aa = round(table$attack_aa, 2)
  
  team_summary <- table %>% group_by(tm_name) %>% summarise(
    bs = round(mean(`Bat Speed (mph)`, na.rm = T), 1),
    phs = round(mean(`Peak Hand Speed (mph)`, na.rm = T), 1),
    ttc = round(mean(`Time to Contact (sec)`, na.rm = T), 2),
    ra = round(mean(`Rotational Acceleration (g)`, na.rm = T), 1),
    rs = round(mean(rotation_score, na.rm = T), 1),
    aa = round(mean(`Attack Angle (deg)`, na.rm = T), 1),
    attack_aa = round(mean(attack_aa, na.rm = T), 1),
    avg_ope = round(mean(`On Plane Efficiency (%)`, na.rm = T), 1),
    avg_vba = round(mean(`Vertical Bat Angle (deg)`, na.rm = T), 1),
    avg_ec = round(mean(`Early Connection (deg)`, na.rm = T), 1),
    avg_ic = round(mean(`Connection at Impact (deg)`, na.rm = T), 1),
    aev = round(mean(ExitSpeed, na.rm = T), 1),
    smash = round(mean(smash_factor_score, na.rm = T), 1)
  )
  
  # table <- table[, c(1, 2, 3, 4, 16, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 14)]
  
  colnames(team_summary) <- c("Player", "Bat Speed (mph)", 
                              "Hand Speed (mph)", "Time to Contact (sec)", "Rotational Acceleration (g)", 
                              "Rotation Score", "Attack Angle (deg)", 
                              "Attack AA (deg)", "On Plane Efficiency (%)", "Vertical Bat Angle (deg)", "Early Connection (deg)",
                              "Connection at Impact (deg)", "Avg Exit Velo (mph)", "Smash Factor")
  
  
  return(team_summary)
  
}

session.anomaly.table <- function(name, date, env, metric) {
  
  # name = "alvarez"
  # env = 2
  # # metric = "Rotation Score"
  # date = "2022-09-06"
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
  }
  
  data$`Bat Speed Efficiency` = data$`Bat Speed (mph)`/data$`Peak Hand Speed (mph)`
  
  if (env == 1) {
    if (metric == "Bat Speed (mph)") {
      data_metric <- data %>% select(tm_name, tmDate, `Bat Speed (mph)`) %>% mutate(
        `Bat Speed (mph)` = round(`Bat Speed (mph)`, 1)
      )
      
    } else if (metric == "Hand Speed (mph)") {
      data_metric <- data %>% select(tm_name, tmDate, `Peak Hand Speed (mph)`) %>% mutate(
        `Peak Hand Speed (mph)` = round(`Peak Hand Speed (mph)`, 1)
      )
      
    } else if (metric == "Bat Speed Efficiency") {
      data_metric <- data %>% select(tm_name, tmDate, `Bat Speed Efficiency`) %>% mutate(
        `Bat Speed Efficiency` = round(`Bat Speed Efficiency`, 2)
      )
      
    } else if (metric == "Rotational Acceleration (g)") {
      data_metric <- data %>% select(tm_name, tmDate, `Rotational Acceleration (g)`) %>% mutate(
        `Rotational Acceleration (g)` = round(`Rotational Acceleration (g)`, 1)
      )
      
    } else if (metric == "Rotation Score") {
      data_metric <- data %>% select(tm_name, tmDate, rotation_score) %>% mutate(
        rotation_score = round(rotation_score)
      ) %>% rename("Rotation Score" = "rotation_score")
      
    } else if (metric == "Time to Contact (sec)") {
      data_metric <- data %>% select(tm_name, tmDate, `Time to Contact (sec)`) %>% mutate(
        `Time to Contact (sec)` = round(`Time to Contact (sec)`, 2)
      )
      
    } else if (metric == "Attack Angle (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, `Attack Angle (deg)`) %>% mutate(
        `Attack Angle (deg)` = round(`Attack Angle (deg)`, 1)
      )
      
    } else if (metric == "Attack AA (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, attack_aa) %>% mutate(
        attack_aa = round(attack_aa, 1)
      ) %>% rename("Attack AA (deg)" = "attack_aa")
      
    } else if (metric == "Smash Factor") {
      data_metric <- data %>% select(tm_name, tmDate, smash_factor_score) %>% mutate(
        smash_factor_score = round(smash_factor_score)
      ) %>% rename("Smash Factor" = "smash_factor_score")
      
    } else if (metric == "On Plane Efficiency (%)") {
      data_metric <- data %>% select(tm_name, tmDate, `On Plane Efficiency (%)`) %>% mutate(
        `On Plane Efficiency (%)` = round(`On Plane Efficiency (%)`, 1)
      ) 
      
    } else if (metric == "Vertical Bat Angle (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, `Vertical Bat Angle (deg)`) %>% mutate(
        `Vertical Bat Angle (deg)` = round(`Vertical Bat Angle (deg)`)
      ) 
      
    } else if (metric == "Early Connection (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, `Early Connection (deg)`) %>% mutate(
        `Early Connection (deg)` = round(`Early Connection (deg)`)
      ) 
      
    } else if (metric == "Connection at Impact (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, `Connection at Impact (deg)`) %>% mutate(
        `Connection at Impact (deg)` = round(`Connection at Impact (deg)`)
      ) 
      
    }
    
    before <- data_metric %>% filter(tmDate < date) %>% na.omit()
    session <- data_metric %>% filter(tmDate == date) %>% na.omit()
    
    before_avg = mean(before[,3], na.rm = T)
    before_sd = sd(before[,3], na.rm = T)
    
    session$Flag = NA
    session$Swing = seq(1, nrow(session), 1)
    for (i in 1:nrow(session)) {
      session$Flag[i] <- ifelse(session[i,3] > (before_avg + before_sd), "HIGH", 
                                ifelse(session[i,3] < (before_avg - before_sd), "LOW", "NORMAL"))
    }
    
    data_final <- session[, c(5, 3, 4)]
    
  } else if (env == 2) {
    if (metric == "Bat Speed (mph)") {
      data_metric <- data %>% select(tm_name, tmDate, `Bat Speed (mph)`, HitterLink) %>% mutate(
        `Bat Speed (mph)` = round(`Bat Speed (mph)`, 1)
      )
      
    } else if (metric == "Hand Speed (mph)") {
      data_metric <- data %>% select(tm_name, tmDate, `Peak Hand Speed (mph)`, HitterLink) %>% mutate(
        `Peak Hand Speed (mph)` = round(`Peak Hand Speed (mph)`, 1)
      )
      
    } else if (metric == "Bat Speed Efficiency") {
      data_metric <- data %>% select(tm_name, tmDate, `Bat Speed Efficiency`, HitterLink) %>% mutate(
        `Bat Speed Efficiency` = round(`Bat Speed Efficiency`, 2)
      )
      
    } else if (metric == "Rotational Acceleration (g)") {
      data_metric <- data %>% select(tm_name, tmDate, `Rotational Acceleration (g)`, HitterLink) %>% mutate(
        `Rotational Acceleration (g)` = round(`Rotational Acceleration (g)`, 1)
      )
      
    } else if (metric == "Rotation Score") {
      data_metric <- data %>% select(tm_name, tmDate, rotation_score, HitterLink) %>% mutate(
        rotation_score = round(rotation_score)
      ) %>% rename("Rotation Score" = "rotation_score")
      
    } else if (metric == "Time to Contact (sec)") {
      data_metric <- data %>% select(tm_name, tmDate, `Time to Contact (sec)`, HitterLink) %>% mutate(
        `Time to Contact (sec)` = round(`Time to Contact (sec)`, 2)
      )
      
    } else if (metric == "Attack Angle (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, `Attack Angle (deg)`, HitterLink) %>% mutate(
        `Attack Angle (deg)` = round(`Attack Angle (deg)`, 1)
      )
      
    } else if (metric == "Attack AA (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, attack_aa, HitterLink) %>% mutate(
        attack_aa = round(attack_aa, 1)
      ) %>% rename("Attack AA (deg)" = "attack_aa")
      
    } else if (metric == "Smash Factor") {
      data_metric <- data %>% select(tm_name, tmDate, smash_factor_score, HitterLink) %>% mutate(
        smash_factor_score = round(smash_factor_score)
      ) %>% rename("Smash Factor" = "smash_factor_score")
      
    } else if (metric == "On Plane Efficiency (%)") {
      data_metric <- data %>% select(tm_name, tmDate, `On Plane Efficiency (%)`) %>% mutate(
        `On Plane Efficiency (%)` = round(`On Plane Efficiency (%)`, 1)
      ) 
      
    } else if (metric == "Vertical Bat Angle (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, `Vertical Bat Angle (deg)`) %>% mutate(
        `Vertical Bat Angle (deg)` = round(`Vertical Bat Angle (deg)`)
      ) 
      
    } else if (metric == "Early Connection (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, `Early Connection (deg)`) %>% mutate(
        `Early Connection (deg)` = round(`Early Connection (deg)`)
      ) 
      
    } else if (metric == "Connection at Impact (deg)") {
      data_metric <- data %>% select(tm_name, tmDate, `Connection at Impact (deg)`) %>% mutate(
        `Connection at Impact (deg)` = round(`Connection at Impact (deg)`)
      ) 
      
    }
    
    data_metric <- data_metric[-which(is.na(data_metric[,3])),]
    data_metric[is.na(data_metric)] <- ""
    before <- data_metric %>% filter(tmDate < date) 
    session <- data_metric %>% filter(tmDate == date) 
    
    before_avg = mean(before[,3], na.rm = T)
    before_sd = sd(before[,3], na.rm = T)
    
    session$Flag = NA
    session$Swing = seq(1, nrow(session), 1)
    for (i in 1:nrow(session)) {
      session$Flag[i] <- ifelse(session[i,3] > (before_avg + before_sd), "HIGH", 
                                ifelse(session[i,3] < (before_avg - before_sd), "LOW", "NORMAL"))
    }
    
    data_final <- session[, c(6, 3, 5, 4)]
  }
  
  return(data_final)
  
}

trend.graph <- function(name, env, metric, fall) {
  
  # name = "horvath"
  # env = 1
  # metric = "Bat Speed Efficiency"
  # fall = F
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
  }
  
  summary <- data %>% group_by(tmDate) %>% summarise(
    swings_captured = sum(!is.na(`Bat Speed (mph)`)),
    avg_bs = round(mean(`Bat Speed (mph)`, na.rm = T), 1),
    sd_bs = sd(`Bat Speed (mph)`, na.rm = T),
    avg_hs = round(mean(`Peak Hand Speed (mph)`, na.rm = T), 1),
    sd_hs = sd(`Peak Hand Speed (mph)`, na.rm = T),
    avg_bse = round(mean((`Bat Speed (mph)`/`Peak Hand Speed (mph)`), na.rm = T), 2),
    sd_bse = sd((`Bat Speed (mph)`/`Peak Hand Speed (mph)`), na.rm = T),
    avg_ra = round(mean(`Rotational Acceleration (g)`, na.rm = T), 1),
    sd_ra = sd(`Rotational Acceleration (g)`, na.rm = T),
    avg_rs = round(mean(rotation_score, na.rm = T), 1),
    sd_rs = sd(rotation_score, na.rm = T),
    avg_ttc = round(mean(`Time to Contact (sec)`, na.rm = T), 3),
    sd_ttc = sd(`Time to Contact (sec)`, na.rm = T),
    avg_aa = round(mean(`Attack Angle (deg)`, na.rm = T), 1),
    sd_aa = sd(`Attack Angle (deg)`, na.rm = T),
    avg_attack_aa = round(mean(attack_aa, na.rm = T), 1),
    sd_attack_aa = sd(attack_aa, na.rm = T),
    avg_smash = round(mean(smash_factor_score, na.rm = T), 1),
    sd_smash = sd(smash_factor_score, na.rm = T),
    avg_ope = round(mean(`On Plane Efficiency (%)`, na.rm = T), 1),
    sd_ope = sd(`On Plane Efficiency (%)`, na.rm = T),
    avg_vba = round(mean(`Vertical Bat Angle (deg)`, na.rm = T), 1),
    sd_vba = sd(`Vertical Bat Angle (deg)`, na.rm = T),
    avg_ec = round(mean(`Early Connection (deg)`, na.rm = T), 1),
    sd_ec= sd(`Early Connection (deg)`, na.rm = T),
    avg_ic = round(mean(`Connection at Impact (deg)`, na.rm = T), 1),
    sd_ic = sd(`Connection at Impact (deg)`, na.rm = T)
  ) %>% arrange(tmDate) %>% filter(swings_captured > 0)
  
  
  if (metric == "Bat Speed (mph)") {
    summary_metric <- summary %>% select(tmDate, avg_bs, sd_bs)
    
  } else if (metric == "Hand Speed (mph)") {
    summary_metric <- summary %>% select(tmDate, avg_hs, sd_hs)
    
  } else if (metric == "Bat Speed Efficiency") {
    summary_metric <- summary %>% select(tmDate, avg_bse, sd_bse)
    
  } else if (metric == "Rotational Acceleration (g)") {
    summary_metric <- summary %>% select(tmDate, avg_ra, sd_ra)
    
  } else if (metric == "Rotation Score") {
    summary_metric <- summary %>% select(tmDate, avg_rs, sd_rs)
    
  } else if (metric == "Time to Contact (sec)") {
    summary_metric <- summary %>% select(tmDate, avg_ttc, sd_ttc)
    
  } else if (metric == "Attack Angle (deg)") {
    summary_metric <- summary %>% select(tmDate, avg_aa, sd_aa)
    
  } else if (metric == "Attack AA (deg)") {
    summary_metric <- summary %>% select(tmDate, avg_attack_aa, sd_attack_aa)
    
  } else if (metric == "Smash Factor") {
    summary_metric <- summary %>% select(tmDate, avg_smash, sd_smash)
    
  } else if (metric == "On Plane Efficiency (%)") {
    summary_metric <- summary %>% select(tmDate, avg_ope, sd_ope)
    
  } else if (metric == "Vertical Bat Angle (deg)") {
    summary_metric <- summary %>% select(tmDate, avg_vba, sd_vba)
    
  } else if (metric == "Early Connection (deg)") {
    summary_metric <- summary %>% select(tmDate, avg_ec, sd_ec)
    
  } else if (metric == "Connection at Impact (deg)") {
    summary_metric <- summary %>% select(tmDate, avg_ic, sd_ic)
    
  }
  
  if (fall == F) {
    summary_metric <- summary_metric %>% filter(tmDate > "2022-12-31")
  }
  
  plot <- ggplot(summary_metric, aes(x = as.Date(tmDate), y = .data[[names(summary_metric)[2]]])) +
    geom_errorbar(
      aes(ymin = .data[[names(summary_metric)[2]]]-.data[[names(summary_metric)[3]]], 
          ymax = .data[[names(summary_metric)[2]]]+.data[[names(summary_metric)[3]]]), color = "#4B9CD3", size = 1, width = 1
    ) +
    geom_point(size = 10, color = "#13294B") + xlab("Session") + ylab(metric) + geom_line(color = "#13294B", linewidth = 2) +
    ggtitle(paste0("Avg ", metric, " by Session")) + 
    scale_x_date(breaks = as.Date(summary_metric$tmDate), date_labels = "%b %d", guide = guide_axis(n.dodge=2)) + theme_bw(base_size = 20) + 
    theme(legend.title=element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  # plot
  
  return(plot)
  
}

density.graph <- function(name, env, date, metric) {
  
  # name = "horvath"
  # env = 2
  # metric = "Attack Angle (deg)"
  # date = "2023-02-07"
  
  if (env == 1) {
    # load all of table query
    table_name = paste0(name, "_joinedBP")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
  } else {
    # load all of table query
    table_name = paste0(name, "_joinedLive")
    load_all_query <- paste0("SELECT * FROM ", table_name, ";")
    # print(load_all_query)
    blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
    rs = dbSendQuery(blastDb, load_all_query)
    loadedData <- dbFetch(rs)
    # Clear the result
    dbClearResult(rs)
    # Disconnect to clean up the connection to the database.
    dbDisconnect(blastDb)
    
    data <- loadedData
    
  }
  
  data$is_date = factor(ifelse(data$tmDate == date, format(as.Date(date), format = "%b %d"), "Season"))
  data$`Bat Speed Efficiency` <- (data$`Bat Speed (mph)`/data$`Peak Hand Speed (mph)`)
  
  data <- data %>% filter(as.Date(tmDate) <= date)
  
  if (metric == "Bat Speed (mph)") {
    plot <- ggplot(data, aes(x = `Bat Speed (mph)`, fill=is_date)) + geom_density(alpha=0.4) + theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$`Bat Speed (mph)`, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$`Bat Speed (mph)`, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Hand Speed (mph)") {
    plot <- ggplot(data, aes(x = `Peak Hand Speed (mph)`, fill=is_date)) + geom_density(alpha=0.4) + theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$`Peak Hand Speed (mph)`, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$`Peak Hand Speed (mph)`, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Time to Contact (sec)") {
    plot <- ggplot(data, aes(x = `Time to Contact (sec)`, fill=is_date)) + geom_density(alpha=0.4) + theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$`Time to Contact (sec)`, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$`Time to Contact (sec)`, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Rotation Score") {
    plot <- ggplot(data, aes(x = rotation_score, fill=is_date)) + geom_density(alpha=0.4) + theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + xlab("Rotation Score") + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$rotation_score, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$rotation_score, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Attack Angle (deg)") {
    plot <- ggplot(data, aes(x = `Attack Angle (deg)`, fill=is_date)) + geom_density(alpha=0.4) + theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$`Attack Angle (deg)`, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$`Attack Angle (deg)`, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Attack AA (deg)") {
    plot <- ggplot(data, aes(x = attack_aa, fill=is_date)) + geom_density(alpha=0.4) + theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + xlab("Attack AA") + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$attack_aa, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$attack_aa, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "On Plane Efficiency (%)") {
    plot <- ggplot(data, aes(x = `On Plane Efficiency (%)`, fill=is_date)) + geom_density(alpha=0.4)+ theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$`On Plane Efficiency (%)`, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$`On Plane Efficiency (%)`, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Vertical Bat Angle (deg)") {
    plot <- ggplot(data, aes(x = `Vertical Bat Angle (deg)`, fill=is_date)) + geom_density(alpha=0.4)+ theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$`Vertical Bat Angle (deg)`, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$`Vertical Bat Angle (deg)`, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Early Connection (deg)") {
    plot <- ggplot(data, aes(x = `Early Connection (deg)`, fill=is_date)) + geom_density(alpha=0.4)+ theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$`Early Connection (deg)`, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$`Early Connection (deg)`, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Connection at Impact (deg)") {
    plot <- ggplot(data, aes(x = `Connection at Impact (deg)`, fill=is_date)) + geom_density(alpha=0.4)+ theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$`Connection at Impact (deg)`, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$`Connection at Impact (deg)`, na.rm = T), color = "#13294B", linetype = "dashed")
    
  } else if (metric == "Smash Factor") {
    plot <- ggplot(data, aes(x = smash_factor_score, fill=is_date)) + geom_density(alpha=0.4) + theme_bw(base_size = 20) +
      theme(legend.title=element_blank(),
            panel.grid = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank()) +scale_fill_manual(values=c("#4B9CD3", "#13294B")) + xlab("Smash Factor") + geom_vline(xintercept = mean((filter(data, is_date != "Season"))$smash_factor_score, na.rm = T), color = "#4B9CD3", linetype = "dashed") +
      geom_vline(xintercept = mean((filter(data, is_date == "Season"))$smash_factor_score, na.rm = T), color = "#13294B", linetype = "dashed")
    
  }
  
  return(plot)
  
}

get.notes <- function(name) {
  
  table_name = paste0(name, "_notes")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  return(loadedData)
  
}

recent.notes <- function(name) {
  
  df = data.frame(get.notes(name)) %>% arrange(desc(id))
  df %>% select(-id) %>% rename("Swing ID" = "Swing.ID")
  
}

recent.mechanics <- function(name) {
  
  df = data.frame(get.mechanics(name)) %>% arrange(desc(id))
  df %>% select(-id)
  
}

add.swing.note <- function(id, note, name, date) {
  
  # note = "test live"
  # id = 58
  query <- paste0("UPDATE `blast`.`", name, "_joinedBP` SET `Note1` = '", note, "' WHERE (`id` = ", id, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
  query <- paste0("INSERT INTO `", name, "_notes` (`Date`,`Type`,`Swing ID`,`Note`) VALUES ('", date, "','Swing','", id, "','", note, "');")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
  
}

add.session.note <- function(note, name, date) {
  
  query <- paste0("INSERT INTO `", name, "_notes` (`Date`,`Type`,`Note`) VALUES ('", date, "','Session','", note, "');")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
  
}

# add.swing.note(33, "", "blaser", "2022-09-13")

edit.note <- function (id1, id2, name, note) {
  
  query <- paste0("UPDATE `blast`.`", name, "_joinedBP` SET `Note1` = '", note, "' WHERE (`id` = ", id2, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
  query <- paste0("UPDATE `blast`.`", name, "_notes` SET `Note` = '", note, "' WHERE (`id` = ", id1, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}

delete.note <- function (id1, id2, name) {
  
  table_name = paste0(name, "_notes")
  query <- paste0("DELETE FROM `", table_name, "` WHERE `id` = ", id1, ";")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
  query <- paste0("UPDATE `blast`.`", name, "_joinedBP` SET `Note1` = NULL WHERE (`id` = ", id2, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}

# delete.note(10, "alvarez")

log.mechanics <- function(name, date, desc, reas) {
  
  query <- paste0("INSERT INTO `", name, "_mechanics` (`Date`,`Description`,`Reasoning`) VALUES ('", date, "','", desc, "','", reas, "');")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}

get.mechanics <- function(name) {
  
  table_name = paste0(name, "_mechanics")
  load_all_query <- paste0("SELECT * FROM ", table_name, ";")
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, load_all_query)
  loadedData <- dbFetch(rs)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  return(loadedData)
  
}

edit.mechanics <- function (id, name, desc, reas) {
  
  query <- paste0("UPDATE `blast`.`", name, "_mechanics` SET `Description` = '", desc, "' WHERE (`id` = ", id, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
  query <- paste0("UPDATE `blast`.`", name, "_mechanics` SET `Reasoning` = '", reas, "' WHERE (`id` = ", id, ");")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}

delete.mechanics <- function (id, name) {
  
  table_name = paste0(name, "_mechanics")
  query <- paste0("DELETE FROM `", table_name, "` WHERE `id` = ", id, ";")
  # print(query)
  blastDb <- dbConnect(RMariaDB::MariaDB(), user='admin', password=userpassword, dbname='blast', host='database-1.crwft7fm4u00.us-east-1.rds.amazonaws.com')
  rs = dbSendQuery(blastDb, query)
  dbClearResult(rs)
  dbDisconnect(blastDb)
  
}


