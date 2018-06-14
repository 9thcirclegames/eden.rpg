genesi <- function(veil, pillar, malkuth, daat, dice.size){
  
  success <- c("asiyah" = dice.size - 1, "yetzirah" = dice.size - 2,"aziluth" = dice.size - 3)
  pillars <- c("strong" = 1, "medium" = 0,"weak" = -1)
  dice <- c("asiyah" = 4, "yetzirah" = 3, "aziluth" = 3)
  
  log.message <- "[%s] [d%d] *** Push a %s %s dice to %s using %d da'at dice"
  
  asiyah.throw <- as.integer(runif(ifelse(veil == "asiyah", dice["asiyah"] + pillars[pillar], ifelse(veil == "yetzirah", 1, 0)), 1, dice.size + 1))
  yetzirah.throw <- as.integer(runif(ifelse(veil == "yetzirah", dice["yetzirah"] + pillars[pillar], 1), 1, dice.size + 1))
  aziluth.throw <- as.integer(runif(ifelse(veil == "aziluth", dice["aziluth"] + pillars[pillar], ifelse(veil == "yetzirah", 1, 0)), 1, dice.size + 1))
  
  # The idea is to start from the nearest number to success and try to push it to nine using the lowest, then pass to others in
  # decreasing order
  
  # Ignore daat on lateral pillars
  if(pillar != "medium") daat <- 0
  
  throws.distance <- data.frame("veil" = factor(c(rep("asiyah", length(asiyah.throw)),
                                                  rep("yetzirah", length(yetzirah.throw)),
                                                  rep("aziluth", length(aziluth.throw))), level = c("asiyah", "yetzirah", "aziluth")),
                                throw = c(asiyah.throw, yetzirah.throw, aziluth.throw),
                                # revelation.distance = c(success["asiyah"] - asiyah.throw + 2,
                                #                         success["yetzirah"] - yetzirah.throw + 2,
                                #                         success["aziluth"]  - aziluth.throw + 2),
                                # sefiroth.distance = c(success["asiyah"] - asiyah.throw + 1,
                                #                       success["yetzirah"]  - yetzirah.throw + 1,
                                #                       success["aziluth"] - aziluth.throw + 1),
                                revelation.distance = c(success["asiyah"] - asiyah.throw + 2,
                                                        success["asiyah"] - yetzirah.throw + 2,
                                                        success["asiyah"]  - aziluth.throw + 2),
                                sefiroth.distance = c(success["asiyah"] - asiyah.throw + 1,
                                                      success["asiyah"]  - yetzirah.throw + 1,
                                                      success["asiyah"] - aziluth.throw + 1),
                                success.distance = c(success["asiyah"]  - asiyah.throw,
                                                     success["yetzirah"] - yetzirah.throw,
                                                     success["aziluth"] - aziluth.throw),
                                stringsAsFactors = FALSE) %>% 
    mutate(revelation.distance = ifelse(revelation.distance < 0, 0, revelation.distance)) %>%
    mutate(sefiroth.distance = ifelse(sefiroth.distance < 0, 0, sefiroth.distance)) %>%
    mutate(success.distance = ifelse(success.distance < 0, 0, success.distance)) %>%
    filter(revelation.distance == 0 | sefiroth.distance == 0 | success.distance == 0 | throw <= daat) %>%
    arrange(revelation.distance, sefiroth.distance, success.distance, desc(veil))

  flog.debug(sprintf("[%s] [d%d] [daat=%d] [malkuth=%d] [%s] %s", veil, dice.size, daat, malkuth, pillar, paste("asiayah: ", paste(asiyah.throw, collapse = " "),
                                                                                                                      "; yetzirah: ", paste(yetzirah.throw, collapse = " "),
                                                                                                                      "; aziluth: ", paste(aziluth.throw, collapse = " "), sep = "")))
  if(nrow(throws.distance) > 0){
  
  apply(throws.distance, 1, function(c.row){
    
    flog.trace(sprintf("[%s] [d%d] [type=%s] throw=%s\trevelation.distance=%s\tsefiroth.distance=%s\tsuccess.distance=%s",
                       veil,
                       dice.size,
                       c.row['veil'],
                       c.row['throw'],
                       c.row['revelation.distance'],
                       c.row['sefiroth.distance'],
                       c.row['success.distance']))
  })}

  inc.success <- function(td){
 
    if(nrow(td) == 0) return(NULL)
    
    throw.success <- data.frame()

    if(!is.na(td[1, "revelation.distance"]) & td[1, "revelation.distance"] <= (nrow(td[2:nrow(td),] %>% filter(throw <= daat)))){
      flog.debug(sprintf(log.message, veil, dice.size, pillar, td[1,"veil"], "revelation", td[1, "revelation.distance"], dice.size))
      throw.success <- rbind(td[1,] %>%
                               select("veil") %>%
                               mutate(revelation = 1, sefiroth = 0, success = 0),
                             inc.success((function(x){
                               if(nrow(x) - x[1, "revelation.distance"] > 1) return(x[2:(nrow(x) - x[1, "revelation.distance"]),])
                               else return(x[0,])
                             })(td))
      )
    } else if(!is.na(td[1, "sefiroth.distance"]) & td[1, "sefiroth.distance"] <= (nrow(td[2:nrow(td),] %>% filter(throw <= daat)))){
      flog.debug(sprintf(log.message, veil, dice.size, pillar, td[1,"veil"], "sefiroth", td[1, "sefiroth.distance"]))
      throw.success <- rbind(td[1,] %>%
                               select("veil") %>%
                               mutate(revelation = 0, sefiroth = 1, success = 0),
                             inc.success((function(x){
                               if(nrow(x) - x[1, "sefiroth.distance"] > 1) return(x[2:(nrow(x) - x[1, "sefiroth.distance"]),])
                               else return(x[0,])
                             })(td))
      )
    } else if(!is.na(td[1, "success.distance"]) & td[1, "success.distance"] <= (nrow(td[2:nrow(td),] %>% filter(throw <= daat)))){
      flog.debug(sprintf(log.message, veil, dice.size, pillar, td[1,"veil"], "success", td[1, "success.distance"]))
      throw.success <- rbind(td[1,] %>%
                               select("veil") %>%
                               mutate(revelation = 0, sefiroth = 0, success = 1),
                             inc.success((function(x){
                               if(nrow(x) - x[1, "success.distance"] > 1) return(x[2:(nrow(x) - x[1, "success.distance"]),])
                               else return(x[0,])
                             })(td))
      )
    } else { 
      
      throw.success <- inc.success((function(x){
        if(nrow(x) >= 2) return(x[2:nrow(x),])
        else return(x[0,])
      })(td))
      
      }
   
    return(throw.success)
    
  } 
  
  success.table <- inc.success(throws.distance) 
  
  if(is.null(success.table)) return(list("revelation" = 0, "sefiroth" = 0, "success" = 0))
  
  return(as.list(success.table %>% select(-veil) %>% summarize_all(sum)))
  
}

