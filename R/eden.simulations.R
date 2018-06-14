######################
# REQUIREMENTS       #
######################
if (!require("pacman")) install.packages("pacman"); invisible(library(pacman))
tryCatch({
  p_load("MonteCarlo", "dplyr", "futile.logger", "ggplot2", "scales", "snow", "purrrlyr")
}, warning=function(w){
  stop(conditionMessage(w))
})

########
# Setup
#
sapply(file.path("R", c("genesi.R")),
       source,
       .GlobalEnv)

flog.threshold(INFO)

#flog.appender(appender.file('eden.sim.log'))
flog.appender(appender.console())

# Example without parallization
veil_grid <- c("asiyah", "yetzirah", "aziluth")
pillar_grid <- c("strong", "medium", "weak")
malkuth_grid <- c(0)
daat_grid <- 0:7
dice_grid <- c(6, 8, 10, 12)

param_list = list("veil" = veil_grid,
                "pillar" = pillar_grid,
                "malkuth" = malkuth_grid,
                "daat" = daat_grid,
                "dice.size" = dice_grid)

# param_list = list("veil" = c("asiyah"),
#                 "pillar" = pillar_grid,
#                 "malkuth" = 0,
#                 "daat" = 0:1,
#                 "dice.size" = 8)

eden.erg <- MonteCarlo(func = genesi,
                  nrep = 100,
                  param_list = param_list,
                  ncpus = 1)


eden.sim <- MakeFrame(output = eden.erg)

eden.sim$pillar <- gsub("pillar=", "", eden.sim$pillar)
eden.sim$veil <- gsub("veil=", "", eden.sim$veil)

eden.sim$total.success <- eden.sim$revelation + eden.sim$sefiroth + eden.sim$success
eden.sim$atleast.success <- (eden.sim$revelation > 0) | (eden.sim$sefiroth > 0) | (eden.sim$success > 0)

eden.sim$veil <- factor(eden.sim$veil, levels = c("aziluth", "yetzirah", "asiyah"))
eden.sim$pillar <- factor(eden.sim$pillar, levels = c("strong", "medium", "weak"))


##########
# SUM UP PLOTS
##
test.dice <- 8
max.daat <- 7
ggplot(data=eden.sim %>% filter(dice.size == test.dice & daat <= max.daat), aes(x = total.success)) + 
  geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..], fill=daat<3), colour="grey40", alpha=0.2) +
  facet_grid(veil ~ pillar) +
  labs(title=sprintf("Percentuale di successi (Da'at < %d, d%d)", max.daat + 1, test.dice)) +
  scale_y_continuous(labels=percent) +
  labs(x="Successi [Nr]", y="Risultati su tiri [%]")


eden.density <- ggplot(data=eden.sim %>% filter(dice.size == 8 & daat <= max.daat), aes(x = total.success)) + 
  geom_density(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), color="grey40", fill="grey40", alpha=0.2, show.legend = FALSE) +
  facet_grid(veil ~ pillar) +
  labs(title=sprintf("Densità di successi (Da'at < %d, d%d)", max.daat + 1, test.dice)) +
  labs(x="Successi [dens]", y="Risultati su tiri [%]")

eden.density %+%
  geom_density(data=eden.sim %>% filter(dice.size == 8 & daat <= max.daat & revelation > 0), aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), , colour="red") %+%
  geom_density(data=eden.sim %>% filter(dice.size == 8 & daat <= max.daat & sefiroth > 0), aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), , colour="green") %+%
  geom_density(data=eden.sim %>% filter(dice.size == 8 & daat <= max.daat & success > 0), aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), , colour="blue")
