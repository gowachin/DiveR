#### Function for shiny ####
library(mn90)
library(shiny.i18n)
source("init.R")

# summarise function, modified from package one
summarise_dive <- function(dive) {
  sup <- dive$palier$time > 0
  n <- sum(sup)
  diz <- sum(dive$palier$time[sup] > 9)
  sp <- c(rep("  ", n - diz), rep(" ", diz))
  
  if(dive$palier$group != "Z"){
    group <- paste0(i18n$t('The dive group is '), dive$palier$group)
  } else {
    group <- ''
  }
  
  ret <- renderText({
    paste0(
      i18n$t("The first dive reach "), depth(dive),
      i18n$t(" meters for a duration of "), dtime(dive), " minutes.\n",
      i18n$t("Total dive time is "), diff(dive$hour),
      i18n$t(" minutes with an ascent of "), dive$dtr, " minutes.\n",
      sum(dive$palier$time > 0), i18n$t(" stops :"),
      paste(sprintf(
        i18n$t("%s%d minutes at %d meters"), sp,
        dive$palier$time[sup],
        dive$palier$depth[sup]
      ),
      collapse = "\n         "
      ), '\n', group
    )
    
  })
  return(ret)
}