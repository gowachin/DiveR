#' @import stats
NULL

#' tank
#'
#' Creation of tank object for usage in consumption part of a dive.
#'
#' @param vol tank volume in litre.
#' @param press tank pression in bar.
#' @param rules tank rules to watch during a dive. A list of two named element :
#' \describe{
#'   \item{"rules"}{vector of 2 named integer indicating a percentage
#'   or a pression. The names will be used in the plot function later}
#'   \item{"sys"}{character string, either '%' or 'bar'. Percentage must be
#'   between 0 and 100.}
#' }
#' @param gas tank gas, by default "Air". Parameter is here for future dev.
#' @param typ tank type, by default "back"
#' \describe{
#'   \item{"solo"}{single tank}
#'   \item{"relay"}{single tank to be dropped at certain time}
#' }
#' @param limit a two element vector with times between which the tank
#' is not used. Can be used to mimic an accident, or a relay tank.
#' @param name Possibility to name the tank for better understanding after.
#' by default will be named after the typ and volume.
#'
#' @details
#' To set a relay tank, rule1 and rule2 must be the same. Therefore the tank
#' won't be usable once pressure reach rule2 and until all other tanks are
#' not used. If multiple tanks are used, the relay must be the first one in order
#'
#' @export
tank <- function(vol, press, rules = list(
                   rules = c("mid" = 50, "res" = 25),
                   sys = "%"
                 ),
                 gas = c("Air"), typ = c("back", "relay"),
                 limit = NULL, name = NULL) {

  #### IDIOT PROOF ####
  # vol and press
  if (all(vol <= 0) | !is.numeric(vol) | length(vol) > 1) {
    stop("vol must be a single positive numeric value.")
  }
  if (all(press < 0) | !is.numeric(press) | length(press) > 1) {
    stop("press must be a single positive, 0 possible, numeric value.")
  }
  # rules
  if (length(rules) != 2 | any(names(rules) != c("rules", "sys"))) {
    stop(paste(
      "rules must be a list of length 2 with a vector of 2 numeric",
      "named rules and a single character string being % or bar"
    ))
  }
  rules$sys <- match.arg(rules$sys, c("%", "bar"))
  if (!is.numeric(rules$rules) | length(rules$rules) != 2) {
    stop("Element rules of rules argument must be a vector of 2 numeric")
  }
  for (i in 1:length(rules$rules)) {
    if (rules$rules[i] < 0) {
      warning("negative rules are not possible and therefor set to 0")
      rules$rules[i] <- 0
    }
  }
  if (is.null(names(rules$rules))) {
    names(rules$rules) <- c("", "")
    warning("There was no names for rules, consider setting them for later use")
  }
  # gas
  gas <- match.arg(gas)
  # stop("Only air is working at this moment")
  typ <- match.arg(typ)

  # TODO : limit vector of 2 positive numeric, negative value trigger warning !
  #### function ####
  # modify rules to bar !
  if (rules$sys == "%") {
    for (i in 1:length(rules$rules)) {
      if (rules$rules[i] > 100) {
        warning(paste(
          "The rule is superior to 100 %",
          "Therefore it is changed to the maximum pression"
        ))
        rules$rules[i] <- 100
      }
    }
    rules$rules <- press * rules$rules / 100
  } else if (rules$sys == "bar") {
    for (i in 1:length(rules$rules)) {
      if (rules$rules[i] > press) {
        warning(paste(
          "The rule is superior to the pression in the tank.",
          "Therefore it is changed to the maximum pression"
        ))
        rules$rules[i] <- press
      }
    }
  }
  # limit in time
  # TODO : maybe remove this as it's is poorly defined
  if (is.null(limit)) {
    limit <- rep(NA, 2)
  } else {
    limit[limit < 0] <- 0
    limit <- sort(limit)
  }
  # name
  if (is.null(name)) {
    name <- paste0(typ, vol)
  }
  # numeric vector
  carac <- c(vol, press, unlist(rules$rules))
  names(carac) <- c("vol", "press", "rule1", "rule2")
  # string vector
  typo <- c(gas, typ, names(rules$rules), name)
  names(typo) <- c("gas", "typ", "rule1", "rule2", "name")

  if (gas != "Air") {
    # TODO : imput other gas
  } else {
    ppo2 <- c(0.21, 1.6)
    dmin <- (ppo2[1] * 70 / 1.47) - 10 # assimilé a ppo2 > 0.18
    dmax <- (ppo2[2] * 70 / 1.47) - 10 # assimilé a ppo2 < 1.6
    # round them
    dmin <- ceiling(dmin)
    dmax <- floor(dmax)
  }

  limit <- c(dmin, dmax, limit)
  names(limit) <- c("mind", "maxd", "t1", "t2")

  tank <- list(carac = carac, typo = typo, limit = limit)
  class(tank) <- "tank"
  return(tank)
}


#' expand
#'
#' @param tank \code{\link[DiveR]{tank}} object or a list of tank objects.
#' Priority of consumption for tanks is set by their order in list.
#' @param dive \code{\link[DiveR]{dive}} object
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
expand <- function(tank, dive) {
  
  if(class(tank) != 'tank' & !(
    class(tank) == "list" & all(unique(unlist(lapply(tank, class))) == "tank")
    )){
    stop('tank must be a single tank object or a list of tanks')
  }
  
  if(class(dive) != 'dive'){
    stop('dive must to be a dive object')
  }
  
  dtime <- max(dive$dtcurve$times)
  depth <- depth(dive)

  #### Single tank ####
  if (class(tank) == "tank") {
    # init table
    table <- data.frame(
      min_depth = tank$limit["mind"],
      max_depth = tank$limit["maxd"],
      begin = 0, end = dtime,
      type = tank$typo["typ"],
      press = tank$carac["press"],
      vol = tank$carac["vol"]
    )

    # duplicate tank for different step in pression following rules
    table$press <- (table[, 6] - tank$carac["rule1"])

    table <- cbind(
      table, table[, 5],
      (tank$carac["rule1"] - tank$carac["rule2"]), table[, 7]
    )

    colnames(table) <- c(
      colnames(table[-c((ncol(table) - 2):ncol(table))]),
      paste0("rul1", colnames(table)[5:7])
    )

    table <- cbind(table, table[, 5], (tank$carac["rule2"]), table[, 7])

    colnames(table) <- c(
      colnames(table[-c((ncol(table) - 2):ncol(table))]),
      paste0("rul2", colnames(table)[5:7])
    )

    # duplicate row to add time when it's not allowed
    if (!all(is.na(tank$limit[c("t1", "t2")]))) {
      for (i in c("t1", "t2")) {
        if (tank$limit[i] >= 0 & tank$limit[i] < dtime) {
          table <- rbind(table, table[1, ])
          table$end[nrow(table)] <- table$end[nrow(table) - 1]
          table$begin[nrow(table)] <- table$end[nrow(table) - 1] <- tank$limit[i]

          if (i == "t1") {
            table$press[nrow(table)] <- 0
            table$rul1press[nrow(table)] <- 0
            table$rul2press[nrow(table)] <- 0
          }
        }
      }
    }
    # duplicate table for allowed depths or not.
    if ((tank$limit["mind"]) > 0) { # minimum depth
      min_table <- table
      min_table$press <- 0
      min_table$rul1press <- 0
      min_table$rul2press <- 0
      min_table$min_depth <- 0
      min_table$max_depth <- tank$limit["mind"]
    } else {
      min_table <- NULL
    }

    if ((tank$limit["maxd"]) < depth) { # maximum depth
      max_table <- table
      max_table$press <- 0
      max_table$rul1press <- 0
      max_table$rul2press <- 0
      max_table$min_depth <- tank$limit["maxd"]
      max_table$max_depth <- depth
    } else {
      max_table <- NULL
      table$max_depth <- depth
    }

    table <- rbind(min_table, table, max_table)
    table

    #### list of tank ####
  } else if (class(tank) == "list" &
    unique(unlist(lapply(tank, class))) == "tank") {
    # create a table per tank
    tank_list <- lapply(tank, expand, dive)
    name <- lapply(lapply(tank, "[[", 2), "[", 5)
    tmp <- do.call("rbind", tank_list) # bind the tank list for unique values

    mindv <- sort(unique(tmp$min_depth))
    maxdv <- sort(unique(tmp$max_depth))
    t1v <- sort(unique(tmp$begin))
    t2v <- sort(unique(tmp$end))

    tmpa <- expand.grid(mindv, t1v)
    tmpb <- expand.grid(maxdv, t2v)
    # table of combinaisons
    table <- cbind(tmpa[, 1], tmpb[, 1], tmpa[, 2], tmpb[, 2])
    # init empty table of possibility
    table <- cbind(table, as.data.frame(matrix(0,
      nrow = nrow(table),
      ncol = 9 * length(tank)
    )))
    colnames(table) <- c(
      colnames(tank_list[[1]])[1:4],
      unlist(lapply(name, paste0, c("", "_press", "_vol")))
    )

    for (i in c(1:length(tank))) {
      # possible depths for tank
      possib_depth <- table$min_depth >= tank[[i]]$limit["mind"] &
        table$max_depth <= tank[[i]]$limit["maxd"]
      # possible time for tank
      if (!all(is.na(tank[[i]]$limit[c("t1", "t2")]))) {
        possib_time <- table$begin < tank[[i]]$limit["t1"] |
          table$end > tank[[i]]$limit["t2"]
      } else {
        possib_time <- TRUE
      }
      # set the typ in column
      table[, 5 + 3 * (i - 1)] <- tank[[i]]$typo["typ"]
      table[, 5 + 3 * (i - 1) + 3 * length(tank)] <- tank[[i]]$typo["typ"]
      table[, 5 + 3 * (i - 1) + 6 * length(tank)] <- tank[[i]]$typo["typ"]
      # put pression for possible usages
      table[possib_depth & possib_time, 6 + 3 * (i - 1)] <- tank[[i]]$carac["press"] -
        tank[[i]]$carac["rule1"]
      table[, 7 + 3 * (i - 1)] <- tank[[i]]$carac["vol"]
      table[, 7 + 3 * (i - 1) + 3 * length(tank)] <- tank[[i]]$carac["vol"]
      table[, 7 + 3 * (i - 1) + 6 * length(tank)] <- tank[[i]]$carac["vol"]
      # ajouter les rule1 et rule2
      table[
        possib_depth & possib_time,
        6 + 3 * (i - 1) + 3 * length(tank)
      ] <- (tank[[i]]$carac["rule1"] -
        tank[[i]]$carac["rule2"])
      table[
        possib_depth & possib_time,
        6 + 3 * (i - 1) + 6 * length(tank)
      ] <- tank[[i]]$carac["rule2"]
    }
  }
  # trim same time rows,
  table <- table[!table$begin == table$end, ]
  # trim rows with FALSE ??
  return(table)
}

#' conso
#'
#' @param dive \code{\link[DiveR]{dive}} object
#' @param tank \code{\link[DiveR]{tank}} object or a list of tank objects.
#' Priority of consumption for tanks is set by their order in list.
#' @param cons Litre per minute breathed by diver. Single numeric positive value.
#' 20 L/min by default
#' @param failure_label Label for when a tank is a empty. Single character
#' string. 'AF' by default.
#'
#' @return conso, a conso class object.
#'
#' @author Jaunatre Maxime <maxime.jaunatre@yahoo.fr>
#'
#' @export
conso <- function(dive, tank, cons = 20, failure_label = "AF") {

  #  TODO : add possible accident here later one ?

  #### IDIOT PROOF ####
  if (all(cons <= 0) | !is.numeric(cons) | length(cons) > 1) {
    stop("cons must be a single positive numeric value.")
  }
  # set values to limit computations
  if (class(tank) == "tank") {
    Ltank <- 1
  } else {
    Ltank <- length(tank)
  }

  #### Cut the dive in parts ####
  # expand the tanks availability
  table <- expand(tank, dive)
  # extract points to cut dive in time and depths
  times <- unique(c(table$begin, table$end)) # unique(unlist(table[,c(3,4)]))
  from_times <- data.frame(times = times, depths = times)
  for (i in 1:nrow(from_times)) {
    from_times$depths[i] <- depth_at_time(dive, from_times$depths[i])
  }
  rm(times, i) # to remove later
  depths <- as.list(unique(unlist(table[, 1:2])))
  for (i in 1:length(depths)) {
    tmp <- time_at_depth(dive, depths[[i]])
    depths[[i]] <- data.frame(
      times = tmp,
      depths = rep(depths[[i]], length(tmp))
    )
  }
  from_depths <- do.call(rbind, depths)
  rm(i, tmp, depths) # to remove later
  # join dfs and sort unique with time order.
  point <- unique(rbind(from_depths, from_times))
  point <- point[order(point$times), ]
  rm(from_depths, from_times)
  # remove points with same time and keep first one (vertical motion)
  point <- point[!duplicated(point$times), ]
  # adding points to dtcurve for cutting
  dtcurve <- unique(rbind(data.frame(
    times = dive$dtcurve$times,
    depths = dive$dtcurve$depths
  ), point))
  dtcurve <- dtcurve[order(dtcurve$times), ]
  dtcurve$pressure <- dtcurve$depths / 10 + 1
  rm(point)

  #### Loop in cuted dive ####
  l <- nrow(dtcurve) - 1

  # init a list of length l
  lcons <- vector(mode = "list", length = l)
  AIR_FAIL <- FALSE
  press_cols <- 6 + 3 * (c(1:(Ltank * 3)) - 1)
  init_press <- apply(table[, press_cols], 2, max)

  init_vols <- apply(table[, (press_cols + 1)], 2, max)
  i <- 1
  lcons[[1]] <- c(0, 0, init_press)
  # TODO : modify loop so lcons is df and not list
  while (i <= l) {
    #### get tank availables for this cut ####
    t1 <- dtcurve$times[i]
    t2 <- dtcurve$times[i + 1]
    if (t1 == t2) {
      if (i == 1) {
        tmp_press <- init_press
      }
      i <- i + 1
      next
    }
    tmpdtcurve <- dtcurve[c(i, i + 1), ]
    d1 <- min(tmpdtcurve$depths)
    d2 <- max(tmpdtcurve$depths)
    tankpres <- table[(table$min_depth <= d1 & table$max_depth >= d2 &
      table$begin < t2 & table$end > t1), ]
    rm(d1, d2)

    if (nrow(tankpres) < 1) {
      # case of vertical motion (like square dive first point)
      tankpres <- table[(table$begin < t2 & table$end > t1), ]
      # tank available at all depths and first pressure > 0
      tankpres[, press_cols[
        !((tankpres[1, press_cols] > 0) &
          (colSums(tankpres[, press_cols] > 0) == nrow(tankpres)))
      ]] <- 0
      tankpres <- tankpres[1, ]

      if (nrow(tankpres) < 1) {
        # TODO : merge this with the other air failure later.
        # AIR FAILURE HERE /!\
        warning(paste(
          "No tank is available between", t1, "and", t2,
          "minutes so you died. Try again !"
        ))
        AIR_FAIL <- TRUE
        break
      }
    }
    #### NEXT on empty tanks !  ####
    if (sum(tankpres[, press_cols]) == 0) {
      # cat('no tank available\n')
      warning(paste(
        "No tank is available between", t1, "and", t2,
        "minutes so you died. Try again !"
      ))
      AIR_FAIL <- TRUE
      lcons[[i]] <- as.data.frame(matrix(0, nrow = 2, ncol = 2 + (Ltank * 3)))
      lcons[[i]][, 2] <- c(t1, t2)

      if (i == 1) {
        tmp_press <- init_press
      }
      lcons[[i]][2, -c(1, 2)] <- tmp_press
      na_press <- tmp_press # assure tmp_press is still available if 2nd no tank
      na_press[tankpres[press_cols] == 0] <- NA
      lcons[[i]][1, -c(1, 2)] <- na_press
      colnames(lcons[[i]]) <- c("vcons", "time", as.character(1:(Ltank * 3)))
      i <- i + 1
      rm(na_press)
      next
    }
    #### compute consumption here ####
    # init table of cons and press
    lcons[[i]] <- as.data.frame(matrix(0, nrow = 1, ncol = 2 + (Ltank * 3)))
    colnames(lcons[[i]]) <- c("vcons", "time", as.character(1:(Ltank * 3)))
    # trapeze method to compute conso
    lcons[[i]][1, 1] <- tmp_conso <-
      cons * (tmpdtcurve$pressure[1] + tmpdtcurve$pressure[2]) *
        (tmpdtcurve$times[2] - tmpdtcurve$times[1]) / 2
    lcons[[i]][1, 2] <- tmpdtcurve$times[2]
    # compute pression in every tank
    tmp_press <- unlist(tankpres[press_cols])
    tmp_press[tankpres[press_cols] == 0] <- NA
    #### empty tanks in loop ####
    for (ii in 1:(Ltank * 3)) {
      if (is.na(tmp_press[ii])) next
      if (tmp_press[ii] == 0) next
      # pass if the prev tank not used
      if (tmp_conso - tmp_press[ii] * init_vols[ii] <= 0 | tmp_conso < 1e-4) {
        tmp_press[ii] <- tmp_press[ii] - tmp_conso / init_vols[ii]
        tmp_conso <- 0 # no more need to breath
        break
      } else {
        # still need to breath, aka tank is not enough
        neg_press <- tmp_press[ii] - tmp_conso / init_vols[ii]
        # add a new time for this
        reg <- lm(c(tmp_press[ii], neg_press) ~
        c(tmpdtcurve$times[1], tmpdtcurve$times[2]))
        neg_time <- (0 - reg$coefficients[1]) / reg$coefficients[2]
        # duplicate line in dtcurve
        dtcurve <- rbind(
          dtcurve[1:i, ], dtcurve[i, ],
          dtcurve[-c(1:i), ]
        )
        l <- l + 1
        # modify time
        lcons[[i]][1, 2] <- dtcurve[i + 1, 1] <- neg_time
        lcons <- c(lcons, "NULL")

        tmp_press[tankpres[press_cols] == 0] <- NA
        tmp_press[ii] <- 0 # so finally this tank is empty
        rm(neg_press, reg, neg_time)
        break # because next consumption is for next row
      }
    }
    lcons[[i]][1, -c(1, 2)] <- tmp_press
    # modify the air availability
    for (ii in 1:(Ltank * 3)) {
      tmp_col <- press_cols[ii]
      tmp <- max(table[table[, tmp_col] >= 0, tmp_col])
      newpress <- min(lcons[[i]][, 2 + ii])
      if (tmp == 0 | is.na(newpress)) next # in case tank is empty
      
      table[table[, tmp_col] == tmp, tmp_col] <- newpress
    }
    rm(tmp_col, tmp, newpress)
    i <- i + 1
  }
  rm(i, l, ii, t1, t2, tmp_conso, tmpdtcurve, tmp_press)
  rm(init_press, init_vols, cons, tankpres)

  vcons <- do.call(rbind, lcons)
  vcons <- as.data.frame(apply(vcons, 2, round, 2))

  if (class(tank) == "tank") {
    rules <- data.frame(
      rule1 = tank$carac["rule1"], name1 = tank$typo["rule1"], temps1 = NA,
      rule2 = tank$carac["rule2"], name2 = tank$typo["rule2"], temps2 = NA,
      empty = 0, nameE = failure_label, tempsE = NA
    )
    rownames(rules) <- tank$typo["name"]
  } else {
    rules <- data.frame(
      rule1 = unlist(lapply(lapply(tank, "[[", 1), "[", 3)),
      name1 = unlist(lapply(lapply(tank, "[[", 2), "[", 3)),
      temps1 = rep(NA, Ltank),
      rule2 = unlist(lapply(lapply(tank, "[[", 1), "[", 4)),
      name2 = unlist(lapply(lapply(tank, "[[", 2), "[", 4)),
      temps2 = rep(NA, Ltank),
      Empty = rep(0, Ltank),
      nameE = rep(failure_label, Ltank),
      tempsE = rep(NA, Ltank)
    )
    rownames(rules) <- unlist(lapply(lapply(tank, "[[", 2), "[", 5))
  } # check for list of tank or single tank is made in expand

  # simplify vcons
  for (i in 3:(Ltank + 2)) {
    # add the columns of same tank
    tmp <- vcons[, c(i, i + Ltank, i + 2 * Ltank)]
    empty_row <- apply(is.na(tmp), 1, all)
    tmp <- rowSums(tmp, na.rm = T)
    if (any(empty_row)) { # once tank empty, he is NA
      tmp[which(empty_row)[1]:length(tmp)] <- NA
      rules[i - 2, 9] <- vcons$time[which(empty_row)[1] - 1]
    }
    l <- length(tmp)
    for (ii in 1:l) { # correction for NA values when absent tank
      if (ii > 2) {
        if (is.na(tmp[ii - 2]) & is.na(tmp[ii - 1]) & !is.na(tmp[ii])) {
          tmp[ii - 1] <- min(tmp[1:(ii - 1)], na.rm = TRUE)
        }
      }
      if (!is.na(tmp[ii])) { # complete times for rules pressure.
        if (tmp[ii] == rules[i - 2, 1] & is.na(rules[i - 2, 3])) {
          rules[i - 2, 3] <- vcons[ii, 2]
        } # rule 1
        if (tmp[ii] == rules[i - 2, 4] & is.na(rules[i - 2, 6])) {
          rules[i - 2, 6] <- vcons[ii, 2]
        } # rule 2
      }
    }
    vcons[, i] <- tmp
  }
  rm(l, tmp, i, ii)
  vcons <- vcons[, 1:(2 + Ltank)]
  colnames(vcons)[2] <- "times"
  colnames(vcons)[-c(1:2)] <- rownames(rules)

  conso <- list(
    vcons = vcons, rules = rules,
    dtcurve = dtcurve, hour = dive$hour
  )
  class(conso) <- "conso"

  return(conso)
}