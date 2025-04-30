#Calculate Colwells P for a given set of breaks
calc_colwells_p <- function(data, breaks){
  reformat <- data %>%
    mutate(Date = format(datetime, "%d/%m/%Y")) %>%
    ungroup() %>%
    rename(Q = observation) %>%
    select(Date, Q) %>%
    data.frame() %>%
    ts.format()
  
  p <- colwells_asl(reformat, breaks = breaks, indices.only = T)
  return(p)
}

#Modified Colwells function
colwells_asl <- function (flow.ts, fn = "mean", breaks = NULL,
                          base.binning = 2, base.entropy = 2, 
                          indices.only = FALSE) {
  s = length(breaks + 1)
  fn <- match.fun(fn)
  flow.ts$month <- factor(strftime(flow.ts[, "Date"], format = "%m"))
  flow.ts$year <- factor(strftime(flow.ts[, "Date"], format = "%Y"))
  flow.ts.monthly <- aggregate(Q ~ month + year, flow.ts, fn, 
                               na.rm = TRUE)
  flow.ts.monthly$flow.class <- cut(flow.ts.monthly$Q, breaks = breaks, 
                                    right = FALSE, include.lowest = TRUE)
  flow.table <- with(flow.ts.monthly, table(flow.class, 
                                            month))
  pbreaks <- "see Table"
  X <- apply(flow.table, 2, sum, na.rm = T)
  Y <- apply(flow.table, 1, sum, na.rm = T)
  Z <- sum(flow.table, na.rm = TRUE)
  HX <- -1 * sum((X/Z) * log(X/Z, base = base.entropy), na.rm = TRUE)
  HY <- -1 * sum((Y/Z) * log(Y/Z, base = base.entropy), na.rm = TRUE)
  HXY <- -1 * sum((flow.table/Z) * log(flow.table/Z, base = base.entropy), 
                  na.rm = TRUE)
  P <- round(1 - (HXY - HX)/log(s, base = base.binning), 2)
  C <- round(1 - HY/log(s, base = base.binning), 2)
  M <- round((HX + HY - HXY)/log(s, base = base.binning), 2)
  if (indices.only == TRUE) {
    return(data.frame(P = P, C = C, M = M, CP = C/P, MP = M/P))
  }
  else {
    return(list(breaks = pbreaks, flow.table = flow.table, 
                P = P, C = C, M = M, CP = C/P, MP = M/P))
  }
}