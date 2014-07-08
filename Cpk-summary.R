DoCpk <- function(dt3) {
  
  library(plyr)
  library(data.table)
  
  dt3$Margin2 = pmin(dt3$Data - dt3$Min,dt3$Max - dt3$Data, na.rm=TRUE)
  dt3$MarginPerc2 = dt3$Margin2 / dt3$Data

  Summary = ddply(dt3,.(RAIL, Measurement), summarize, 
                  mean = mean(Data), 
                  margin = min(Margin2), 
                  sd = sd(Data), 
                  LSL = min(Min), 
                  USL = max(Max)
                  )

  Summary$SigmaMargin = Summary$margin / (3*Summary$sd)
  Summary$Cpl = (Summary$mean - Summary$LSL) / (3*Summary$sd)
  Summary$Cpu = (Summary$USL - Summary$mean) / (3*Summary$sd)
  
  Summary$Cpk = pmin(Summary$Cpu, Summary$Cpl, na.rm=TRUE)
  return(Summary)
}