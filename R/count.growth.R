# Calculate growth of the classes created (Deforestation, Reforestation...) ####
count.growth <- function(proxy.table,
                         output.folder,
                         output.name,
                         column.used = NULL){

  # Function ifself ####
  # Organize data ####
  # Number of diferent columns used
  n.types.table <- unique(proxy.table[,column.used])

  # Create empty columns
  proxy.table$Deforestation <- NA
  proxy.table$Reforestation <- NA
  proxy.table$Growth_Agriculture <- NA
  proxy.table$Growth_Mining <- NA
  proxy.table$Growth_Pasture <- NA
  proxy.table$Growth_Urban <- NA

  # Columns that we want to use later
  suppressWarnings({
    col.num <- which(!is.na(as.numeric(colnames(proxy.table))))
  })

  first.col <- 1:(min(col.num) - 1)

  # Reorder columns
  proxy.table <- proxy.table[, c(colnames(proxy.table)[first.col],
                                 "Deforestation", "Reforestation",
                                 "Growth_Urban", "Growth_Mining",
                                 "Growth_Pasture", "Growth_Agriculture",
                                 "Forest", "NonForest", "Water", "Others",
                                 "Urban", "Mining", "Pasture", "Agriculture",
                                 sort(colnames(proxy.table)[col.num]))]


  # Calculate growth ####
  # DEFORESTATION
  proxy.table[2:nrow(proxy.table), "Deforestation"] <-
    base::diff(proxy.table[, "Forest"], lag = 1) * (-1)

  # REFORESTATION (when deforestation is negative)
  proxy.table[, "Reforestation"] <-
    ifelse(is.na(proxy.table[, "Deforestation"]) |
             proxy.table[, "Deforestation"] < 0,
           proxy.table[, "Deforestation"] * (-1),
           0)

  # GROWTH AGRICULTURE
  proxy.table[2:nrow(proxy.table), "Growth_Agriculture"] <-
    base::diff(proxy.table[, "Agriculture"], lag = 1)

  # GROWTH MINING
  proxy.table[2:nrow(proxy.table), "Growth_Mining"] <-
    base::diff(proxy.table[, "Mining"], lag = 1)

  # GROWTH PASTURE
  proxy.table[2:nrow(proxy.table), "Growth_Pasture"] <-
    base::diff(proxy.table[, "Pasture"], lag = 1)

  # GROWTH URBAN
  proxy.table[2:nrow(proxy.table), "Growth_Urban"] <-
    base::diff(proxy.table[, "Urban"], lag = 1)

  # Primeiro ano sempre NA
  proxy.table[(proxy.table$Year == min(proxy.table$Year)),
              c("Deforestation", "Reforestation",
                "Growth_Urban", "Growth_Mining",
                "Growth_Pasture", "Growth_Agriculture")] <- NA

  # When growths are negative, make them equal zero
  proxy.table[which(proxy.table$Deforestation < 0), "Deforestation"] <- 0
  proxy.table[which(proxy.table$Growth_Agriculture < 0), "Growth_Agriculture"] <- 0
  proxy.table[which(proxy.table$Growth_Mining < 0), "Growth_Mining"] <- 0
  proxy.table[which(proxy.table$Growth_Pasture < 0), "Growth_Pasture"] <- 0
  proxy.table[which(proxy.table$Growth_Urban < 0), "Growth_Urban"] <- 0


  # Save two files/tables as a .txt file####
  # Save the final complete table
  write.table(x = proxy.table,
              file = paste0(output.folder, output.name),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
              fileEncoding = "UTF-8")

  # Save the table only with the classes
  write.table(x = proxy.table[,is.na(as.numeric(colnames(proxy.table)))],
              file = paste0(output.folder, "Simplified_", output.name),
              quote = FALSE, sep = "\t", dec = ".", row.names = FALSE,
              fileEncoding = "UTF-8")


  # Return ####
  # Return the complete table
  return(proxy.table)
}
