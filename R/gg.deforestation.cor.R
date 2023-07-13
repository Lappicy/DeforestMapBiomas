# Graph for Deforestation x Other classes growth ####
gg.deforestation.cor <-
  function(proxy.table,
           type.one = "Growth",
           type.two = "Deforestation",
           color.one = c("purple", "grey50", "#EA9999", "darkorange"),
           color.two = "darkgreen",
           save.as = "Results/Deforestation vs Growth.png",
           title.name = "Analysis for Uaça 1 e 2",
           different.groups = NULL){

    # Libraries required ####
    require(ggplot2)
    require(stats)


    # Function itself ####
    # Prepare data ####
    # Make sure the data is in a data.frame format.
    proxy.table <- as.data.frame(proxy.table)

    # Make sure the Year column is numeric
    proxy.table$Year <- as.numeric(proxy.table$Year)

    # Which columns have "type.one" or "type.two" in their names
    columns.type.one <-
      colnames(proxy.table)[grep(type.one, colnames(proxy.table))]

    columns.type.one.names <- substr(x = columns.type.one,
                                     start = nchar(type.one) + 2,
                                     stop = nchar(columns.type.one))

    colnames(proxy.table)[colnames(proxy.table) %in% columns.type.one] <-
      columns.type.one.names

    columns.type.one <- columns.type.one.names

    columns.type.two <-
      colnames(proxy.table)[grep(type.two, colnames(proxy.table))]


    # Correlation analysis ####
    # Only get type.one and type.two columns
    cor.table <- proxy.table[,which(colnames(proxy.table) %in%
                                      c(columns.type.two, columns.type.one))]

    # Remove any blank data (NA)
    cor.table <- na.omit(cor.table)

    # Correlate the data
    suppressWarnings(cor.data <- stats::cor(cor.table)[,1])

    # Which class has the higher correlation
    cor.information <-
      round(max(abs(cor.data[2:length(cor.data)]), na.rm = TRUE), digits = 2)

    # Write a + or - sign accordingly
    ifelse(max(abs(cor.data[2:length(cor.data)]), na.rm = TRUE) ==
             max(cor.data[2:length(cor.data)], na.rm = TRUE),
           cor.information <- paste0("+", cor.information),
           cor.information <- paste0("-", cor.information))

    # Make sure it always has 5 characters
    if(nchar(cor.information) == 4) cor.information <- paste0(cor.information, "0")

    # Name the values
    names(cor.information) <-
      columns.type.one[which.max(abs(cor.data[2:length(cor.data)]))]


    # Prepare table for ggplot2 ####
    proxy.table <-
      tidyr::pivot_longer(data = proxy.table,
                          cols = c(columns.type.two, columns.type.one),
                          names_to = "Classes",
                          values_to = "NumValue") |>
      dplyr::group_by_at(c("Year", different.groups, "Classes")) |>
      dplyr::reframe(NumValue = sum(NumValue, na.rm = TRUE)) |>
      as.data.frame()

    # Alfabetical order of type.one and type.two
    order.types <- order(c(columns.type.one, columns.type.two))


    # GGPLOT2 graph ####
    gg.internal <-

      # Data being plotted
      ggplot2::ggplot(data = proxy.table,
                      aes(x = Year, y = NumValue, color = Classes)) +

      # Plot the deforestation only
      ggplot2::geom_line(data = proxy.table[proxy.table$Classes %in%
                                              columns.type.two,],
                         color = color.two, linewidth = 1, alpha = 0.5) +

      # Plot other classes
      ggplot2::geom_line(linewidth = 1, alpha = 0.3) +
      ggplot2::geom_point(alpha = 0.5) +

      ggplot2::scale_color_manual(values = c(color.one, color.two)[order.types]) +

      ggplot2::labs(title = title.name,
                    subtitle = paste0("Greatest correlation with ",
                                      names(cor.information), " (",
                                      cor.information, ")"),
                    x = "Year", y = bquote("[" ~ km^2 ~"]"), color = "Class:") +
      ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = ",",
                                                              decimal.mark = ".",
                                                              scientific = F)) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top",
                     legend.text = element_text(size = 12, color = "black"),
                     legend.justification = "left",
                     legend.box.just = "left",
                     title = element_text(size = 12, color = "black", face = "bold"),
                     text = element_text(size = 12, color = "black"),
                     axis.text = element_text(size = 12, color = "black"))


    # save where the user setted it to
    ggplot2::ggsave(filename = save.as, plot = gg.internal,
                    width = 3000, height = 1500, units = "px", dpi = 300)


    # Returns (NULL) ####
    return(NULL)
}
