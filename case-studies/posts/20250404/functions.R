plot_democracy_index <- function(vc.country, index_col, year_min = NULL, year_max = NULL) {
  # Use the min/max years from the data if not provided
  if (is.null(year_min)) year_min <- min(data.vdem$year)
  if (is.null(year_max)) year_max <- max(data.vdem$year)
  
  
  library(data.table)
 
  source('data/titlevdem.R', local = T)
  
  # Afficher un aperçu

  index_title <- vdem_indicators[Code == index_col, index]
  index_subtitle <- vdem_indicators[Code == index_col, Description]
  index_interpretation <- vdem_indicators[Code == index_col, Interpretation]
  
  
  # Filter data by country and year range
  plot_data <- data.vdem[country_name %in% vc.country & 
                           year >= year_min & 
                           year <= year_max, 
                         .(year, index_value = get(index_col), country = country_name)]
  
  
  plot_data <- data.vdem[country_name %in% vc.country & 
                           year >= year_min & 
                           year <= year_max, 
                         .(year,
                           index_value = get(index_col),
                           index_low = get(paste0(index_col, "_codelow")),
                           index_high = get(paste0(index_col, "_codehigh")),
                           country = country_name)]
  
  
  # Compute the year-on-year gradient for each country
  plot_data <- plot_data[order(country, year)]
  plot_data[, gradient := c(NA, diff(index_value)), by = country]
  
  
  
  ggplot(plot_data, aes(x = year, y = index_value, group = country)) + 
    
    geom_ribbon(
      aes(ymin = index_low, ymax = index_high, fill = country),
      alpha = 0.2,
      color = NA
    ) +
    
    # Base gray line with country-dependent linetype
    geom_line(aes(linetype = country), color = "gray80", linewidth = 0.5) +
    # geom_line(aes(color = country, linetype = country), linewidth = 1.5) + 
    
    geom_point(aes(color = country, shape = country), size = 3) +
    
    # Colored segments based on gradient direction
    geom_segment(data = plot_data[!is.na(gradient)],
                 aes(x = year - 1, 
                     xend = year,
                     y = index_value - gradient, 
                     yend = index_value,
                     color = gradient >= 0, 
                     linetype = country),
                 linewidth = 1.5) +
    
    geom_hline(yintercept = 0.5, color = "gray60", linetype = "dashed") +
    
    # Color palette: red for negative, green for positive
    scale_color_manual(
      values = c("FALSE" = "#872916", "TRUE" = "green4"),
      labels = c("FALSE" = "Déclin", "TRUE" = "Amélioration/stabilité"),
      name = "Tendance"
    ) +
    
    labs(
      title = paste("", index_title, ":", paste(vc.country, collapse = " vs ")),
      subtitle = paste(index_subtitle, " (", year_min, "-", year_max, ")", "\n",
                       index_interpretation,
                       sep = ""),
      x = "Année",
      y = paste(index_title, "Indice"), 
      # caption = "Données : V-DEM\nAnalyse : labinc.ogoouestat.com",
      caption = "Données : V-DEM"
      
    ) +
    
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    
    scale_x_continuous(
      breaks = seq(year_min, year_max, by = ifelse(year_max - year_min > 50, 10, 2))
    ) +  
    # geom_vline(xintercept = 1990, color = "blue", linetype = "dashed", alpha = 0.7) +
    # Ajout des lignes verticales et des annotations avec angle
    # geom_vline(xintercept = 1990, color = "black", linetype = "dashed", alpha = 0.7) +  
    # geom_vline(xintercept = 1993, color = "black", linetype = "dashed", alpha = 0.7) +  
    # geom_vline(xintercept = 2003, color = "black", linetype = "dashed", alpha = 0.7) +  
    geom_vline(xintercept = 2009, color = "black", linetype = "dashed", alpha = 0.7) +  
    geom_vline(xintercept = 2016, color = "black", linetype = "dashed", alpha = 0.7) +  
    geom_vline(xintercept = 2018, color = "black", linetype = "dashed", alpha = 0.7) +  
    geom_vline(xintercept = 2023, color = "black", linetype = "dashed", alpha = 0.7) +  
    
    # annotate("text", x = 1990 - 0.3, y = max(plot_data$index_value, na.rm = TRUE) * 0.85, 
    #          label = "Introduction du multipartisme", hjust = -0.1, angle = 15, size = 3, color = "black") +
    # annotate("text", x = 1993 - 0.3, y = max(plot_data$index_value, na.rm = TRUE) * 0.87, 
    #          label = "1ère élection pluraliste", hjust = -0.1, angle = 15, size = 3, color = "black") +
    # annotate("text", x = 2003 - 0.3, y = max(plot_data$index_value, na.rm = TRUE) * 0.89, 
    #          label = "Limite de mandat supprimée", hjust = -0.3, angle = 15, size = 3, color = "black") +
    annotate("text", x = 2009 - 0.3, y = max(plot_data$index_value, na.rm = TRUE) * 0.93, 
             label = "1er mandat\nAli Bongo", hjust = 0.1, angle = 0, size = 3, color = "black") +
    annotate("text", x = 2016 - 0.3, y = max(plot_data$index_value, na.rm = TRUE) * 0.93, 
             label = "2ème mandat\nAli Bongo", hjust = 0.5, angle = 0, size = 3, color = "black") +
    annotate("text", x = 2018 - 0.3, y = max(plot_data$index_value, na.rm = TRUE) * 0.83, 
             label = "AVC de \nAli Bongo", hjust = 0.5, angle = 0, size = 3, color = "black") +
    annotate("text", x = 2023 - 0.3, y = max(plot_data$index_value, na.rm = TRUE) * 0.93, 
             label = "Coup d'État \nmilitaire", hjust = 0.5, angle = 0, size = 3, color = "black") + 
    
    theme_labinc() +
    guides(color = guide_legend(nrow = 1)) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12, face = "bold")
    )
  
}





plot_democracy_radar <- function(vc.country, index_col, year_sel = NULL) {
  # Use the min/max years from the data if not provided
  if (is.null(year_sel)) year_sel <- max(data.vdem$year)
  
  
  source('data/titlevdem.R', local = T)
  
  # Afficher un aperçu

  index_title <- vdem_indicators[match(index_col, Code), index]
  index_subtitle <- vdem_indicators[match(index_col, Code), Description]
  index_interpretation <- vdem_indicators[match(index_col, Code), Interpretation]
  
  
  # Filter data by country and year range
  
  
  
  plot_data <- data.vdem[country_name %in% vc.country & 
                           year == year_sel, 
                         c("country_name", (index_col)), with = F]
  
  if("v2x_corr" %in% names(plot_data)) plot_data[, v2x_corr := 1 - v2x_corr]
  if("v2xnp_pres" %in% names(plot_data)) plot_data[, v2xnp_pres := 1 - v2xnp_pres]
  if("v2xnp_client" %in% names(plot_data)) plot_data[, v2xnp_client := 1 - v2xnp_client]
  
  
  setnames(plot_data, c("country_name", index_col), c("country", index_title))
  
  make_textbox_label <- function(titles, interpretations) {
    n <- length(titles)
    mid <- ceiling(n / 2)
    
    left <- paste0("\n• ", titles[1:mid], " : ", interpretations[1:mid])
    right <- paste0("\n• ", titles[(mid + 1):n], " : ", interpretations[(mid + 1):n])
    
    # équilibrer la longueur
    max_len <- max(length(left), length(right))
    left <- c(left, rep("", max_len - length(left)))
    right <- c(right, rep("", max_len - length(right)))
    
    paste(
      "Interprétation des indices :\n\n",
      paste0(format(left, width = 60, justify = "left"), "   ", right, collapse = "\n")
    )
  }
  
  
  
  library(RColorBrewer)
  
  # Get the Set3 color palette with enough colors for your countries
  num_countries <- length(unique(plot_data$country))
  set3_colors <- brewer.pal(min(12, max(3, num_countries)), "Set2")
  
  # If you have more than 12 countries, you may need to extend the palette
  if(num_countries > 12) {
    set3_colors <- colorRampPalette(set3_colors)(num_countries)
  }
  
  plt <- plot_data %>%
    ggradar(
      font.radar = "Oswald",
      grid.label.size = ,
      axis.label.size = 6,
      group.point.size = 2,
      group.line.width = 1,
      group.colours = set3_colors
    ) + 
    labs(
      title = paste0("Indices démocratiques comparés – ", year_sel),
      #subtitle = paste(vc.country, collapse = " vs "),
      x = NULL,
      y = NULL, 
      caption = paste0(
        "Données : V-DEM\nAnalyse : labinc.ogoouestat.com"
        # paste0(paste(gsub("\n", "", index_title), " : ", index_interpretation), collapse = "\n")
      )
    ) + 
    theme_labinc2() +
    theme(
      plot.title = element_text(size = 20, face = "bold"),
      plot.title.position = "plot", # slightly different from default
      plot.subtitle = element_text(size = 16),
      plot.caption = element_text(size = 15),
      legend.position = "bottom",
      legend.text = element_text(size = 12),
      legend.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.margin = margin(1,1,1,1)
    )
  
  
  # theme(
  #   axis.title = element_blank(),
  #   axis.text = element_text(color=txt_col, size=7),
  #   strip.text.x = element_text(face="bold"),
  #   plot.title = element_markdown(hjust=.5,size=34, color=txt_col,lineheight=.8, face="bold", margin=margin(20,0,30,0)),
  #   plot.subtitle = element_markdown(hjust=.5,size=18, color=txt_col,lineheight = 1, margin=margin(10,0,30,0)),
  #   plot.caption = element_markdown(hjust=.5, margin=margin(60,0,0,0), size=8, color=txt_col, lineheight = 1.2),
  #   plot.caption.position = "plot",
  #   plot.background = element_rect(color=bg, fill=bg),
  #   plot.margin = margin(10,10,10,10),
  #   legend.position = "none",
  #   legend.title = element_text(face="bold")
  # )
  
  
  
  library(ggtext)
  
  text <- tibble(
    x = 0, y = 0,
    label = make_textbox_label(index_title, index_interpretation)
  )
  
  sub <- ggplot(text, aes(x = x, y = y)) +
    geom_textbox(
      aes(label = label),
      box.color = "#f0eeeb", fill = "#f0eeeb",
      width = unit(25, "lines"),
      family = "Oswald",
      size = 5,
      lineheight = 1.1
    ) +
    labs(
      x = NULL,
      y = NULL
    ) + 
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_labinc2() + 
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  # TITLE
  text2 <- tibble(
    x = 0, y = 0,
    label = paste0("**Indices démocratiques\n comparés –", year_sel, "**")
  )
  
  title <- ggplot(text2, aes(x = x, y = y)) +
    geom_textbox(
      aes(label = label),
      box.color = "#f0eeeb", fill="#f0eeeb", width = unit(12, "lines"),
      family="Oswald", size = 10, lineheight = 1
    ) +
    labs(
      x = NULL,
      y = NULL
    ) + 
    coord_cartesian(expand = FALSE, clip = "off") +
    theme_void() +
    theme(plot.background = element_rect(color="#f0eeeb", fill="#f0eeeb")) + 
    theme_labinc2() + 
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  
  
  
  # finalPlot <- (plt) 
  
  
  # finalPlot <- (plt) + sub
  
  # finalPlot <- plt+ plot_spacer() + sub
  
  
  finalPlot <- (plt + theme(plot.margin = unit(c(0,150,0,0), "pt"))) +
    (sub) + plot_layout(widths = c(4, 1))
  
  # finalPlot <- wrap_elements(title) /
  #   wrap_elements(sub) /
  #   plt +
  #   plot_layout(heights = c(1, 1.2, 3)) +
  #   plot_annotation(
  #     theme = theme(
  
  #       plot.caption = element_markdown(
  #         hjust = 0, margin = margin(20, 0, 0, 0),
  #         size = 6, color = txt_col, lineheight = 1.2
  #       ),
  #       plot.margin = margin(20, 20, 20, 20)
  #     )
  #   )
  
  # finalPlot <- wrap_elements(title) /
  #   (plt | sub) +
  #   plot_layout(heights = c(1, 2), widths = c(2, 1)) +
  #   plot_annotation(
  #     theme = theme(
  #       plot.caption = element_markdown(
  #         hjust = 0, margin = margin(20, 0, 0, 0),
  #         size = 6, color = txt_col, lineheight = 1.2
  #       ),
  #       plot.margin = margin(20, 20, 20, 20)
  #     )
  #   )
  # 
  
  
  
  return(list("finalPlot" = finalPlot, 
              "plt" = plt, 
              "sub" = sub))
  
}

