library(vdemdata)
library(data.table)
library(ggplot2)
library(patchwork)



source("case-studies/posts/20250329/theme/theme.R")

data.vdem <- vdemdata::vdem |> data.table()
# Define a function to create standardized plots for different indices with year range parameters
# Define a function to create charts with gradient coloring
plot_democracy_index <- function(vc.country, index_col, year_min = NULL, year_max = NULL) {
  # Use the min/max years from the data if not provided
  if (is.null(year_min)) year_min <- min(data.vdem$year)
  if (is.null(year_max)) year_max <- max(data.vdem$year)
  
  
  library(data.table)
  
  vdem_indicators <- data.table(
    Code = c(
      "v2x_civlib", "v2x_clphy", "v2x_jucon", "v2x_corr", "v2x_freexp_altinf",
      "v2x_execorr", "v2x_frassoc_thick", "v2x_suffr", "v2x_mpi", 
      "v2xlg_legcon", "v2xcs_ccsi", "v2x_partipdem", "v2x_delibdem",
      "v2x_libdem", "v2x_polyarchy", "v2xlg_leginteract", "v2xps_party",
      "v2xnp_pres"
    ),
    index = c(
      "Libertés civiles",
      "Protection physique",
      "Indépendance judiciaire",
      "Contrôle de la corruption",
      "Liberté d'expression et accès à l'information",
      "Corruption au sein de l'exécutif",
      "Liberté d'association (renforcée)",
      "Droit de vote universel",
      "Influence des médias publics",
      "Contraintes législatives sur l'exécutif",
      "Influence des acteurs sociaux civils",
      "Démocratie participative",
      "Démocratie délibérative",
      "Démocratie libérale",
      "Démocratie électorale",
      "Interaction exécutif-législatif",
      "Partis politiques",
      "Présence de médias non étatiques"
    ),
    Description = c(
      "Mesure les libertés individuelles comme la liberté d'expression et de réunion.",
      "Évalue le respect de l'intégrité physique (absence de torture, exécutions, etc).",
      "Évalue le niveau d'indépendance du système judiciaire.",
      "Mesure la perception de la corruption au sein des institutions publiques.",
      "Évalue la diversité et l'indépendance des sources d'information.",
      "Mesure la probabilité que les dirigeants utilisent le pouvoir à des fins privées.",
      "Mesure les conditions pour les ONG, partis, syndicats à opérer librement.",
      "Indique si le droit de vote est largement garanti à tous les adultes.",
      "Évalue si les médias publics sont neutres ou biaisés politiquement.",
      "Mesure les contre-pouvoirs exercés par le parlement sur l'exécutif.",
      "Évalue le poids de la société civile dans les affaires politiques.",
      "Mesure la participation active des citoyens au processus politique.",
      "Mesure le degré de débat public rationnel et informé avant la prise de décision politique.",
      "Évalue la protection des droits individuels et la séparation des pouvoirs.",
      "Mesure la qualité des élections et la compétition politique.",
      "Mesure la collaboration ou le conflit entre les branches du pouvoir.",
      "Mesure le pluralisme et la force des partis politiques.",
      "Mesure l'existence de médias indépendants de l'État."
    ),
    Interpretation = c(
      "0: Absence de libertés civiles; 100: Protection complète des libertés civiles",
      "0: Violations systématiques de l'intégrité physique; 100: Protection totale de l'intégrité physique",
      "0: Aucune indépendance judiciaire; 100: Indépendance judiciaire complète",
      "0: Corruption minimale; 100: Corruption endémique",
      "0: Censure totale; 100: Liberté d'expression et pluralisme médiatique complets",
      "0: Exécutif sans corruption; 100: Corruption exécutive systémique",
      "0: Aucune liberté d'association; 100: Liberté d'association totale",
      "0: Suffrage très limité; 100: Suffrage universel effectif",
      "0: Médias publics entièrement biaisés; 100: Médias publics totalement neutres",
      "0: Aucune contrainte sur l'exécutif; 100: Fortes contraintes législatives",
      "0: Société civile inexistante/réprimée; 100: Société civile active et influente",
      "0: Aucune participation citoyenne; 100: Participation citoyenne étendue",
      "0: Absence de délibération; 100: Processus délibératif idéal",
      "0: Régime autoritaire; 100: Démocratie libérale pleinement développée",
      "0: Régime fermé non-électoral; 100: Démocratie électorale pleinement développée",
      "0: Conflit permanent exécutif-législatif; 100: Collaboration constructive",
      "0: Absence de partis politiques indépendants; 100: Système de partis fort et pluraliste",
      "0: Monopole étatique des médias; 100: Médias non-étatiques nombreux et indépendants"
    )
  )
  
  # Afficher un aperçu
  print(vdem_indicators)
  
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










# vc.country <- c("Nigeria", "Gabon", "Cameroon", "Equatorial Guinea", "Republic of the Congo")
vc.country <- c("Gabon", "Senegal", "Ivory Coast")


# Example usage with different year ranges:
p1 <- plot_democracy_index(vc.country = vc.country, index_col = "v2x_civlib", 2009, 2024) 
p2 <- plot_democracy_index(vc.country = vc.country, index_col = "v2x_jucon", 2009, 2024) 
p3 <- plot_democracy_index(vc.country = vc.country, index_col = "v2x_corr",  2009, 2024) 
p4 <- plot_democracy_index(vc.country = vc.country, index_col = "v2xps_party", 2009, 2024) 
p5 <- plot_democracy_index(vc.country = vc.country, index_col = "v2x_mpi", 2005, 2024) 
p6 <- plot_democracy_index(vc.country = vc.country, index_col = "v2xcs_ccsi", 2005, 2024) 









######################## continent

library(data.table)
library(data.table)

# Ensure country_name is character
data.vdem[, country_name := as.character(country_name)]

# Assign continent using nested ifelse
data.vdem[, continent := ifelse(
  country_name %in% c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", 
                      "Cabo Verde", "Cameroon", "Central African Republic", "Chad", 
                      "Comoros", "Congo", "Democratic Republic of the Congo", "Djibouti", 
                      "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", 
                      "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Ivory Coast", 
                      "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", "Malawi", "Mali", 
                      "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", 
                      "Nigeria", "Rwanda", "Sao Tome and Principe", "Senegal", "Seychelles", 
                      "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", 
                      "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe", "Côte d'Ivoire"), "Africa",
  ifelse(country_name %in% c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan", 
                             "Brunei", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia", 
                             "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", 
                             "Kyrgyzstan", "Laos", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
                             "Myanmar", "Nepal", "North Korea", "Oman", "Pakistan", "Palestine", 
                             "Philippines", "Qatar", "Saudi Arabia", "Singapore", "South Korea", 
                             "Sri Lanka", "Syria", "Taiwan", "Tajikistan", "Thailand", "Timor-Leste", 
                             "Turkey", "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Vietnam", 
                             "Yemen"), "Asia",
         ifelse(country_name %in% c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", 
                                    "Bulgaria", "Croatia", "Czech Republic", "Denmark", "Estonia", "Finland", 
                                    "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", 
                                    "Kosovo", "Latvia", "Liechtenstein", "Lithuania", "Luxembourg", "Malta", 
                                    "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", 
                                    "Norway", "Poland", "Portugal", "Romania", "Russia", "San Marino", 
                                    "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", 
                                    "Ukraine", "United Kingdom", "Vatican City"), "Europe",
                ifelse(country_name %in% c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                                           "Canada", "Costa Rica", "Cuba", "Dominica", "Dominican Republic", 
                                           "El Salvador", "Grenada", "Guatemala", "Haiti", "Honduras", 
                                           "Jamaica", "Mexico", "Nicaragua", "Panama", "Saint Kitts and Nevis", 
                                           "Saint Lucia", "Saint Vincent and the Grenadines", 
                                           "Trinidad and Tobago", "United States", "United States of America"), "North America",
                       ifelse(country_name %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
                                                  "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", 
                                                  "Uruguay", "Venezuela"), "South America",
                              ifelse(country_name %in% c("Australia", "Fiji", "Kiribati", "Marshall Islands", "Micronesia", 
                                                         "Nauru", "New Zealand", "Palau", "Papua New Guinea", "Samoa", 
                                                         "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu"), "Oceania",
                                     "Unknown")))))
)]



vc.continent = "Africa"
plot_continent_index <- function(index_col = "v2x_mpi", vc.continent = "Africa", selected_year = 2020, 
                                 color = "steelblue", data_vdem = data.vdem) {
  
  # Input validation
  if (!is.data.table(data_vdem)) {
    stop("Input data must be a data.table")
  }
  
  if (!(index_col %in% names(data_vdem))) {
    stop(paste("Column", index_col, "not found in the dataset"))
    
    
    
  }
  
  
  vdem_indicators <- data.table(
    Code = c(
      "v2x_civlib", "v2x_clphy", "v2x_jucon", "v2x_corr", "v2x_freexp_altinf",
      "v2x_execorr", "v2x_frassoc_thick", "v2x_suffr", "v2x_mpi", 
      "v2xlg_legcon", "v2xcs_ccsi", "v2x_partipdem", "v2x_delibdem",
      "v2x_libdem", "v2x_polyarchy", "v2xlg_leginteract", "v2xps_party",
      "v2xnp_pres", 
      "v2exdeathog", "v2exctlhg", "v2exrmhgnp",
      "v2exrescon", "v2exbribe", "v2exembez",
      "v2excrptps", "v2exthftps",
      "v2exthftps", "v2exapup", "v2exapupap", "v2reginfo", "v2regendtype",
      "v2regint", "v2regidnr", "v2regdur", "v2regimpgroup", "v2regsuploc",
      "v2regimpoppgroup", "v2regoppgroupssize", "v2regsupgroupssize"
    ),
    index = c(
      "Libertés civiles",
      "Protection physique",
      "Indépendance judiciaire",
      "Contrôle de la corruption",
      "Liberté d'expression et accès à l'information",
      "Corruption au sein de l'exécutif",
      "Liberté d'association (renforcée)",
      "Droit de vote universel",
      "Influence des médias publics",
      "Contraintes législatives sur l'exécutif",
      "Influence des acteurs sociaux civils",
      "Démocratie participative",
      "Démocratie délibérative",
      "Démocratie libérale",
      "Démocratie électorale",
      "Interaction exécutif-législatif",
      "Partis politiques",
      "Présence de médias non étatiques",
      "Usage de la peine de mort par l'exécutif",
      "Contrôle de l’exécutif sur le gouvernement",
      "Pouvoir de nomination du chef de l’exécutif",
      "Restrictions imposées par l’exécutif",
      "Usage de pots-de-vin",
      "Détournement de fonds publics",
      "Perception globale de la corruption",
      "Vol de ressources publiques",
      "Vol de ressources publiques",
      "Accès du public à l'information",
      "Accès des autorités publiques à l'information",
      "Degré d'information régionale",
      "Type de régime politique",
      "Intervention des autorités dans les régions",
      "Nombre d'identifiants régionaux",
      "Durée du régime régional",
      "Influence des groupes d’intérêt",
      "Localisation des groupes de soutien",
      "Opposition des groupes d’intérêt",
      "Taille des groupes d’opposition régionaux",
      "Taille des groupes de soutien régionaux"
      
    ),
    Description = c(
      "Mesure les libertés individuelles comme la liberté d'expression et de réunion.",
      "Évalue le respect de l'intégrité physique (absence de torture, exécutions, etc).",
      "Évalue le niveau d'indépendance du système judiciaire.",
      "Mesure la perception de la corruption au sein des institutions publiques.",
      "Évalue la diversité et l'indépendance des sources d'information.",
      "Mesure la probabilité que les dirigeants utilisent le pouvoir à des fins privées.",
      "Mesure les conditions pour les ONG, partis, syndicats à opérer librement.",
      "Indique si le droit de vote est largement garanti à tous les adultes.",
      "Évalue si les médias publics sont neutres ou biaisés politiquement.",
      "Mesure les contre-pouvoirs exercés par le parlement sur l'exécutif.",
      "Évalue le poids de la société civile dans les affaires politiques.",
      "Mesure la participation active des citoyens au processus politique.",
      "Mesure le degré de débat public rationnel et informé avant la prise de décision politique.",
      "Évalue la protection des droits individuels et la séparation des pouvoirs.",
      "Mesure la qualité des élections et la compétition politique.",
      "Mesure la collaboration ou le conflit entre les branches du pouvoir.",
      "Mesure le pluralisme et la force des partis politiques.",
      "Mesure l'existence de médias indépendants de l'État.",
      "Mesure à quel point le chef de l’exécutif autorise ou utilise la peine de mort.",
      "Évalue le degré de contrôle exercé par l’exécutif sur les membres du gouvernement.",
      "Indique dans quelle mesure le chef de l’exécutif peut nommer ou révoquer des hauts fonctionnaires.",
      "Mesure le niveau de contrainte imposé aux autres institutions par le pouvoir exécutif.",
      "Évalue l’usage de pots-de-vin comme outil de gouvernance.",
      "Mesure l’ampleur des détournements de fonds publics par les autorités.",
      "Indique la perception générale de la corruption dans le système politique.",
      "Mesure la fréquence et l’étendue du vol de biens publics ou de ressources par les autorités.",
      "Mesure la fréquence et l'étendue du vol de biens publics ou de ressources par les autorités.",
      "Mesure le niveau d'accès des citoyens aux informations gouvernementales.",
      "Mesure l'étendue de l'accès des autorités publiques à des données confidentielles ou citoyennes.",
      "Indique à quel point les informations sont disponibles ou utilisées à un niveau régional.",
      "Classifie le type de régime selon des critères institutionnels ou formels.",
      "Mesure le degré d’intervention des autorités centrales dans les affaires régionales.",
      "Compte le nombre d'identifiants uniques associés aux structures régionales.",
      "Indique la durée pendant laquelle le régime régional est en place.",
      "Mesure l'influence des groupes d’intérêt dans les politiques publiques régionales.",
      "Indique où sont localisés les principaux groupes de soutien du régime.",
      "Mesure l’existence et l’influence des groupes d’opposition dans la région.",
      "Évalue la taille des groupes d'opposition actifs dans les régions.",
      "Évalue la taille des groupes de soutien du régime présents dans les régions."
    ),
    Interpretation = c(
      "0: Absence de libertés civiles; 100: Protection complète des libertés civiles",
      "0: Violations systématiques de l'intégrité physique; 100: Protection totale de l'intégrité physique",
      "0: Aucune indépendance judiciaire; 100: Indépendance judiciaire complète",
      "0: Corruption minimale; 100: Corruption endémique",
      "0: Censure totale; 100: Liberté d'expression et pluralisme médiatique complets",
      "0: Exécutif sans corruption; 100: Corruption exécutive systémique",
      "0: Aucune liberté d'association; 100: Liberté d'association totale",
      "0: Suffrage très limité; 100: Suffrage universel effectif",
      "0: Médias publics entièrement biaisés; 100: Médias publics totalement neutres",
      "0: Aucune contrainte sur l'exécutif; 100: Fortes contraintes législatives",
      "0: Société civile inexistante/réprimée; 100: Société civile active et influente",
      "0: Aucune participation citoyenne; 100: Participation citoyenne étendue",
      "0: Absence de délibération; 100: Processus délibératif idéal",
      "0: Régime autoritaire; 100: Démocratie libérale pleinement développée",
      "0: Régime fermé non-électoral; 100: Démocratie électorale pleinement développée",
      "0: Conflit permanent exécutif-législatif; 100: Collaboration constructive",
      "0: Absence de partis politiques indépendants; 100: Système de partis fort et pluraliste",
      "0: Monopole étatique des médias; 100: Médias non-étatiques nombreux et indépendants",
      "0: Interdiction ou non-utilisation; 100: Usage fréquent et autorisé de la peine de mort",
      "0: Autonomie totale des membres du gouvernement; 100: Contrôle total de l’exécutif",
      "0: Aucun pouvoir de nomination; 100: Pouvoir discrétionnaire total",
      "0: Pas de restrictions; 100: Restrictions institutionnelles très fortes",
      "0: Aucun usage; 100: Usage systématique des pots-de-vin",
      "0: Aucun détournement; 100: Détournement massif et régulier",
      "0: Aucun soupçon de corruption; 100: Corruption omniprésente",
      "0: Aucun vol signalé; 100: Vol généralisé de ressources publiques",
      "0: Aucun vol rapporté; 100: Vol généralisé de ressources publiques",
      "0: Aucun accès; 100: Accès public libre et garanti à l’information",
      "0: Aucun accès; 100: Accès total à l’information stratégique",
      "0: Aucune information régionale; 100: Informations très détaillées disponibles",
      "Classification catégorique des types de régime (autoritaire, hybride, démocratique, etc.)",
      "0: Aucune intervention; 100: Contrôle central complet des régions",
      "Valeur numérique du nombre d’identifiants régionaux enregistrés",
      "Mesure continue en années ou cycles institutionnels",
      "0: Aucun rôle; 100: Influence majeure sur les décisions politiques",
      "0: Tous exogènes; 100: Présence locale forte",
      "0: Aucune opposition; 100: Opposition régionale structurée et active",
      "0: Groupes très petits ou inexistants; 100: Groupes d’opposition très larges",
      "0: Groupes très petits; 100: Groupes de soutien très larges et bien implantés"
      
    )
  )
  
  # Afficher un aperçu
  # print(vdem_indicators)
  
  index_title <- vdem_indicators[Code == index_col, index]
  index_subtitle <- vdem_indicators[Code == index_col, Description]
  index_interpretation <- vdem_indicators[Code == index_col, Interpretation]
  
  
  
  # Filter by continent
  plot_data <- data_vdem[continent == vc.continent, ]
  
  # Further filter by countries if specified
  
  # Create plot data
  plot_data <- plot_data[year == selected_year, 
                         .(year,
                           continent,
                           index_value = get(index_col),
                           country = country_name)]
  
  
  
  # Handle empty results
  if (nrow(plot_data) == 0) {
    warning("No data found for the specified parameters")
    return(NULL)
  }
  
  # Order data and set factor levels
  plot_data <- plot_data[order(index_value)]
  plot_data <- plot_data[, country := factor(country, levels = country)]
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = index_value, y = reorder(country, index_value), 
                             fill = country == "Gabon")) +
    
    geom_bar(stat = "identity") +
    
    labs(
      title = paste("", index_title, "in", paste(vc.continent), "(", selected_year, ")"),
      subtitle = paste(index_subtitle, "\n", index_interpretation, sep = ""), 
      x = paste(index_title, "Indice"),
      y = "Pays"
    ) +
    
    geom_vline(xintercept = 0.5, color = "black", linetype = "dashed", alpha = 0.7) +
    
    scale_fill_manual(values = c("TRUE" = "#872916", "FALSE" = "gray80"), guide = "none") +
    
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  # Apply custom theme if available
  if (exists("theme_labinc")) {
    p <- p + theme_labinc()
  }
  
  return(p)
}


plot_continent_index(index_col = "v2exrescon", selected_year = 2010, vc.continent = "Africa")

# 
# ggsave(
#   filename = "~/Desktop/democracy_deliberative_index_2005_2024.png",
#   plot = p6,
#   width = 10,
#   height = 7)


# 
# 
# p5
# 
# ggsave()
# # To display the plots together:
# # library(patchwork)
# (p1 + p2) / (p3 + p4)

# Or with gridExtra:
# library(gridExtra)
# grid.arrange(p1, p2, p3, p4, ncol = 2)