# Create a data frame for the 2025 presidential election results
election_results <- data.frame(
  Region = c(
    rep("Estuaire", 8),
    rep("Haut-Ogooué", 8),
    rep("Moyen-Ogooué", 8),
    rep("Ngounié", 8),
    rep("Nyanga", 8),
    rep("Ogooué-Ivindo", 8),
    rep("Ogooué-Lolo", 8),
    rep("Ogooué-Maritime", 8),
    rep("Wouleu-Ntem", 8), 
    rep("G10", 8)
  ),
  Inscrits = c(
    rep(366563, 8),
    rep(103388, 8),
    rep(43845, 8),
    rep(80304, 8),
    rep(36785, 8),
    
    rep(43053, 8),
    rep(38401, 8),
    
    rep(88645, 8),
    rep(87533, 8),
    rep(28148, 8)
    
  ),
  Votants = c(
    rep(235964, 8),
    rep(90246, 8),
    rep(29739, 8),
    rep(53624, 8),
    rep(25580, 8),
    
    rep(24816, 8),
    rep(28741, 8),
    
    rep(47239, 8),
    rep(81674, 8),
    rep(18009, 8)
    
  ),
  Nuls = c(
    rep(10782, 8),
    rep(1419, 8),
    rep(1227, 8),
    rep(2035, 8),
    rep(462, 8),
    
    rep(1484, 8),
    rep(849, 8),
    
    rep(2399, 8),
    rep(664, 8),
    rep(1311, 8)
    
  ),
  Suffrages_exprimes = c(
    rep(225182, 8),
    rep(88827, 8),
    rep(28512, 8),
    rep(51589, 8),
    rep(27118, 8),
    
    rep(28332, 8),
    rep(27892, 8),
    
    rep(44840, 8),
    rep(81010, 8),
    rep(16698, 8)
    
  ),
  Candidat = rep(c(
    "BILIE BY NZE Alain Claude",
    "BOUNGOUERES Alain Simplice",
    "ESSINGONE Joseph Lapensée",
    "GNINGA CHANING Zenaba",
    "IBINGA IBINGA Axel Stophène",
    "ILOKO BOUSSENGUI Stéphane Germain",
    "N'GOMA Thierry Yvon Michel",
    "OLIGUI NGUEMA Brice Clotaire"
  ), 10),
  Voix = c(
    # Estuaire
    10552, 1167, 1795, 1011, 805, 1042, 272, 208538,
    
    # Haut-Ogooué
    1001, 168, 114, 216, 55, 78, 30, 87165,
    # Moyen-Ogooué
    621, 64, 385, 102, 38, 176, 47, 27079,
    # Ngounié
    594, 245, 114, 185, 92, 310, 48, 50001,
    # Nyanga
    219, 25, 37, 105, 24, 24, 22, 26662,
    # Ogooué-Ivindo
    1580, 24, 28, 64, 27, 23, 26, 26560,
    # Ogooué-Lolo
    321, 82, 50, 99, 25, 125, 19, 27171,
    # Ogooué-Maritime
    1677, 146, 380, 321, 155, 248, 70, 41843,
    # Wouleu-Ntem
    776, 35, 93, 37, 91, 105, 45, 79828,
    ## G10
    1924, 343, 748, 279, 72, 83, 22, 13227
    
  )
) |> data.table()


election_results[, taux_participation := Votants / Inscrits]
election_results[, taux_abstention := 1 - taux_participation]


election_results[, dispersion_voix := sd(Voix), by = c("Region")]
election_results[, dispersion_voix := dispersion_voix/Suffrages_exprimes]
election_results[, perc_voix := Voix/Suffrages_exprimes]



library(scales)
ggplot(
  election_results[, .(Region, taux_abstention)] |> unique(),
  aes(x = reorder(Region, taux_abstention), y = taux_abstention, fill = taux_abstention)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", name = "Taux d'abstention") +
  labs(
    title = "Taux d'abstention par région",
    x = "Région",
    y = "Taux d'abstention (%)"
  ) +
  theme_minimal() 



ggplot(
  election_results[, .(Region, dispersion_voix)] |> unique(),
  aes(x = reorder(Region, dispersion_voix), y = dispersion_voix, fill = dispersion_voix)
) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", name = "Taux d'abstention") +
  labs(
    title = "Taux d'abstention par région",
    x = "Région",
    y = "Taux d'abstention (%)"
  ) +
  theme_minimal() +
  theme_labinc2()



##############

library(dplyr)
library(ggplot2)

# Total votes per candidate
total_votes <- election_results %>%
  group_by(Candidat) %>%
  summarise(Total = sum(Voix)) %>%
  arrange(desc(Total))




################

ggplot(election_results, aes(x = Region, y = Voix, fill = Candidat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Candidate Performance by Region",
    x = "Region",
    y = "Votes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  theme_labinc2()

##############

election_results[, perc_voix := Voix / Suffrages_exprimes]

alt_results <- election_results[!Candidat %in% c("OLIGUI NGUEMA Brice Clotaire", "BILIE BY NZE Alain Claude")]



gabon_sf_enhanced <- merge(alt_results, gabon_sf_enhanced, by.y = "NAME_1", by.x = "Region")



best_alt <- gabon_sf_enhanced[, .SD[which.max(Voix)], by = Region]


fig1 <- ggplot(alt_results, aes(x = reorder(Region, perc_voix), y = perc_voix, fill = Candidat)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    name = "Candidat",
    values = c(
      "BOUNGOUERES Alain Simplice" = "#A8E6CF",
      "ESSINGONE Joseph Lapensée" = "#3792CB",
      "GNINGA CHANING Zenaba" = "#FFD700",
      "IBINGA IBINGA Axel Stophène" = "#F67280",
      "ILOKO BOUSSENGUI Stéphane Germain" = "#CBAACB",
      "N'GOMA Thierry Yvon Michel" = "#FFB347"
    )
  ) + 
  
  labs(
    title = "Resultat election alternative par province",
    subtitle = "Pourcentage des suffrages exprimés obtenus par le candidat arrivé en tête (hors favoris)",
    x = "Province",
    y = "Part des voix (%)",
    fill = "Candidat",
  ) +
  theme_labinc2() +   # Remplace par theme_light(), theme_bw(), etc. si besoin
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5, hjust = 0),
  ) 

best_alt_sf <- st_as_sf(best_alt, sf_column_name = "geom")

fig2 <- ggplot(best_alt_sf) +
  geom_sf(aes(fill = Candidat), color = "white", size = 0.2) +
  # geom_sf_text(aes(label = paste0(Region, " : ",round(perc_voix * 100, 1), "%")), size = 2.4, color = "black") + 
  scale_fill_manual(
    name = "Candidat",
    values = c(
      "BOUNGOUERES Alain Simplice" = "#A8E6CF",
      "ESSINGONE Joseph Lapensée" = "#3792CB",
      "GNINGA CHANING Zenaba" = "#FFD700",
      "IBINGA IBINGA Axel Stophène" = "#F67280",
      "ILOKO BOUSSENGUI Stéphane Germain" = "#CBAACB",
      "N'GOMA Thierry Yvon Michel" = "#FFB347"
    )
  ) + 
  
  
  # scale_fill_brewer(palette = "Set3", name = "Candidat") +
  labs(
    title = "Résultats régionaux des candidats alternatifs",
    subtitle = "Ce graphique cartographie, pour chaque province, le candidat ayant obtenu le \nplus de suffrages exprimés hors des deux favoris (OLIGUI et BILIE)",
    caption = "Source : Données issues des résultats provisoires de l’élection présidentielle 2025.\nAnalyse : Le candidat affiché pour chaque région est celui qui, hors OLIGUI NGUEMA et BILIE BY NZE, \na obtenu le plus grand nombre de voix parmi les suffrages exprimés.",
    x = NULL,
    y = NULL
  ) +
  theme_labinc2() +   # Ton thème personnalisé si défini
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 8),
    plot.caption = element_text(size = 5, hjust = 0),
    axis.text = element_blank(),
    axis.ticks = element_blank()) 


fig1  + fig2



#################

VOIX <- election_results$Voix

VOIX <- as.character(VOIX)

VOIX <- unlist(stringr::str_split(VOIX, pattern = ""))

VOIX <- data.table(table(VOIX))
ggplot(VOIX, aes(x = VOIX, y = N, fill = VOIX)) +
  geom_bar(stat = "identity", position = "dodge") 


library(benford.analysis)
# Run Benford analysis
bfd <- benford(election_results$Voix, number.of.digits = 1)

# Plot the results
plot(bfd)

