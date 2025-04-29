library(data.table)
library(gganimate)
library(av)

source("case-studies/posts/20250329/theme/theme.R")

# Liste de toutes les intentions avec date associée
intentions_list <- list(
  "2024-04-08 - 17:00" = list(
    "Alain Simplice Bongouères" = 48,
    "Alain Claude\n Billie Bi Nze" = 191,
    "Axel Stophène Ibinga Ibinga" = 13,
    "Brice Clotaire\n Oligui Nguema" = 336,
    "Zenaba Gninga Chaning" = 15,
    "Joseph Lapensée Essingone" = 15,
    "Stephane Germain Iloko" = 5,
    "Thierry Yvon Michel Nguema" = 5,
    "Bulletin blanc" = 18
  ),
  "2024-04-09 - 00:00" = list(
    "Alain Simplice Bongouères" = 102,
    "Alain Claude\n Billie Bi Nze" = 399,
    "Axel Stophène Ibinga Ibinga" = 21,
    "Brice Clotaire\n Oligui Nguema" = 669,
    "Zenaba Gninga Chaning" = 27,
    "Joseph Lapensée Essingone" = 31,
    "Stephane Germain Iloko" = 14,
    "Thierry Yvon Michel Nguema" = 9,
    "Bulletin blanc" = 60
  ), 
  "2024-04-09 - 09:00" = list(
    "Alain Simplice Bongouères" = 112,
    "Alain Claude\n Billie Bi Nze" = 426,
    "Axel Stophène Ibinga Ibinga" = 21,
    "Brice Clotaire\n Oligui Nguema" = 736,
    "Zenaba Gninga Chaning" = 29,
    "Joseph Lapensée Essingone" = 32,
    "Stephane Germain Iloko" = 16,
    "Thierry Yvon Michel Nguema" = 10,
    "Bulletin blanc" = 65
  ),
  
  
  "2024-04-09 - 12:00" = list(
    "Alain Simplice Bongouères" = 114,
    "Alain Claude\n Billie Bi Nze" = 440,
    "Axel Stophène Ibinga Ibinga" = 22,
    "Brice Clotaire\n Oligui Nguema" = 769,
    "Zenaba Gninga Chaning" = 30,
    "Joseph Lapensée Essingone" = 34,
    "Stephane Germain Iloko" = 16,
    "Thierry Yvon Michel Nguema" = 10,
    "Bulletin blanc" = 66
  ),
  
  "2024-04-09 - 15:00" = list(
    "Alain Simplice Bongouères" = 115,
    "Alain Claude\n Billie Bi Nze" = 454,
    "Axel Stophène Ibinga Ibinga" = 23,
    "Brice Clotaire\n Oligui Nguema" = 799,
    "Zenaba Gninga Chaning" = 31,
    "Joseph Lapensée Essingone" = 35,
    "Stephane Germain Iloko" = 16,
    "Thierry Yvon Michel Nguema" = 10,
    "Bulletin blanc" = 68
  ), 
  
  "2024-04-09 - 22:30" = list(
    "Alain Simplice Bongouères" = 116,
    "Alain Claude\n Billie Bi Nze" = 476,
    "Axel Stophène Ibinga Ibinga" = 24,
    "Brice Clotaire\n Oligui Nguema" = 846,
    "Zenaba Gninga Chaning" = 33,
    "Joseph Lapensée Essingone" = 38,
    "Stephane Germain Iloko" = 17,
    "Thierry Yvon Michel Nguema" = 11,
    "Bulletin blanc" = 71
  ), 
  
  "2024-04-10 - 10:00" = list(
    "Alain Simplice Bongouères" = 121,
    "Alain Claude\n Billie Bi Nze" = 499,
    "Axel Stophène Ibinga Ibinga" = 28,
    "Brice Clotaire\n Oligui Nguema" = 903,
    "Zenaba Gninga Chaning" = 34,
    "Joseph Lapensée Essingone" = 40,
    "Stephane Germain Iloko" = 17,
    "Thierry Yvon Michel Nguema" = 12,
    "Bulletin blanc" = 75
  ), 
  
  
  "2024-04-10 - 17:00" = list(
    "Alain Simplice Bongouères" = 120,
    "Alain Claude\n Billie Bi Nze" = 510,
    "Axel Stophène Ibinga Ibinga" = 29,
    "Brice Clotaire\n Oligui Nguema" = 936,
    "Zenaba Gninga Chaning" = 37,
    "Joseph Lapensée Essingone" = 44,
    "Stephane Germain Iloko" = 17,
    "Thierry Yvon Michel Nguema" = 13,
    "Bulletin blanc" = 77
  )

)

# Transformation en data.table
dt.intentions <- rbindlist(lapply(names(intentions_list), function(d) {
  data.table(
    candidat = names(intentions_list[[d]]),
    intentions = unlist(intentions_list[[d]], use.names = FALSE),
    date = (d)
  )
}))

# Aperçu
print(dt.intentions)

##############


###########


dt.intentions[, pct := intentions / sum(intentions) * 100, by = date]

# View the result
print(dt.intentions)


dt.intentions[, total_intentions := sum(intentions), by = date]

total_intentions <- max(dt.intentions$total_intentions)

# Identifier le gagnant
dt.intentions[, gagnant := ifelse(intentions == max(intentions), "Gagnant", "Autres"),  by = date]
dt.intentions[candidat == "Bulletin blanc", gagnant := "Bulletin Blanc"]




###### ME
n <- total_intentions
N <- 868115
p <- 0.5
z <- 2.58 ## ("95%")
z <- 1.96 ## ("95%")


ME <- z * sqrt(p * (1 - p) / n) * sqrt((N - n) / (N - 1))
ME_percent <- round(ME * 100, 1)




# Calcul de la marge d’erreur pour chaque date
dt.intentions[, ME := round(
  z * sqrt(p * (1 - p) / total_intentions) * sqrt((N - total_intentions) / (N - 1)) * 100,
  1
), by = date]


dt.intentions[, facet_label := paste0(
  format(as.POSIXct(date, format = "%Y-%m-%d - %H:%M"), "%d/%m à %Hh"),
  "\n", total_intentions, " participants | ±", ME, "%"
), by = date]



# dt.intentions[, facet_label := paste0(
#   format(as.POSIXct(date, format = "%Y-%m-%d - %H:%M"), "%d/%m %Hh"),  # Date formatée
#   "\nTotal votants : ", total_intentions,
#   "\nMarge d’erreur estimée : ±", round(1.96 * sqrt(0.25 / total_intentions) * 100, 1), "%",
#   "\nRésultats issus d’un sondage ouvert en ligne.",
#   "\n⚠️ Non représentatif : participation libre et biais d’auto-sélection.",
#   "\nLes intentions peuvent évoluer avec le temps et la visibilité des résultats."
# ), by = date]

last_4_dates <- dt.intentions[, sort(unique(date), decreasing = TRUE)][1:4]

# Filter for these dates
dt_last4 <- dt.intentions[date %in% last_4_dates]

# Ajouter le texte au-dessus des barres
ggplot(dt.intentions, aes(x = reorder(candidat, -pct), y = pct, fill = gagnant)) +
# ggplot(dt.intentions[date %in% last_4_dates], aes(x = reorder(candidat, -pct), y = pct, fill = gagnant)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("Gagnant" = "#872916", 
                               "Autres" = "#2c3e50", 
                               "Bulletin Blanc" = "#ffffff")) +
  facet_wrap(facet_label ~ .) +  
  labs(
    title = "Évolution des intentions de vote",
    subtitle = "Chaque panel indique la date, le nombre de réponses et la marge d'erreur (±)",
    x = "Candidat",
    y = "Pourcentage (%)",
    fill = "Statut",
    caption = sprintf("Source : Sondage public (9tv).\n Analysis: Les résultats étant visibles avant le vote, ils peuvent influencer les participants et introduire un biais non pris en compte dans la marge d’erreur. \nCette dernière est calculée selon une hypothèse de proportion maximale (p = %s) sur le nombre de participants, pour un fichier electoral estimée à %s inscrits.", 
    p, N)) + 
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, max(dt.intentions$pct) + 10)) +
  theme_labinc() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# 
# 
# # Extraire les colonnes nécessaires
# # evolution_dt <- dt.intentions[, .(candidat, pct, date = as.POSIXct(date, format = "%Y-%m-%d - %H:%M"))]
# 
evolution_dt <- dt.intentions[
  candidat != "Bulletin blanc",
  .(candidat, pct, date = as.POSIXct(date, format = "%Y-%m-%d - %H:%M"), ME)
]

# # Ajouter les bornes d'incertitude
evolution_dt[, `:=`(
  lower = pmax(0, pct - ME),
  upper = pmin(100, pct + ME)
)]
# 
# ggplot(evolution_dt[candidat != "Bulletin blanc"], aes(x = date, y = pct, color = candidat, group = candidat)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 2) +
#   scale_y_continuous(labels = scales::percent_format(scale = 1)) +
#   labs(
#     title = "Évolution temporelle des intentions de vote",
#     subtitle = "Basé sur les différents relevés du sondage 9tv",
#     x = "Date",
#     y = "Pourcentage (%)",
#     color = "Candidat",
#     caption = "Attention : les résultats visibles en amont peuvent influencer les votes. Courbes lissées selon les déclarations successives."
#   ) +
#   theme_labinc() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(face = "bold", hjust = 0.5)
#   )
# 
# 
evolution_dt[, date_label := format(date, "%d/%m %Hh")]
# 
# # 
ggplot(evolution_dt[(candidat %in% c("Brice Clotaire\n Oligui Nguema", "Alain Claude\n Billie Bi Nze")), ], aes(x = factor(date_label), y = pct, color = candidat, group = candidat)) +
  # Ribbon pour la marge d'erreur
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = candidat), alpha = 0.15, colour = NA) +
  # Courbe d'évolution
  geom_line(linewidth = 1) +
  # Points pour chaque date
  geom_point(size = 2) +

  # Échelle en pourcentage
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +

  labs(
    title = "Évolution temporelle des intentions de vote avec incertitude",
    subtitle = "Les zones en transparence indiquent la marge d’erreur estimée (±)",
    x = "Date",
    y = "Pourcentage (%)",
    color = "Candidat",
    fill = "Candidat",
    caption = "Source : Sondage 9tv. La marge d’erreur est estimée à partir d’un échantillon libre et non représentatif."
  ) +

  theme(
    theme(axis.text.x = element_text(angle = 45, hjust = 1)),
    plot.title = element_text(face = "bold", hjust = 0.5)
  ) +

  theme_labinc()


############


# 
# # Assure-toi que la date est au bon format
# dt.intentions[, date_fmt := as.POSIXct(date, format = "%Y-%m-%d - %H:%M")]
# 
# # Création du plot
# p_anim <- ggplot(dt.intentions, aes(x = reorder(candidat, -pct), y = pct, fill = gagnant)) +
#   geom_col() +
#   geom_text(aes(label = paste0(round(pct, 1), "%")), vjust = -0.5, size = 4) +
#   scale_fill_manual(values = c("Gagnant" = "#872916", "Autres" = "#2c3e50", "Bulletin Blanc" = "#ffffff")) +
#   # scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, max(dt.intentions$pct) + 10)) +
#   labs(
#     title = "Évolution des intentions de vote",
#     subtitle = "Date : {format(as.POSIXct(closest_state), '%d/%m à %Hh')} — {round(unique(dt.intentions[date == closest_state]$ME), 1)}% de marge d’erreur",
#     caption = "Source : Sondage public (9tv). Résultats visibles avant vote pouvant induire un biais d'influence.",
#     x = "Candidat",
#     y = "Pourcentage (%)",
#     fill = "Statut"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     plot.title = element_text(face = "bold", hjust = 0.5)
#   ) +
#   transition_states(date_fmt, transition_length = 2, state_length = 1) +
#   ease_aes('cubic-in-out')
# 
# # Exporter l'animation (format .gif)
# animate(p_anim, width = 1000, height = 600, duration = 10, fps = 2, renderer = av_renderer("intentions_vote.mp4"))
# 
