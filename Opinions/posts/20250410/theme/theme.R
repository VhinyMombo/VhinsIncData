# Installer si nécessaire
# install.packages("showtext")
# install.packages("sysfonts")

# Charger les packages
library(showtext)
library(sysfonts)

# Ajouter la police Oswald depuis Google Fonts
font_add_google(name = "Oswald", family = "Oswald")

# Activer l’utilisation de showtext pour les rendus
showtext_auto()

theme_labinc <- function() {
  theme_minimal(base_family = "Oswald") +  # Police utilisée pour tous les textes
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold",
        color = "#2c3e50"
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 13,
        face = "italic",
        color = "#34495e"
      ),
      plot.caption = element_text(
        size = 9,
        face = "italic",
        color = "#7f8c8d",
        hjust = 1
      ),
      axis.title = element_text(
        size = 12,
        face = "bold",
        color = "#2c3e50"
      ),
      axis.text = element_text(
        size = 10,
        color = "#2c3e50"
      ),
      axis.line = element_line(color = "#bdc3c7"),
      panel.grid.major = element_line(color = "#ecf0f1", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(
        face = "bold",
        size = 11,
        color = "#2c3e50"
      ),
      legend.text = element_text(size = 10, color = "#2c3e50"),
      legend.position = "top",
      plot.background = element_rect(fill = "#f0eeeb", color = NA),
      panel.background = element_rect(fill = "#f0eeeb", color = NA)
    )
}




theme_labinc2 <- function() {
  theme_minimal(base_family = "Oswald") +  # Police utilisée pour tous les textes
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 16,
        face = "bold",
        color = "#2c3e50"
      ),
      plot.subtitle = element_text(
        hjust = 0.5,
        size = 13,
        face = "italic",
        color = "#34495e"
      ),
      plot.caption = element_text(
        size = 9,
        face = "italic",
        color = "#7f8c8d",
        hjust = 1
      ),
      axis.title = element_text(
        size = 12,
        face = "bold",
        color = "#2c3e50"
      ),
      axis.text = element_text(
        size = 10,
        color = "#2c3e50"
      ),
      axis.line = element_line(color = "#bdc3c7"),
      panel.grid.major = element_line(color = "#ecf0f1", linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(
        face = "bold",
        size = 11,
        color = "#2c3e50"
      ),
      legend.text = element_text(size = 10, color = "#2c3e50"),
      legend.position = "right",
      plot.background = element_rect(fill = "#f0eeeb", color = NA),
      panel.background = element_rect(fill = "#f0eeeb", color = NA)
    )
}