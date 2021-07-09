library(ggiraph)
library(tidyverse)
library(aspe)

df <- capt_mef_colo_ext
interactif = FALSE
lr = "fr"
largeur = 6
hauteur = 5
pop = "10603"

couleurs_liste_rouge <- c("DD" = "#d1d1c6",
                          "LC" = "#60c659",
                          "NT" = "#cce226",
                          "VU" = "#f9e81e",
                          "EN" = "#fc7f3f",
                          "CR" = "#db1e05",
                          "EW" = "#542344",
                          "EX" = "black")

gg_colo_ext_pops2(df = capt_mef_colo_ext %>% filter(pop_id == pop),
                  interactif = TRUE,
                  lr = "fr",
                  largeur = 6,
                  hauteur = 5)

gg_colo_ext_pops2 <- function(df, interactif = FALSE, lr = NA, largeur = 6, hauteur = 5, ...)
  
{
  # mise en forme des étiquettes inspirée de https://stackoverflow.com/a/57086284
  int_breaks <- function(x, n = 5){
    if (length(unique(x)) > 1) {
      pretty(x, n)[round(pretty(x, n), 1) %% 1 == 0]
    } else {
      round(unique(x)) + c(-1, 0, 1)
    }
  }
  
  int_limits <- function(x) {
    if (length(unique(x)) > 1) {
      range(x)
    } else {
      range(int_breaks(x))
    }
  }
  
  create_graph <- function(pop, df, interactive, lr) {
    df_pop <- df %>%
      filter(pop_id == pop) %>%
      rowwise() %>%
      mutate(
        hover = shiny::HTML(
          paste0(
            esp_code_alternatif, " (", annee, ")<br>",
            "Effectif: ", effectif
          )
        )
      ) %>%
      ungroup()
    
    if(lr == "fr") {
      
      data(liste_rouge)
      
      df_pop <- df_pop %>% 
        left_join(liste_rouge %>% 
                    select(esp_code_alternatif,
                           statut = statut_lr_fr))
    }
    
    if(lr == "eu") {
      
      data(liste_rouge)
      
      df_pop <- df_pop %>% 
        left_join(liste_rouge %>% 
                    select(esp_code_alternatif,
                           statut = statut_lr_eu))
    }
    
    if(lr == "int") {
      
      data(liste_rouge)
      
      df_pop <- df_pop %>% 
        left_join(liste_rouge %>% 
                    select(esp_code_alternatif,
                           statut = statut_lr_int))
    }
    
    # df_pop <- df_pop %>%
    #   mutate(esp_label = paste("<span style = 'color: ",
    #                                   case_when(statut == "DD" ~ couleurs_liste_rouge["DD"],
    #                                             statut == "LC" ~ couleurs_liste_rouge["LC"],
    #                                             statut == "NT" ~ couleurs_liste_rouge["NT"],
    #                                             statut == "VU" ~ couleurs_liste_rouge["VU"],
    #                                             statut == "EN" ~ couleurs_liste_rouge["EN"],
    #                                             statut == "CR" ~ couleurs_liste_rouge["CR"],
    #                                             statut == "EW" ~ couleurs_liste_rouge["EW"],
    #                                             statut == "EX" ~ couleurs_liste_rouge["EX"],
    #                                             TRUE ~ "black"),
    #                                   ";'>",
    #                            esp_code_alternatif,
    #                                   "</span>", sep = ""))
    

    
    df_pop <- df_pop %>%
      # select(esp_code_alternatif, statut) %>% 
      # distinct() %>%
      mutate(esp_col = case_when(statut == "DD" ~ couleurs_liste_rouge["DD"],
                                   statut == "LC" ~ couleurs_liste_rouge["LC"],
                                   statut == "NT" ~ couleurs_liste_rouge["NT"],
                                   statut == "VU" ~ couleurs_liste_rouge["VU"],
                                   statut == "EN" ~ couleurs_liste_rouge["EN"],
                                   statut == "CR" ~ couleurs_liste_rouge["CR"],
                                   statut == "EW" ~ couleurs_liste_rouge["EW"],
                                   statut == "EX" ~ couleurs_liste_rouge["EX"],
                                   TRUE ~ "black")) #%>% 
      # arrange(esp_code_alternatif) %>% 
      # pull(esp_label)
    
    # esp_col <- data.frame(esp_col$esp_label,
    #                      names = esp_col$esp_code_alternatif) %>%
    #   arrange(esp_label)
    
    libelle <- df_pop %>%
      pull(pop_libelle) %>%
      na.omit() %>%
      .[1]
    
    gg <- ggplot(
      data = df_pop,
      aes(
        x = annee,
        y = esp_code_alternatif,
        size = diam_point,
        color = col_ext,
        shape = type_point
      )
    ) +
      geom_point_interactive(
        aes(
          tooltip = hover
        )
      ) +
      labs(
        x = "",
        y = "",
        col = "",
        title = libelle
      ) +
      scale_color_manual(
        values = c(
          "colonisation" = "#00B81F",
          "extinction" = "red",
          "statu quo" = "darkblue"
        ),
        na.value = "grey10"
      ) +
      scale_x_continuous(
        breaks = int_breaks,
        limits = int_limits
      ) +
      guides(shape = FALSE, size = FALSE) +
      theme(
        legend.position = "none",
        axis.text.y = element_text(hjust = 0),
        axis.text.x = element_text(colour = df_pop %>%
                                       select(esp_code_alternatif, esp_col) %>% 
                                       distinct() %>% 
                                       arrange(esp_code_alternatif) %>%
                                       pull(esp_col))
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank()
      )
    
    if (interactive) {
      girafe(
        ggobj = gg,
        width_svg = largeur,
        height_svg = hauteur,
        options = list(
          opts_sizing(...)
        )
      )
    } else {
      gg
    }
  }
  
  
  graphs <- df %>%
    pull(pop_id) %>%
    unique() %>%
    map(
      .f = create_graph,
      df = df,
      interactive = interactif,
      lr = lr
    ) %>%
    set_names(unique(df$pop_id))
  
  if (length(graphs) == 1) {
    graphs[[1]]
  } else {
    graphs
  }
  
  
}
