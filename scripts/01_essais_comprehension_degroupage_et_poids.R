# analyses pour comprendre comment la base permet de calculer des densités de biomasse
# avec les différents types de lots, mesures individuelles etc.

library(tidyverse)
library(aspe)


load(file = "raw_data/toutes_tables_aspe_sauf_mei.RData")
load(file = "raw_data/mei.RData")

passerelle_mei <- mef_creer_passerelle() %>%
  left_join(
    y = mesure_individuelle %>%
      select(
        mei_id,
        lop_id = mei_lop_id,
        mei_taille,
        mei_poids,
        mei_poids_estime,
        mei_mesure_reelle,
        tlo_id = mei_tlo_id
      )
  ) %>%
  left_join(
    y = lot_poissons %>%
      select(
        lop_id,
        lop_esp_id,
        lop_tyl_id,
        lop_longueur_specimens_taille_mini,
        lop_longueur_specimens_taille_maxi,
        lop_poids,
        lop_poids_estime,
        lop_effectif,
        lop_tlo_id
      )
  ) %>%
  distinct()

save(passerelle_mei, file = "processed_data/passerelle_mei.RData")
load(file = "processed_data/passerelle_mei.RData")

# On complète la passerelle avec les champs utiles 
passerelle_mei_simp <- passerelle_mei %>% 
  select(lop_id,
         lop_tyl_id,
         lop_tlo_id,
         lop_esp_id,
         lop_poids,
         lop_effectif,
         lop_longueur_specimens_taille_mini,
         lop_longueur_specimens_taille_maxi,
         mei_id,
         mei_poids,
         mei_taille)

save(passerelle_mei_simp, file = "processed_data/passerelle_mei_simp.RData")

load(file = "processed_data/passerelle_mei_simp.RData")

rm(passerelle, passerelle_mei)

# Récupération des données sur les lots pour en vérifier la cohérence
# une ligne par lot
# auparavant on corrige qq pbs en cas de poids nul ou de type de longueur NA
# ATTENTION, 10' environ
lots <- passerelle_mei_simp %>% 
  mutate(lop_poids = ifelse(lop_poids == 0, NA, lop_poids), # poids de lots nuls
         mei_poids = ifelse(mei_poids == 0, NA, mei_poids), # poids ind nuls 
         lop_tlo_id = ifelse(is.na(lop_tlo_id), 2, lop_tlo_id)) %>%  # types de long NA => Lt 
  group_by(lop_id,
           lop_tyl_id,
           lop_tlo_id,
           lop_esp_id) %>% 
    summarise(lop_poids = head(lop_poids, 1),
              lop_effectif = head(lop_effectif, 1),
              lop_longueur_specimens_taille_mini = head(lop_longueur_specimens_taille_mini, 1),
              lop_longueur_specimens_taille_maxi = head(lop_longueur_specimens_taille_maxi, 1),
              lop_poids_moy = ifelse(test = is.na(lop_effectif),
                                     yes = NA,
                                     no = lop_poids / lop_effectif),
              mei_id_n = n_distinct(mei_id),
              mei_poids_n = length(na.omit(mei_poids)),
              mei_poids_moy = mean(mei_poids, na.rm = T),
              mei_taille_n = length(na.omit(mei_taille)),
              effectif_retenu = max(lop_effectif, mei_poids_n, mei_taille_n)
              ) %>%
  ungroup() %>% 
  left_join(y = ref_espece %>%
              select(lop_esp_id = esp_id, esp_code_alternatif)) %>% 
  select(-lop_esp_id)
  
save(lots, file = "processed_data/lots.RData")

# -------------------------------------------------------------------------------------
# qq vérifications
load(file = "processed_data/lots.RData")
# A-t-on toujours lop_effectif = n_distinct(mei_id) - effectif du lot = nb ind en mesure indiv ? NON
lots_mei_poids_manquants <- lots %>% 
  filter(lop_effectif != mei_id_n)

# un même lot peut-il avoir plusieuts type ? NON 
nb_tyl_id_par_lop_id <- lots %>% 
  group_by(lop_id) %>% 
    summarise(tyl_id_n = n_distinct(lop_tyl_id)) %>% 
  filter(tyl_id_n > 1)

# un même lot peut-il comprendre des Lt et des Lf ? NON
nb_tlo_id_par_lop_id <- lots %>% 
  group_by(lop_id) %>% 
    summarise(tlo_id_n = n_distinct(lop_tlo_id)) %>% 
  filter(tlo_id_n > 1)

lots_lop_poids_manquant <- lots %>% 
  filter(is.na(lop_poids))


# -------------------------------------------------------------------------------------
# Constitution d'une table associant lop_id et lop_poids_estime
# lop_poids_estimé est le produit effectif_retenu x lop_poids_moy
# il faut donc dans tous les cas calculer un lop_poids_moy

# gestion des cas 1 à 3 (Pas de recours à la relation taille-poids)

load(file = "raw_data/mei.RData")
gdata::keep(mesure_individuelle, sure = TRUE)
load(file = "processed_data/lots.RData")
lots_cas_1_a_3 <- lots %>% 
  mutate(lop_poids_moy_est = case_when(
    !is.na(lop_poids) & lop_effectif > 0 ~ lop_poids / lop_effectif, # cas 1
    lop_effectif == 1 | lop_tyl_id == 4 ~ mei_poids_moy, # cas 2
    (mei_poids_n > 10 | mei_poids_n > lop_effectif / 2) ~ mei_poids_moy, # cas 3
    TRUE ~ NA_real_
    ))

# gestion des cas 4 à 6 (recours à la relation taille-poids)
lots_cas_4_a_6 <- lots_cas_1_a_3 %>% 
  filter(is.na(lop_poids_moy_est))

lots_cas_1_a_3 <- lots_cas_1_a_3 %>% 
  filter(!is.na(lop_poids_moy_est))

mei_cas_4_a_6 <- lots_cas_4_a_6 %>% 
  left_join(y = mesure_individuelle %>% 
              rename(lop_id = mei_lop_id))

# nb de lignes mei sans taille (après vérif pas de cas avec slt le poids)
mei_a_completer %>% filter(is.na(mei_taille)) %>% nrow()

# chargement des relations taille-poids (au final, il faudra remettre les scripts dans l'ordre)

load(file = "processed_data/tp.RData")

tp <- tp %>% 
  select(esp_code_alternatif = code_espece, a, b, lop_tlo_id = tlo_id)

mei_cas_4_a_6 <- mei_cas_4_a_6 %>% 
  left_join(y = tp)

mei_cas_4_a_6_tp_exist_pour_tlo <- mei_cas_4_a_6 %>% 
  mutate(mei_poids_dapres_tp = a * (mei_taille / 10) ^ b)

mei_cas_4_a_6_tp_non_exist_pour_tlo <- mei_cas_4_a_6_tp_exist_pour_tlo %>% 
  filter(is.na(a)) %>% 
  mutate(lop_tlo_id = ifelse(lop_tlo_id == 1, 2, 1)) %>% 
  select(-(a:mei_poids_dapres_tp)) %>% 
  left_join(y = tp) %>% 
  mutate(mei_poids_dapres_tp = a * (mei_taille / 10) ^ b)

mei_cas_4_a_6_tp_non_exist_pour_tlo %>% sample_n(500) %>% View


gdata::keep(passerelle_mei, passerelle_mei_simp, sure = T)
object.size(passerelle_mei)/1e6

noms <- ls() %>% map(.f = function(x) names(get(x)))
noms %>% str_detect("mep_id")
