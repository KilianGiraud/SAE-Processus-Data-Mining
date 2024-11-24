library(dplyr)
library(tidyr)
data <- read.csv("croissance.csv")
data <- na.omit(data)
IMC_taille_poids_f <- read.csv("IMC_taille_poids_f.csv")
IMC_taille_poids_m <- read.csv("IMC_taille_poids_m.csv")
data$IMC <- data$poids/(data$taille/100)^2
data$sexe <- as.factor(data$sexe)
data$ind <- as.factor(data$ind)

data_m <- data[data$sexe=="M",]
data_f <- data[data$sexe=="F",]

mean(data_f$age)    
mean(data_m$age)

mean(data_f$taille)
mean(data_m$taille)

mean(data_f$poids)
mean(data_m$poids)

mean(data_f$IMC)
mean(data_m$IMC)
summary(data_f)
summary(data_m)

new_data <- read.csv("new_data.csv")
new_data_clean <- read.csv("new_data_clean.csv")
new_data$taille <- round(new_data$taille, 2)
new_data$poids <- round(new_data$poids, 2)





























# Calculer l'IMC pour new_data
new_data$IMC <- new_data$poids / (new_data$taille / 100)^2


# Fonction pour ajouter une colonne IMC_indic
ajouter_IMC_indic <- function(df) {
  df$IMC_indic <- ifelse(df$IMC < 18, "Faible",
                         ifelse(df$IMC <= 35, "Moyen", "Fort"))
  return(df)
}

new_data_clean <- ajouter_IMC_indic(new_data_clean)



# Calcul des évolutions :


# Charger les fichiers CSV
IMC_taille_poids_f <- read.csv("IMC_taille_poids_f.csv")
IMC_taille_poids_m <- read.csv("IMC_taille_poids_m.csv")


# Liste des colonnes pour lesquelles on veut calculer les évolutions (toutes sauf "Age")
colonnes <- c("IMC1", "IMC2", "IMC_maigre", "IMC_surpoids", "IMC_obese",
              "Poids1", "Poids2", "Poids_faible", "Poids_fort",
              "Taille1", "Taille2", "Taille_faible", "Taille_forte")

# Calculer l'évolution pour chaque colonne et ajouter une nouvelle colonne avec le suffixe "_evo"
for (col in colonnes) {
  IMC_taille_poids_f[[paste0(col, "_evo")]] <- c(NA, diff(IMC_taille_poids_f[[col]]))
}

# Calculer l'évolution pour chaque colonne et ajouter une nouvelle colonne avec le suffixe "_evo"
for (col in colonnes) {
  IMC_taille_poids_m[[paste0(col, "_evo")]] <- c(NA, diff(IMC_taille_poids_m[[col]]))
}


# Sélection des colonnes pour chaque catégorie (uniquement les valeur 1 et 2 car ils représentes l'évolution pour 95% des enfants, les "faibles" et "fort" et autres reste des valeurs extrême.)
colonnes_IMC <- c("IMC1_evo", "IMC2_evo")
colonnes_Poids <- c("Poids1_evo", "Poids2_evo")
colonnes_Taille <- c("Taille1_evo", "Taille2_evo")

# Calcul de la moyenne des évolutions par catégorie fille
IMC_taille_poids_f$IMC_moyenne_evo <- rowMeans(IMC_taille_poids_f[, colonnes_IMC], na.rm = TRUE)
IMC_taille_poids_f$Poids_moyenne_evo <- rowMeans(IMC_taille_poids_f[, colonnes_Poids], na.rm = TRUE)
IMC_taille_poids_f$Taille_moyenne_evo <- rowMeans(IMC_taille_poids_f[, colonnes_Taille], na.rm = TRUE)

# Calcul de la moyenne des évolutions par catégorie garçon
IMC_taille_poids_m$IMC_moyenne_evo <- rowMeans(IMC_taille_poids_m[, colonnes_IMC], na.rm = TRUE)
IMC_taille_poids_m$Poids_moyenne_evo <- rowMeans(IMC_taille_poids_m[, colonnes_Poids], na.rm = TRUE)
IMC_taille_poids_m$Taille_moyenne_evo <- rowMeans(IMC_taille_poids_m[, colonnes_Taille], na.rm = TRUE)


# Générer un vecteur de 18 valeurs uniformément réparties de 0.5 à 1.0 pour les marges d'erreurs en fonction de l'âge (cela augmente car avec l'age l'évolution diminue.)
uniform_vector <- seq(0.5, 1.0, length.out = 18)


# Sélectionner les colonnes d'évolution dans le dataframe
colonnes_evo <- grep("_evo$", colnames(IMC_taille_poids_f), value = TRUE)

# Initialiser un nouveau dataframe pour stocker les résultats
result_df <- data.frame(Age = IMC_taille_poids_f$Age)

# Boucle pour créer les colonnes avec marges d'erreur pour chaque colonne d'évolution
for (col in colonnes_evo) {
  # Calcul des valeurs "bas" et "haut" avec la marge d'erreur
  result_df[[paste0(col, "_bas")]] <- IMC_taille_poids_f[[col]] * (1 - uniform_vector)
  result_df[[paste0(col, "_haut")]] <- IMC_taille_poids_f[[col]] * (1 + uniform_vector)
}

# Supprimer la première ligne correspondant à l'âge 1 (pas de valeurs précédentes)
result_df <- result_df[-1, ]


# Créer borne_age avec les âges min et max pour chaque individu, avec arrondis spécifiés
borne_age <- data %>%
  group_by(ind) %>%
  summarize(
    Age_min = ceiling(min(age)),
    Age_max = floor(max(age))
  )

# Identifier les âges présentant des "trous" de plus de 2 ans dans `data`
trous <- data %>%
  arrange(ind, age) %>%
  group_by(ind) %>%
  mutate(diff_age = lead(age) - age) %>%
  filter(diff_age >= 2) %>%
  select(ind, age, diff_age)

# Créer une table des âges troués pour chaque individu
ages_troues <- trous %>%
  rowwise() %>%
  mutate(ages_interpolated = list(seq(age + 1, age + diff_age - 1))) %>%
  select(ind, ages_interpolated) %>%
  unnest(ages_interpolated) %>%
  rename(age = ages_interpolated)

# Joindre avec borne_age et `ages_troues` pour filtrer les âges hors bornes et dans les trous
new_data_hors_borne <- new_data %>%
  left_join(borne_age, by = "ind") %>%
  filter(age < Age_min | age > Age_max) %>%
  bind_rows(
    new_data %>%
      inner_join(ages_troues, by = c("ind", "age"))
  ) %>%
  bind_rows(
    new_data %>%
      filter(taille < 60 | poids < 6.5)  # Ajouter les conditions pour taille et poids (en fonction des données de IMC_taille_poids)
  )

new_data_hors_borne <- new_data_hors_borne %>%
  select(-Age_min, -Age_max)

new_data_hors_borne <- new_data_hors_borne %>% distinct()


# Obtenir `new_data_dans_borne` en excluant les lignes présentes dans `new_data_hors_borne`
new_data_dans_borne <- anti_join(new_data, new_data_hors_borne, by = c("ind", "age", "taille", "poids", "IMC", "sexe"))


# Vérifier les poids négatifs dans `new_data_dans_borne`
poids_negatifs <- new_data_dans_borne %>%
  filter(poids < 0)

# Ajouter les lignes avec poids négatif à `new_data_hors_borne`
new_data_hors_borne <- bind_rows(new_data_hors_borne, poids_negatifs)

# Supprimer les lignes avec poids négatif de `new_data_dans_borne`
new_data_dans_borne <- new_data_dans_borne %>%
  filter(poids >= 0)


###
# ATTENTION CETTE PARTIE PREND BEAUCOUP DE TEMPS A S EXECUTER ENVIRON 20 MIN DONC IL Y A UN load data a la fin de cette partie 
###

trouver_bornes_age <- function(row, ref_data) {
  # Extraire les valeurs courantes
  ind_courant <- row$ind
  age_courant <- row$age
  
  # Filtrer ref_data pour le même individu
  ref_ind_data <- ref_data %>% filter(ind == ind_courant)
  
  # Trouver la borne inférieure (la plus grande valeur d'âge inférieure à l'âge courant)
  borne_inf <- ref_ind_data %>%
    filter(age < age_courant) %>%
    arrange(desc(age)) %>%
    slice(1)
  
  # Trouver la borne supérieure (la plus petite valeur d'âge supérieure à l'âge courant)
  borne_sup <- ref_ind_data %>%
    filter(age > age_courant) %>%
    arrange(age) %>%
    slice(1)
  
  # Extraire les informations de chaque borne ou assigner NA si la borne n'existe pas
  list(
    age_inf = ifelse(nrow(borne_inf) > 0, borne_inf$age, NA),
    age_sup = ifelse(nrow(borne_sup) > 0, borne_sup$age, NA),
    taille_inf = ifelse(nrow(borne_inf) > 0, borne_inf$taille, NA),
    taille_sup = ifelse(nrow(borne_sup) > 0, borne_sup$taille, NA),
    poids_inf = ifelse(nrow(borne_inf) > 0, borne_inf$poids, NA),
    poids_sup = ifelse(nrow(borne_sup) > 0, borne_sup$poids, NA)
  )
}


# Appliquer la fonction à chaque ligne de new_data_hors_borne
new_data_hors_borne <- new_data_hors_borne %>%
  rowwise() %>%
  mutate(bornes = list(trouver_bornes_age(cur_data(), new_data_dans_borne))) %>%
  unnest_wider(bornes)

# Afficher les premières lignes pour vérifier
head(new_data_hors_borne)

save(new_data_hors_borne, new_data_dans_borne, file = "new_data_hors_borne.RData")

# Charger les dataframes depuis le fichier .RData
load("new_data_hors_borne.RData")


# Filtrer les données de new_data_hors_borne pour ne garder que celles avec taille et poids dans les bornes
new_data_test <- new_data_hors_borne %>%
  filter(
    !is.na(taille_inf) & !is.na(taille_sup) & 
      !is.na(poids_inf) & !is.na(poids_sup) & 
      taille >= taille_inf & taille <= taille_sup &
      poids >= poids_inf & poids <= poids_sup
  )

new_data_to_add <- new_data_test %>%
  select(ind, sexe, age, taille, poids, IMC)

new_data_dans_borne <- bind_rows(new_data_dans_borne, new_data_to_add)



# Filtrer les données de new_data_hors_borne pour exclure les lignes déjà transférées dans new_data_dans_borne
new_data_hors_borne <- new_data_hors_borne %>%
  anti_join(new_data_to_add, by = c("ind", "sexe", "age", "taille", "poids", "IMC"))





# Identifier les individus avec une diminution de la taille entre l'âge N et N+1
taille_diminue <- new_data_dans_borne %>%
  arrange(ind, age) %>% # Trier par individu et âge
  group_by(ind) %>% # Grouper par individu
  mutate(taille_diff = taille - lag(taille)) %>% # Calculer la différence de taille avec l'âge précédent
  filter(taille_diff < 0) %>% # Filtrer les cas où la taille diminue
  ungroup() # Dégrouper pour retourner un dataframe simple


# Ajouter les données de `taille_diminue` dans `new_data_hors_borne`
new_data_hors_borne <- bind_rows(new_data_hors_borne, taille_diminue)

# Supprimer les données de `taille_diminue` de `new_data_dans_borne`
new_data_dans_borne <- anti_join(new_data_dans_borne, taille_diminue, by = c("ind", "age", "taille", "poids", "IMC", "sexe"))







# Pour sauvegarder les data
#write.csv(new_data_hors_borne, "new_data_hors_borne.csv")
#write.csv(new_data_dans_borne, "new_data_dans_borne.csv")
#new_data_hors_borne <- read.csv("new_data_hors_borne.csv")



################################################################################
# Début de la partie 2 correction des données à risque
################################################################################


# Étape 1 : Classement des individus par âge pour la taille et le poids
classements <- new_data_dans_borne %>%
  group_by(age) %>%
  arrange(taille) %>%
  mutate(classement_taille = row_number()) %>%
  arrange(poids) %>%
  mutate(classement_poids = row_number()) %>%
  ungroup()

# Étape 2 : Calcul de la moyenne des classements pour chaque individu
classement_moyen <- classements %>%
  group_by(ind) %>%
  summarize(
    moyenne_classement_taille = mean(classement_taille, na.rm = TRUE),
    moyenne_classement_poids = mean(classement_poids, na.rm = TRUE)
  )

# Étape 3 : Classement global séparé pour la taille et le poids
classement_final_taille <- classement_moyen %>%
  arrange(moyenne_classement_taille) %>%
  mutate(classement_taille_global = row_number())

classement_final_poids <- classement_moyen %>%
  arrange(moyenne_classement_poids) %>%
  mutate(classement_poids_global = row_number())

# Combiner les classements pour un tableau final
classement_final <- classement_final_taille %>%
  select(ind, classement_taille_global, moyenne_classement_taille) %>%
  inner_join(
    classement_final_poids %>% select(ind, classement_poids_global, moyenne_classement_poids),
    by = "ind"
  )




# Compter le nombre de données par âge et par sexe pour choisir le nombre d'individu types
compte_par_sexe_age <- new_data_dans_borne %>%
  group_by(sexe, age) %>%
  summarize(
    nombre_donnees = n(),
    .groups = "drop"
  )



#Créer des individus types pour combler les trous dans les bornes inférieure ou supérieure

# Étape 1 : Ajouter des classes pour la taille et le poids
classement_final <- classement_final %>%
  mutate(
    classe_taille = ntile(classement_taille_global, 20), # Classes du plus petit au plus grand pour la taille
    classe_poids = ntile(classement_poids_global, 20)   # Classes du plus léger au plus lourd pour le poids
  )

# Étape 2 : Calculer les moyennes pour les garçons
moyennes_garcons_taille <- new_data_dans_borne %>%
  filter(sexe == "M") %>%
  inner_join(classement_final, by = "ind") %>%
  group_by(classe_taille, age) %>%
  summarize(
    taille_moyenne = mean(taille, na.rm = TRUE), .groups = "drop"
  )

moyennes_garcons_poids <- new_data_dans_borne %>%
  filter(sexe == "M") %>%
  inner_join(classement_final, by = "ind") %>%
  group_by(classe_poids, age) %>%
  summarize(
    poids_moyen = mean(poids, na.rm = TRUE), .groups = "drop"
  )

# Étape 3 : Calculer les moyennes pour les filles
moyennes_filles_taille <- new_data_dans_borne %>%
  filter(sexe == "F") %>%
  inner_join(classement_final, by = "ind") %>%
  group_by(classe_taille, age) %>%
  summarize(
    taille_moyenne = mean(taille, na.rm = TRUE), .groups = "drop"
  )

moyennes_filles_poids <- new_data_dans_borne %>%
  filter(sexe == "F") %>%
  inner_join(classement_final, by = "ind") %>%
  group_by(classe_poids, age) %>%
  summarize(
    poids_moyen = mean(poids, na.rm = TRUE), .groups = "drop"
  )

# Étape 4 : Combiner les moyennes pour les garçons
individus_types_garcons <- full_join(
  moyennes_garcons_taille %>% rename(classe = classe_taille, taille = taille_moyenne),
  moyennes_garcons_poids %>% rename(classe = classe_poids, poids = poids_moyen),
  by = c("classe", "age")
) %>%
  mutate(individu_type = paste0("garcon_type_", classe))

# Étape 5 : Combiner les moyennes pour les filles
individus_types_filles <- full_join(
  moyennes_filles_taille %>% rename(classe = classe_taille, taille = taille_moyenne),
  moyennes_filles_poids %>% rename(classe = classe_poids, poids = poids_moyen),
  by = c("classe", "age")
) %>%
  mutate(individu_type = paste0("fille_type_", classe))

# Étape 6 : Résultat final
individus_types <- bind_rows(individus_types_garcons, individus_types_filles)


##########
#Associé chaque individu de new_data_hors_borne à un individu type
##########


# Étape 1 : Associer les classes aux individus dans `new_data_hors_borne`
classes_ind_hors_borne <- new_data_hors_borne %>%
  select(ind) %>%  # On ne garde que les identifiants
  distinct() %>%   # Supprime les doublons
  left_join(
    new_data_dans_borne %>%
      inner_join(classement_final, by = "ind") %>%
      select(ind, classe_taille, classe_poids),  # On ne garde que les classes pertinentes
    by = "ind"
  )

# Étape 2 : Trouver la classe dominante pour chaque individu
classes_dominantes <- classes_ind_hors_borne %>%
  group_by(ind) %>%
  summarize(
    classe_taille_dominante = as.integer(names(which.max(table(classe_taille)))), # Classe taille la plus fréquente
    classe_poids_dominante = as.integer(names(which.max(table(classe_poids))))    # Classe poids la plus fréquente
  )

# Étape 3 : Associer l’individu type correspondant
individus_hors_borne_avec_types <- classes_dominantes %>%
  mutate(
    individu_type_taille = paste0("type_taille_", classe_taille_dominante),
    individu_type_poids = paste0("type_poids_", classe_poids_dominante)
  )




##########
#Subdiviser new_data_hors_borne en trois pour les trois cas spécifiques de données à modifier
##########



# Étape 1 : Créer le deuxième dataframe
df_deuxieme <- new_data_hors_borne %>%
  filter(
    is.na(taille_inf) | is.na(taille_sup) | is.na(poids_inf) | is.na(poids_sup)
  ) %>%
  group_by(ind) %>%
  filter(n() > 1) %>%
  ungroup()

# Étape 2 : Créer le troisième dataframe
df_troisieme <- new_data_hors_borne %>%
  filter(
    age %in% c(1, 18),
    is.na(taille_inf) | is.na(taille_sup) | is.na(poids_inf) | is.na(poids_sup)
  )

# Étape 3 : Supprimer les données de `df_deuxieme` ayant un âge de 1 ou 18
df_deuxieme <- df_deuxieme %>%
  filter(!age %in% c(1, 18))

# Étape 4 : Créer le premier dataframe
df_premier <- anti_join(
  new_data_hors_borne,
  bind_rows(df_deuxieme, df_troisieme),
  by = colnames(new_data_hors_borne)
)

##########
# Correction des données du poids et de la taille
##########

###
# Modifier df_premier pour calculer la moyenne de taille et poids
###
df_premier <- df_premier %>%
  mutate(
    taille = round((taille_inf + taille_sup) / 2, 2),  # Moyenne des bornes taille 
    poids = round((poids_inf + poids_sup) / 2, 2)      # Moyenne des bornes poids 
  ) %>%
  select(-IMC)  



###
# Modifier df_troisième pour calculer la moyenne de taille et poids
###


# Ajouter une colonne sexe en fonction de la valeur de individu_type
individus_types <- individus_types %>%
  mutate(sexe = ifelse(grepl("garcon", individu_type), "M",
                       ifelse(grepl("fille", individu_type), "F", NA)))

individus_types <- individus_types %>%
  rename(
    taille_type = taille,
    poids_type = poids
  )

# Étape 1 : Réinitialiser les colonnes taille et poids dans df_troisieme
df_troisieme <- df_troisieme %>%
  mutate(
    taille = NA,
    poids = NA,
    IMC = NA
  )

# Étape 2 : Associer les classes taille et poids depuis classement_final
df_troisieme <- df_troisieme %>%
  left_join(
    classement_final %>%
      select(ind, classe_taille, classe_poids),
    by = "ind"
  )

# Étape 3 : Associer les tailles depuis individus_types en fonction de la classe_taille et du sexe
df_troisieme <- df_troisieme %>%
  left_join(
    individus_types %>%
      select(classe, age, sexe, taille_type) %>%
      rename(classe_taille = classe),
    by = c("classe_taille", "age", "sexe")
  )

# Étape 4 : Associer les poids depuis individus_types en fonction de la classe_poids et du sexe
df_troisieme <- df_troisieme %>%
  left_join(
    individus_types %>%
      select(classe, age, sexe, poids_type) %>%
      rename(classe_poids = classe),
    by = c("classe_poids", "age", "sexe")
  )

# Étape 5 : Générer les valeurs ajustées avec une variation de -+ 1.5 %
set.seed(123)  
df_troisieme <- df_troisieme %>%
  mutate(
    taille = ifelse(!is.na(taille_type), round(runif(n(), 0.985, 1.015) * taille_type, 2), NA),  # Taille ajustée
    poids = ifelse(!is.na(poids_type), round(runif(n(), 0.985, 1.015) * poids_type, 2), NA)     # Poids ajusté
  )

###
# Modifier df_troisième pour calculer la moyenne de taille et poids
###


# Réinitialiser les colonnes taille, poids et IMC
df_deuxieme <- df_deuxieme %>%
  mutate(
    taille = NA,
    poids = NA,
    IMC = NA
  )

# Étape 1 : Associer les classes taille et poids depuis classement_final
df_deuxieme <- df_deuxieme %>%
  left_join(
    classement_final %>%
      select(ind, classe_taille, classe_poids),
    by = "ind"
  )

# Étape 2 : Associer les tailles depuis individus_types en fonction de la classe_taille, sexe et âge
df_deuxieme <- df_deuxieme %>%
  left_join(
    individus_types %>%
      select(classe, age, sexe, taille_type) %>%
      rename(classe_taille = classe),
    by = c("classe_taille", "age", "sexe")
  )

# Étape 3 : Associer les poids depuis individus_types en fonction de la classe_poids, sexe et âge
df_deuxieme <- df_deuxieme %>%
  left_join(
    individus_types %>%
      select(classe, age, sexe, poids_type) %>%
      rename(classe_poids = classe),
    by = c("classe_poids", "age", "sexe")
  )

# Étape 4 : Générer des valeurs ajustées avec une variation de ±1.5 % et garantir la cohérence des âges
set.seed(123)  # Fixer la graine pour des résultats reproductibles
df_deuxieme <- df_deuxieme %>%
  group_by(ind) %>%
  arrange(age) %>%  # Assurer un tri par âge pour chaque individu
  mutate(
    taille = ifelse(
      !is.na(taille_type),
      round(
        cummax(runif(n(), 0.985, 1.015) * taille_type),  # Assurer une taille croissante
        2
      ),
      NA
    ),
    poids = ifelse(
      !is.na(poids_type),
      round(
        cummax(runif(n(), 0.985, 1.015) * poids_type),  # Assurer un poids croissant
        2
      ),
      NA
    ),
    IMC = round(poids / (taille / 100)^2, 2)  # Calculer l'IMC
  ) %>%
  ungroup()



# Fusionner les trois dataframe :

# Conserver uniquement les colonnes spécifiées dans df_premier, df_deuxieme et df_troisieme
df_premier <- df_premier %>%
  mutate(IMC = NA)

df_deuxieme <- df_deuxieme %>%
  mutate(
    IMC = NA
  )


df_premier <- df_premier %>%
  select(ind, sexe, age, taille, poids, IMC)

df_deuxieme <- df_deuxieme %>%
  select(ind, sexe, age, taille, poids, IMC)

df_troisieme <- df_troisieme %>%
  select(ind, sexe, age, taille, poids, IMC)

df_fusionne <- bind_rows(df_premier, df_deuxieme, df_troisieme)

df_fusionne$IMC <- df_fusionne$poids/(df_fusionne$taille/100)^2


# Fonction pour ajouter une colonne IMC_indic
ajouter_IMC_indic <- function(df) {
  df$IMC_indic <- ifelse(df$IMC < 18, "Faible",
                         ifelse(df$IMC <= 35, "Moyen", "Fort"))
  return(df)
}

df_fusionne <- ajouter_IMC_indic(df_fusionne)
new_data_dans_borne <- ajouter_IMC_indic(new_data_dans_borne)
# Pour sauvegarder la data
#write.csv(df_fusionne,"df_fusionne.csv")

new_data_dans_borne <- new_data_dans_borne %>%
  select(ind, sexe, age, taille, poids, IMC, IMC_indic)


new_data_VF <- bind_rows(df_fusionne, new_data_dans_borne)


# version finale et parfaitement corrigé du spline cubique
#write.csv(new_data_VF,"new_data_VF.csv")





