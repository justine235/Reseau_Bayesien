#=================================================================================================
#
#                                         PROJET ECONOMETRIE
#
#=================================================================================================

#========================================== 1) Environnement 
#=== 1.1) Librairies
source("http://bioconductor.org/biocLite.R")
biocLite("Rgraphviz")
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

my_library<- c("tidyverse","data.table","dplyr", "foreign", "mice", "highcharter", "haven", 
               "scales", "plotly", "bnlearn", "ade4", "irr", "vcd", "Rgraphviz", "DescTools")
ipak(my_library)


#=== 1.2) Chemin de travail
# setwd("C:/Users/acer/Desktop/Cours/TIDE 2017/S2/Econométrie qualitative/Projet")
# setwd("C:/Users/charl/Desktop/analyse_quali/projet")
setwd("C:/Users/mottierm/Documents/Perso/Econometrie/Data")


#=== 1.3) Importation de la table
tab1 <- read_sav("fr.cdsp.ddi.BPF2007-R1_F1.sav")
#tab1 <- fread("tab1.csv")

# Selection des variables utiles
tab1 <- tab1 %>% dplyr::select("Q46M", "RCRS2","RCRS7", "RCRS13", "RAGE", "SEXE", "GR", "RRS8", "Q48", "RCRS15","Q12A", "Q12B", "Q14", 
                         "Q15", "Q17B", "Q18", "Q37","Q38", "Q44", "Q17D", "Q17E") %>% as.data.frame()


#=== 1.4) Premiere visualisation
# quick view
glimpse(tab1)

# Missing value
auto_missing <- bind_cols(sapply(tab1, function(x) sum(is.na(x))) %>% 
                            as.data.frame() %>% 
                            dplyr::rename(Missing_Values_Per_Variable = ".") %>% 
                            mutate(Vars = rownames(.)) %>% 
                            dplyr::select(Vars, Missing_Values_Per_Variable) %>% 
                            arrange(desc(Missing_Values_Per_Variable)),
                          sapply(tab1, function(x) percent((sum(is.na(x)) / length(x)))) %>% 
                            as.data.frame() %>% 
                            dplyr::rename(Proportion_Missing_Values = ".") %>% 
                            arrange(desc(Proportion_Missing_Values)))

names(auto_missing) <- c("Variable", "Valeurs manquantes", "Proportion")

hchart(auto_missing %>% arrange(desc(`Valeurs manquantes`)), "column", 
       hcaes(x = Variable, y = `Valeurs manquantes`)) %>% 
  hc_title(text = "Visualition des missings values") 

apply(tab1,2,function(x) sum(is.na(x)))



#========================================== 2) Missing values + correlation
#=== 2.1) Valeurs manquantes
# Q38 : Journal TV regulierement regarde
sort(table(tab1$Q38),decreasing=TRUE)[1:2]
tab1$Q38[is.na(tab1$Q38)] <- 1

# Q37 : Informations TV : nombre de jours regardees
sort(table(tab1$Q37),decreasing=TRUE)[1:2]
tab1$Q37[is.na(tab1$Q37)] <- 7

# Q12B : Probleme le plus immportant en France : en second
sort(table(tab1$Q12B),decreasing=TRUE)[1:2]
tab1$Q12B[is.na(tab1$Q12B)] <- 1

apply(tab1,2,function(x) sum(is.na(x)))
tab1 <- tab1 %>% rename(y = Q46M) %>%  mutate(y = ifelse( y < 3, 1, 0))


#=== 2.2) Correlation 
# https://n2s.hypotheses.org/1
# http://www.jybaudot.fr/Inferentielle/associations.html
# https://eric.univ-lyon2.fr/~ricco/cours/cours/Dependance_Variables_Qualitatives.pdf 

# a)  test du Khi2
# H0 : independance entre les variables
# H1 : dependance
# Plus il est grand plus la liaison est forte
t1 <- sapply(tab1[,2:21],function(x) chisq.test(tab1$y,x)$p.value) %>% as.data.frame()
colnames(t1) <- c("Khi2")
t1$Variable <- rownames(t1)
t1$result_khi2 <- ifelse(t1$Khi2 < 0.05, "dependance", "Independance")

# a)  Test du phi
# http://rcompanion.org/handbook/H_10.html
# H0 : independance entre les variables
# H1 : dependance
# Plus il est grand plus la liaison est forte
t2 <- sapply(tab1[,2:21],function(x) DescTools::Phi(tab1$y,x)) %>% as.data.frame()
colnames(t2) <- c("Phi")
t2$Variable <- rownames(t2)
t2$result_Phi <- ifelse(t2$Phi == 0, "Absence de relation", 
                    ifelse(t2$Phi > 0 & t2$Phi <= 0.1, "Tres faible",
                    ifelse(t2$Phi > 0.1 & t2$Phi <= 0.2, "Faible",
                    ifelse(t2$Phi > 0.2 & t2$Phi <= 0.4, "Modere",
                    ifelse(t2$Phi > 0.4 & t2$Phi <= 0.8, "Fort", "Louche (colinearite)")))))

t1 <- merge(t1, t2, by.x = "Variable", by.y = "Variable")

# c) V de Cramer 
# https://lemakistatheux.wordpress.com/2013/05/31/le-v-de-cramer/
# Mesure l'intensite des reltions entre les variables
t2 <- sapply(tab1[,2:21],function(x) assocstats(table(tab1$y, x))$cramer) %>% as.data.frame()
colnames(t2) <- c("Cramer")
t2$Variable <- rownames(t2)
t2$result_Cramer <- ifelse(t2$Cramer == 0, "Absence de relation", 
                    ifelse(t2$Cramer > 0 & t2$Cramer <= 0.1, "Tres faible",
                    ifelse(t2$Cramer > 0.1 & t2$Cramer <= 0.2, "Faible",
                    ifelse(t2$Cramer > 0.2 & t2$Cramer <= 0.4, "Modere",
                    ifelse(t2$Cramer > 0.4 & t2$Cramer <= 0.8, "Fort", "Louche (colinearite)")))))

t1 <- merge(t1, t2, by.x = "Variable", by.y = "Variable")

# e) Coefficient de contingence
t2 <- sapply(tab1[,2:21],function(x) assocstats(table(tab1$y, x))$contingency) %>% as.data.frame()
colnames(t2) <- c("Contingence")
t2$Variable <- rownames(t2)
t2$result_Contingence <- ifelse(t2$Contingence == 0, "Absence de relation", 
                         ifelse(t2$Contingence > 0 & t2$Contingence <= 0.1, "Tres faible",
                         ifelse(t2$Contingence > 0.1 & t2$Contingence <= 0.2, "Faible",
                         ifelse(t2$Contingence > 0.2 & t2$Contingence <= 0.4, "Modere",
                         ifelse(t2$Contingence > 0.4 & t2$Contingence <= 0.8, "Fort", "Louche (colinearite)")))))

t1 <- merge(t1, t2, by.x = "Variable", by.y = "Variable")

# e) Kolmogorov-Smirnov Tests : T de Tschuprow
# https://lemakistatheux.wordpress.com/2013/09/01/le-t-de-tschuprow/
# Mesure d'association entre deux variables qualitatives distinctes 
# Permet de s'affranchir de l'influence des grands echantillons
# Proche de 0 : proche 1 => dependance fortes
t2 <- sapply(tab1[,2:21],function(x) TschuprowT(tab1$y,x)) %>% as.data.frame()
colnames(t2) <- c("T_Tschuprow")
t2$Variable <- rownames(t2)
t2$Variable <- str_replace_all(t2$Variable, ".D", "")
t2$result_Tschuprow <- ifelse(t2$T_Tschuprow == 0, "Absence de relation", 
                         ifelse(t2$T_Tschuprow > 0 & t2$T_Tschuprow <= 0.1, "Tres faible",
                         ifelse(t2$T_Tschuprow > 0.1 & t2$T_Tschuprow <= 0.3, "Faible",
                         ifelse(t2$T_Tschuprow > 0.3 & t2$T_Tschuprow <= 0.5, "Modere",
                         ifelse(t2$T_Tschuprow > 0.5 & t2$T_Tschuprow <= 0.8, "Fort", "Louche (colinearite)")))))

t1 <- merge(t1, t2, by.x = "Variable", by.y = "Variable")



#========================================== 3) Regroupement des variables de contexte et d'opinion
# Regroupement des variables de contexte ------------------------------------

# RCRS2
tab1 <- tab1 %>% mutate(RCRS2 = ifelse(RCRS2 == 1, "Sans / peu diplome",
                                 ifelse(RCRS2 == 2 | RCRS2 == 3, "Baccalaureat, BEPC, CAP, BEP",
                                 ifelse(RCRS2 == 4 | RCRS2 == 5, "Etudes sup", ""))))
# RCRS7
tab1 <- tab1 %>% mutate(RCRS7 = ifelse(RCRS7 == 1 | RCRS7 == 2, "Agr, Artisan, commerce, chef d'ent",
                                 ifelse(RCRS7 == 3 | RCRS7 == 5 | RCRS7 == 6 | RCRS7 == 7, "Intermediaire",
                                 ifelse(RCRS7 == 4 , "Enseignant",            
                                 ifelse(RCRS7 == 8, "Etudiant, eleve",     
                                 ifelse(RCRS7 == 9 , "N'a jamais travaille", ""))))))
# RCRS13
tab1 <- tab1 %>% mutate(RCRS13 = ifelse(RCRS13 == 1 | RCRS13 == 4, "A un parent",
                                  ifelse(RCRS13 == 2 , "N'a qu'un grand parent",
                                  ifelse(RCRS13 == 3 , "N'a pas d'ascendance etrangere", ""))))

# RAGE
tab1 <- tab1 %>% mutate(RAGE = ifelse(RAGE == 1 , "18-24 ans",
                                ifelse(RAGE == 2 , "25-34 ans",
                                ifelse(RAGE == 3 , "35-49 ans",            
                                ifelse(RAGE == 4 , "50-64 ans",     
                                ifelse(RAGE == 5 , "65 ans et plus", ""))))))
# RAGE
tab1 <- tab1 %>% mutate(SEXE = ifelse(SEXE == 1 , "HOMME","FEMME"))
                             
# GR
tab1 <- tab1 %>% mutate(GR = ifelse(GR == 1 , "GRAND NORD",
                               ifelse(GR == 2 , "GRAND OUEST",
                               ifelse(GR == 3 , "GRAND SUD OUEST",            
                               ifelse(GR == 4 , "GRAND SUD EST",     
                               ifelse(GR == 5 , "GRAND CENTRE", 
                               ifelse(GR == 6 , "GRAND EST",       
                               ifelse(GR == 7 , "ILE DE FRANCE",""))))))))

# RRS8
tab1 <- tab1 %>% mutate(RRS8 = ifelse(RRS8 == 1 , "Travaille",
                                ifelse(RRS8 == 2 | RRS8 == 4 , "Chomage",
                                ifelse(RRS8 == 3 | RRS8 == 6 | RRS8 == 7 , "Autre",            
                                ifelse(RRS8 == 5 , "Eleve, etudiant",  "")))))   
# Q48
tab1 <- tab1 %>% mutate(Q48 = ifelse(Q48 == 1 , "Celibataire",
                               ifelse(Q48 == 2 | Q48 == 4 , "Couple / concubinage",
                               ifelse(Q48 == 3  , "Vivant en couple sans etre marie(e)", 
                               ifelse(Q48 == 7 | Q48 == 8  , "VEUF", 
                               ifelse(Q48 == 5 | Q48 == 6 | Q48 == 9 , "Divorce + nsp",  ""))))))   
# RCRS15
tab1 <- tab1 %>% mutate(RCRS15 = ifelse(RCRS15 == 1 , "Catholique pratiquant",
                                  ifelse(RCRS15 == 2 , "Catholique non pratiquant",
                                  ifelse(RCRS15 == 3 | RCRS15 == 5 , "Autre religion", 
                                  ifelse(RCRS15 == 4  , "Sans religion",  "")))))  
 


  # Regroupement des variables d'opinion ------------------------------------
  
# Q12A
tab1 <- tab1 %>% mutate(Q12A = ifelse(Q12A == 1 , "Emploi",
                                 ifelse(Q12A == 2 | Q12A == 8 | Q12A == 11 | Q12A == 13 , "Securite / immigration",
                                 ifelse(Q12A == 3 | Q12A == 4 | Q12A == 9 , "Inegalite sociales / culturelles", 
                                 ifelse(Q12A == 5 | Q12A == 6 | Q12A == 7 | Q12A == 10  , "Prix",  "")))))  

# Q12B
tab1 <- tab1 %>% mutate(Q12B = ifelse(Q12B == 1 , "Emploi",
                                ifelse(Q12B == 2 | Q12B == 8 | Q12B == 11 | Q12B == 12 | Q12B == 13 | Q12B == 99, "Securite / immigration",
                                ifelse(Q12B == 3 | Q12B == 4 | Q12B == 9 , "Inegalite sociales / culturelles", 
                                ifelse(Q12B == 5 | Q12B == 6 | Q12B == 7 | Q12B == 10  , "Prix",  "")))))  

# Q14
tab1 <- tab1 %>% mutate(Q14 = ifelse(Q14 == 1 | Q14 == 2 , "Oui",
                                ifelse(Q14 == 3 | Q14 == 6 , "Stable",
                                ifelse(Q14 == 4 | Q14 == 5 , "Non",  ""))))  

# Q15
tab1 <- tab1 %>% mutate(Q15 = ifelse(Q15 == 1 | Q15 == 2 , "Oui",
                               ifelse(Q15 == 3 | Q15 == 6 , "Stable",
                               ifelse(Q15 == 4 | Q15 == 5 , "Non",  ""))))  

# Q17B
tab1 <- tab1 %>% mutate(Q17B = ifelse(Q17B == 1 | Q17B == 2 , "Oui",
                               ifelse(Q17B == 4 | Q17B == 5 | Q17B == 3 , "Non",  "")))  

# Q18
tab1 <- tab1 %>% mutate(Q18 = ifelse(Q18 == 1 , "En progres",
                                ifelse(Q18 == 2 ,"En declin",      
                                ifelse(Q18 == 4 | Q18 == 3 , "Ni l'un, ni l'autre",  ""))))  

# Q37
tab1 <- tab1 %>% mutate(Q37 = ifelse(Q37 == 0 | Q37 == 1 | Q37 == 2, "Rarement",
                               ifelse(Q37 == 3 | Q37 == 4 | Q37 == 5 ,"Se tient au courant",      
                               ifelse(Q37 == 6 | Q37 == 7 , "Regulierement",  "Rarement"))))  

# Q38
tab1 <- tab1 %>% mutate(Q38 = ifelse(Q38 == 1 | Q38 == 2 | Q38 == 10 | Q38 == 11 | Q38 == 12  | Q38 == 13 , "DROITE",
                               ifelse(Q38 == 3 | Q38 == 4 | Q38 == 5 | Q38 == 6 | Q38 == 7 | Q38 == 8 | Q38== 9 ,"GAUCHE",      
                               ifelse(Q38 == 14 | Q38 == 15 | Q38 == 16 | Q38 == 99 , "Autres",  ""))))  

# Q44
tab1 <- tab1 %>% mutate(Q44 = ifelse(Q44 == 1 , "Une chance",
                               ifelse(Q44 == 2 | Q44 == 4 ,"Un danger",      
                               ifelse(Q44 == 3  , "Ni l'un, ni l'autre",  ""))))  
# Q17D
tab1 <- tab1 %>% mutate(Q17D = ifelse(Q17D == 1 | Q17D == 2 | Q17D == 5 , "Oui",
                               ifelse(Q17D == 3 | Q17D == 4, "Non",  "")))

# Q17E
tab1 <- tab1 %>% mutate(Q17E = ifelse(Q17E == 1 | Q17E == 2 | Q17E == 5 , "Oui",
                                ifelse(Q17E == 3 | Q17E == 4, "Non",  "")))





#========================================== 4) Preparation des datas
tab1[] <- lapply(tab1, unclass)

# Preparation des tables de travail
liste_candidat <- c("y")
liste_contexte <- c("RCRS2","RCRS7", "RCRS13", "RAGE", "SEXE", "GR", "RRS8", "Q48", "RCRS15")
liste_opinion <-  c("Q12A", "Q12B", "Q14", "Q15", "Q17B", "Q18", "Q37","Q38", "Q44", "Q17D", "Q17E")

base <- tab1 %>% dplyr::select(liste_candidat, liste_opinion, liste_contexte) # Base de travail

Mod1 <- base %>% dplyr::select(c(liste_contexte, liste_candidat))
Mod2 <- base %>% dplyr::select(liste_opinion, liste_candidat)

write.table(Mod1, "Mod1.txt", sep="\t")
write.table(Mod2, "Mod2.txt", sep="\t")


#========================================== 5) Premieres visualisations
#=== 5.1) Equilibre entre les classes
total <- tab1 %>% group_by(y) %>% summarise(Nbr = length(y))
total <- total %>% mutate(y = ifelse(y == 0, "Vote improbable pour DSK",
                              ifelse(y == 1, "Vote probable pour DSK",  "")))

plot_ly(total, labels = ~y, values = ~Nbr, type = 'pie', 
        textposition = 'inside',
        textinfo = 'percent',
        text = ~y,
        insidetextfont = list(color = 'black'),
        hoverinfo = 'text',
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1))) %>%
  layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)) %>% 
  layout(showlegend = T, paper_bgcolor= "transparent", plot_bgcolor = "transparent") %>% 
  config(displayModeBar = FALSE) 


#=== 5.2) Variables de contextes
# RCRS2
y1 <- tab1 %>% filter(y == 1)
y0 <- tab1 %>% filter(y == 0)

total1 <- y1 %>% group_by(RCRS2) %>% count(n=n())
total2 <- y0 %>% group_by(RCRS2) %>% count(n=n())


highchart() %>%
  hc_plotOptions(column = list(
    stacking = "normal"
  )) %>% 
  hc_add_series(total2, "column", color = "#BFFF00", name = "votant", hcaes(x = RCRS2, y = n)) %>% 
  hc_add_series(total1, "column", color = "#FF0000", name = "votant", hcaes(x = RCRS2, y = n)) %>% 
  hc_title(text = "RCRS2") 



# GR
y1 <- tab1 %>% filter(y == 1)
y0 <- tab1 %>% filter(y == 0)

total1 <- y1 %>% group_by(GR) %>% count(n=n())
total2 <- y0 %>% group_by(GR) %>% count(n=n())


highchart() %>%
  hc_plotOptions(column = list(
    stacking = "normal"
  )) %>% 
  hc_add_series(total2, "column", color = "#BFFF00", name = "votant", hcaes(x = GR, y = n)) %>% 
  hc_add_series(total1, "column", color = "#FF0000", name = "votant", hcaes(x = GR, y = n)) %>% 
  hc_title(text = "GR") 



#=== 5.3) Variables d'opinion
# Q37
total1 <- y1 %>% group_by(Q37) %>% count(n=n())
total2 <- y0 %>% group_by(Q37) %>% count(n=n())


highchart() %>%
  hc_plotOptions(column = list(
    stacking = "normal"
  )) %>% 
  hc_add_series(total2, "column", color = "#BFFF00", name = "non votant", hcaes(x = Q37, y = n)) %>% 
  hc_add_series(total1, "column", color = "#FF0000", name = "votant", hcaes(x = Q37, y = n)) %>% 
  hc_title(text = "Q37") 


# Q38
total1 <- y1 %>% group_by(Q38) %>% count(n=n())
total2 <- y0 %>% group_by(Q38) %>% count(n=n())


highchart() %>%
  hc_plotOptions(column = list(
    stacking = "normal"
  )) %>% 
  hc_add_series(total2, "column", color = "#BFFF00", name = "non votant", hcaes(x = Q38, y = n)) %>% 
  hc_add_series(total1, "column", color = "#FF0000", name = "votant", hcaes(x = Q38, y = n)) %>% 
  hc_title(text = "Q38") 


# Q17D
total1 <- y1 %>% group_by(Q17D) %>% count(n=n())
total2 <- y0 %>% group_by(Q17D) %>% count(n=n())

highchart() %>%
  hc_plotOptions(column = list(
    stacking = "normal"
  )) %>% 
  hc_add_series(total2, "column", color = "#BFFF00", name = "non votant", hcaes(x = Q17D, y = n)) %>% 
  hc_add_series(total1, "column", color = "#FF0000", name = "votant", hcaes(x = Q17D, y = n)) %>% 
  hc_title(text = "Q17D") 


# Q44
total1 <- y1 %>% group_by(Q44) %>% count(n=n())
total2 <- y0 %>% group_by(Q44) %>% count(n=n())

highchart() %>%
  hc_plotOptions(column = list(
    stacking = "normal"
  )) %>% 
  hc_add_series(total2, "column", color = "#BFFF00", name = "non votant", hcaes(x = Q44, y = n)) %>% 
  hc_add_series(total1, "column", color = "#FF0000", name = "votant", hcaes(x = Q44, y = n)) %>% 
  hc_title(text = "Q44") 


# Q17D
total1 <- y1 %>% group_by(Q17D) %>% count(n=n())
total2 <- y0 %>% group_by(Q17D) %>% count(n=n())

highchart() %>%
  hc_plotOptions(column = list(
    stacking = "normal"
  )) %>% 
  hc_add_series(total2, "column", color = "#BFFF00", name = "non votant", hcaes(x = Q17D, y = n)) %>% 
  hc_add_series(total1, "column", color = "#FF0000", name = "votant", hcaes(x = Q17D, y = n)) %>% 
  hc_title(text = "Q17D") 



# Interactions
library(interplot)
interplot()


#========================================== 6) Modelisation 
#=== 6.0) prise en compte des interactions 
# ^2 permet de créer toutes les interactions possibles
# Nous le faisons manuellement dans une soucis de grandeur : on test l'interaction d'ordre 2 tour à tour 
# Methode fortement critiquable (on est pas en modele globale lorsque l'on fait en backward) mais il permet d'avoir une idée des interactions significatives


# INTERACTIONS sur les variables de contexte ------------------------------------
# INTERACTION SUR RCRS2
mod_interaction_context <- 
                    glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 +  # variable simple
                            RCRS2 * RCRS7 +  RCRS2 * RCRS13  +  RCRS2 * RAGE  +  RCRS2 * SEXE + RCRS2 * GR + RCRS2 * RRS8 + RCRS2 * Q48 + RCRS2 * RCRS15,     # interaction d'ordre 2
                            data=Mod1, family=binomial(link=logit))

choix_interaction <- step(mod_interaction_context, direction="backward", trace = FALSE)
choix_interaction$anova 
#=> on garde 1 interaction : RCRS2 * Q48 


# INTERACTION SUR RCRS7
mod_interaction_context <- 
                     glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 +  # variable simple
                             RCRS7*RCRS2 + RCRS7*RCRS13 + RCRS7*RAGE + RCRS7*SEXE + RCRS7*GR + RCRS7*RRS8 + RCRS7*Q48 + RCRS7*RCRS15,     # interaction d'ordre 2
                             data=Mod1, family=binomial(link=logit))

choix_interaction <- step(mod_interaction_context, direction="backward", trace = FALSE)
choix_interaction$anova
#=> on garde 0 interaction 


# INTERACTION SUR RCRS13
mod_interaction_context <- 
                      glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 +  
                              RCRS13*RCRS2 + RCRS13*RCRS7 + RCRS13*RAGE + RCRS13*SEXE + RCRS13*GR + RCRS13*RRS8 + RCRS13*Q48 + RCRS13*RCRS15,    
                              data=Mod1, family=binomial(link=logit))

choix_interaction <- step(mod_interaction_context, direction="backward", trace = FALSE)
choix_interaction$anova
#=> on garde 1 interaction : RCRS13 * SEXE


# INTERACTION SUR RAGE
mod_interaction_context <- 
                      glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 + 
                              RAGE*RCRS2 + RAGE*RCRS7 + RAGE*RCRS13 + RAGE*SEXE + RAGE*GR + RAGE*RRS8 + RAGE*Q48 + RAGE*RCRS15,    
                              data=Mod1, family=binomial(link=logit))

choix_interaction <- step(mod_interaction_context, direction="backward", trace = FALSE)
choix_interaction$anova
#=> on garde 1 interaction : RAGE * SEXE 



# INTERACTION SUR SEXE
mod_interaction_context <- 
                        glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 +  
                                SEXE*RCRS2 + SEXE*RCRS7 + SEXE*RCRS13 + SEXE*RAGE + SEXE*GR + SEXE*RRS8 + SEXE*Q48 + SEXE*RCRS15,    
                                data=Mod1, family=binomial(link=logit))

choix_interaction <- step(mod_interaction_context, direction="backward", trace = FALSE)
choix_interaction$anova
#=> on garde 3 interactions : SEXE*RCRS13 SEXE*RAGE SEXE*GR 



# INTERACTION SUR GR
mod_interaction_context <- 
                      glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 +  
                              GR*RCRS2 + GR*RCRS7 + GR*RCRS13 + GR*RAGE + GR*SEXE + GR*RRS8 + GR*Q48 + GR*RCRS15,    
                              data=Mod1, family=binomial(link=logit))

choix_interaction <- step(mod_interaction_context, direction="backward", trace = FALSE)
choix_interaction$anova
#=> on garde GR*SEXE


# INTERACTION SUR Q48 
mod_interaction_context <- 
                      glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 + 
                              Q48*RCRS2 + Q48*RCRS7 + Q48*RCRS13 + Q48*RAGE + Q48*SEXE + Q48*GR + Q48*RRS8 + Q48*RCRS15,    
                              data=Mod1, family=binomial(link=logit))

choix_interaction <- step(mod_interaction_context, direction="backward", trace = FALSE)
choix_interaction$anova
#=> on garde Q48*RCRS2

# INTERACTION SUR RCRS15
mod_interaction_context <- 
                    glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 + 
                            RCRS15*RCRS2 + RCRS15*RCRS7 + RCRS15*RCRS13 + RCRS15*RAGE + RCRS15*SEXE + RCRS15*GR + RCRS15*RRS8 + RCRS15*Q48,    
                        data=Mod1, family=binomial(link=logit))

choix_interaction <- step(mod_interaction_context, direction="backward", trace = FALSE)
choix_interaction$anova
#=> on garde 0 interaction

#===> RESUME POUR LES VAR DE CONTEXTE SELECTIONNE: 
# RCRS2 * Q48 
# RCRS13 * SEXE  
# RAGE * SEXE 
# SEXE*RCRS13 SEXE*RAGE SEXE*GR 
# GR * SEXE
# Q48*RCRS2

# RCRS2*Q48 + RCRS13*SEXE + RAGE*SEXE + SEXE*GR 



# INTERACTIONS sur les variables d'opinions ------------------------------------

# Q12A
mod_interaction_opinion <- 
                      glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                             Q12A*Q12B + Q12A*Q14 + Q12A*Q15 + Q12A*Q17B + Q12A*Q18 + Q12A*Q37 + 
                             Q12A*Q38 + Q12A*Q44 + Q12A*Q17D + Q12A*Q17E ,   
                             data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#= > on garde 0 interactions

# Q12B
mod_interaction_opinion <- 
                    glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                          Q12B*Q12A + Q12B*Q14 + Q12B*Q15 + Q12B*Q17B + Q12B*Q18 + Q12B*Q37 + 
                          Q12B*Q38 + Q12B*Q44 + Q12B*Q17D + Q12B*Q17E,   
                        data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde 0 interaction 

# Q14
mod_interaction_opinion <- 
                  glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                        Q14*Q12A + Q14*Q12B + Q14*Q15 + Q14*Q17B + Q14*Q18 + Q14*Q37 + 
                        Q14*Q38 + Q14*Q44 + Q14*Q17D + Q14*Q17E,   
                      data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde Q14*Q18 + Q14*Q37 + Q14*Q17D + Q14*Q17E


# Q15
mod_interaction_opinion <- 
                        glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                              Q15*Q12A + Q15*Q12B + Q15*Q14 + Q15*Q17B + Q15*Q18 + Q15*Q37 + 
                              Q15*Q38 + Q15*Q44 + Q15*Q17D + Q15*Q17E,   
                               data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde 0 interaction


# Q17B
mod_interaction_opinion <- 
                      glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                             Q17B*Q12A + Q17B*Q12B + Q17B*Q14 + Q17B*Q15 + Q17B*Q18 + Q17B*Q37 + 
                             Q17B*Q38 + Q17B*Q44 + Q17B*Q17D + Q17B*Q17E,   
                             data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde Q17B*Q44



# Q18
mod_interaction_opinion <- 
                        glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                               Q18*Q12A + Q18*Q12B + Q18*Q14 + Q18*Q15 + Q18*Q17B + Q18*Q37 + 
                               Q18*Q38 + Q18*Q44 + Q18*Q17D + Q18*Q17E,   
                               data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde Q18*Q14


# Q37 
mod_interaction_opinion <- 
                        glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                               Q37*Q12A + Q37*Q12B + Q37*Q14 + Q37*Q15 + Q37*Q17B + Q37*Q18 + 
                               Q37*Q38 + Q37*Q44 + Q37*Q17D + Q37*Q17E,   
                               data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde Q37*Q14


# Q38 
mod_interaction_opinion <- 
                        glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                               Q38*Q12A + Q38*Q12B + Q38*Q14 + Q38*Q15 + Q38*Q17B + Q38*Q18 + 
                               Q38*Q37 + Q38*Q44 + Q38*Q17D + Q38*Q17E,   
                               data=Mod2, family=binomial(link=logit), control = list(maxit = 1000))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde Q38*Q12B + Q38*Q14 + Q38*Q15 + Q38*Q18 + Q38*Q37 + Q38*Q44 + Q38*Q17D + Q38*Q17E 
# Warning message: glm.fit: des probabilités ont été ajustées numériquement à 0 ou 1 
# https://perso.univ-rennes1.fr/valerie.monbet/GLM/GLMpharma.pdf
#  l'estimateur du maximum de vraisemblance n'existe pas
# On ne va pas sélectionner ces intéractions

# Q44
mod_interaction_opinion <- 
                    glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                              Q44*Q12A + Q44*Q12B + Q44*Q14 + Q44*Q15 + Q44*Q17B + Q44*Q18 + 
                              Q44*Q37 + Q44*Q38 + Q44*Q17D + Q44*Q17E,   
                              data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde Q44*Q17B 

# Q17D 
mod_interaction_opinion <- 
                      glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                             Q17D*Q12A + Q17D*Q12B + Q17D*Q14 + Q17D*Q15 + Q17D*Q17B + Q17D*Q18 + 
                             Q17D*Q37 + Q17D*Q38 + Q17D*Q44 + Q17D*Q17E,   
                             data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde Q17D*Q14

# Q17E
mod_interaction_opinion <- 
                    glm(y~ Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + 
                           Q17E*Q12A + Q17E*Q12B + Q17E*Q14 + Q17E*Q15 + Q17E*Q17B + Q17E*Q18 + 
                           Q17E*Q37 + Q17E*Q38 + Q17E*Q44 + Q17E*Q17D,   
                           data=Mod2, family=binomial(link=logit))

choix_interaction_opinion <- step(mod_interaction_opinion, direction="backward", trace = FALSE)
choix_interaction_opinion$anova
#=> on garde Q17E*Q14

#===> RESUME POUR LES VAR D'OPINION SELECTIONNE: 
#=> on garde Q14*Q18 + Q14*Q37 + Q14*Q17D + Q14*Q17E
#=> on garde Q17B*Q44
#=> on garde Q18*Q14
#=> on garde Q37*Q14
#======> on garde pas Q38*Q12B + Q38*Q14 + Q38*Q15 + Q38*Q18 + Q38*Q37 + Q38*Q44 + Q38*Q17D + Q38*Q17E 
#=> on garde Q44*Q17B 
#=> on garde Q17D*Q14
#=> on garde Q17E*Q14

# Q14*Q18 + Q14*Q37 + Q14*Q17D + Q14*Q17E + 17B*Q44

#=== 6.0) selection de variables

# SELECTION sur les variables de contexte ------------------------------------
## Modele avec variables de contexte (Mod1)
# AIC

mod_contexte_n <- glm(y~1, data=Mod1, family=binomial(link=logit))
mod_contexte_f <- glm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RRS8 + Q48 + RCRS15 +  # variable simple
                          RCRS2*Q48 + RCRS13*SEXE + RAGE*SEXE + SEXE*GR ,   # interaction simple      
                          data=Mod1, family=binomial(link=logit))



# choix par AIC (par defaut k = 2)
choix_contexte_b <- step(mod_contexte_f, direction="backward", trace = FALSE)
choix_contexte_f <- step(mod_contexte_n, direction="forward", trace = FALSE, scope = list(lower = mod_contexte_n, upper = mod_contexte_f))
choix_contexte_s <- step(mod_contexte_n, direction="both", trace = FALSE, scope = list(upper = mod_contexte_f))

choix_contexte_b$anova # enleve RCRS2:Q48 / Q48 / RRS8
choix_contexte_f$anova # enleve RCRS2:Q48 / Q48 / RRS8
choix_contexte_s$anova # enleve RCRS2:Q48 / Q48 / RRS8

# => MODELE 1 : RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RCRS15 + RCRS13*SEXE + RAGE*SEXE + SEXE*GR



# choix par BIC
choix_contexte_b <- step(mod_contexte_f, direction="backward", trace = FALSE, k = log(nrow(Mod1)))
choix_contexte_f <- step(mod_contexte_n, direction="forward", trace = FALSE, scope = list(lower = mod_contexte_n, upper = mod_contexte_f), k = log(nrow(Mod1)))
choix_contexte_s <- step(mod_contexte_n, direction="both", trace = FALSE, k = log(nrow(Mod1)), scope = list(upper = mod_contexte_f))

choix_contexte_b$anova # garde RCRS2, RAGE, SEXE, RCRS15
choix_contexte_f$anova # garde RCRS2, RAGE, SEXE, RCRS15
choix_contexte_s$anova # garde RCRS2, RAGE, SEXE, RCRS15

# => MODELE 2 : RCRS2 + RAGE + SEXE + RCRS15

       

# Comparaison des modeles
model1 <- lm(y ~ RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RCRS15 + RCRS13*SEXE + RAGE*SEXE + SEXE*GR, data=Mod1) 
model2 <- lm(y ~ RCRS2 + RAGE + SEXE + RCRS15, data=Mod1) 


# Model1
anova(model1)
RSS <- c(crossprod(model1$residuals)) # 977.3732
MSE <- RSS / length(model1$residuals) #  0.1729864
RMSE <- sqrt(MSE) #  0.4159163

# Model2
anova(model2)
RSS <- c(crossprod(model2$residuals)) # 991.4805
MSE <- RSS / length(model2$residuals) #  0.1754833
RMSE <- sqrt(MSE)  # 0.4189072

# ==> BEST : RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RCRS15 + RCRS13*SEXE + RAGE*SEXE + SEXE*GR 

# SELECTION sur les variables d'opinions ------------------------------------
## Modele avec variables d'opinion (MOD2)
# AIC

mod_opinion_n <- glm(y~1, data=Mod2, family=binomial(link=logit))
mod_opinion_f <- glm(y ~  Q12A + Q12B + Q14 + Q15 + Q17B + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E  +  # variable simple
                          Q14*Q18 + Q14*Q37 + Q14*Q17D + Q14*Q17E + Q17B*Q44,
                          data=Mod2, family=binomial(link=logit))

# choix par AIC (par defaut k = 2)
choix_opinion_b <- step(mod_opinion_f, direction="backward", trace = FALSE)
choix_opinion_f <- step(mod_opinion_n, direction="forward", trace = FALSE, scope = list(lower = mod_opinion_n, upper = mod_opinion_f))
choix_opinion_s <- step(mod_opinion_n, direction="both", trace = FALSE, scope = list(upper = mod_opinion_f))

choix_opinion_b$anova # enleve Q15, Q17B, Q17B:Q44
choix_opinion_f$anova # enleve Q15, Q17B, Q17B:Q44
choix_opinion_s$anova # enleve Q15, Q17B, Q17B:Q44

# => MODELE 1 : Q12A + Q12B + Q14 + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + Q14*Q18 + Q14*Q37 + Q14*Q17D + Q14*Q17E


# choix par BIC
choix_opinion_b <- step(mod_opinion_f, direction="backward", trace = FALSE, k = log(nrow(Mod2)))
choix_opinion_f <- step(mod_opinion_n, direction="forward", trace = FALSE, scope = list(lower = mod_opinion_n, upper = mod_opinion_f), k = log(nrow(Mod2)))
choix_opinion_s <- step(mod_opinion_n, direction="both", trace = FALSE, k = log(nrow(Mod2)), scope = list(upper = mod_opinion_f))

choix_opinion_b$anova # garde  Q14 + Q38 + Q17D + Q17E + Q14 * Q17E  
choix_opinion_f$anova # garde Q38 + Q17E + Q17D
choix_opinion_s$anova # garde Q38 + Q17E + Q17D

# => MODELE 2 : Q14 + Q38 + Q17D + Q17E + Q14 * Q17E 
# => MODELE 3 : Q14 + Q38 + Q17D + Q17E

# Comparaison des modeles
model1 <- lm(y ~ Q12A + Q12B + Q14 + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + Q14*Q18 + Q14*Q37 + Q14*Q17D + Q14*Q17E , data=Mod2) 
model2 <- lm(y ~ Q14 + Q38 + Q17D + Q17E + Q14*Q17E , data=Mod2) 
model3 <- lm(y ~ Q38 + Q17E + Q17D, data=Mod2)



# Model1
anova(model1)
RSS <- c(crossprod(model1$residuals)) # 958.9358
MSE <- RSS / length(model1$residuals) #  0.1697231
RMSE <- sqrt(MSE)  # 0.4119747


# Model2
anova(model2)
RSS <- c(crossprod(model2$residuals)) # 973.604
MSE <- RSS / length(model2$residuals) #  0.1723193
RMSE <- sqrt(MSE)  #  0.4151136

# Model3
anova(model3)
RSS <- c(crossprod(model3$residuals)) # 977.45
MSE <- RSS / length(model3$residuals) #  0.173
RMSE <- sqrt(MSE)  # 0.4159327


# ==> BEST : Q12A + Q12B + Q14 + Q18 + Q37 + Q38 + Q44 + Q17D + Q17E + Q14*Q18 + Q14*Q37 + Q14*Q17D + Q14*Q17E

# SELECTION GLOBLE ------------------------------------
# ==> BEST :   RCRS2 + RCRS7 + RCRS13 + RAGE + SEXE + GR + RCRS15 + RCRS13*SEXE + RAGE*SEXE + SEXE*GR
# ==> BEST :  Q38 + Q17E + Q17D + Q12A + Q12B + Q14 + Q14*Q17E + Q44 + Q14*Q17D + Q37 + Q14:Q37 + Q18 + Q14*Q18

liste_contexte_top <- c("RCRS2","RAGE","RCRS15","SEXE","RCRS7","RCRS13","RCRS15", "GR")
liste_opinion_top <- c("Q38","Q17E","Q17D","Q12A","Q12B","Q14","Q44","Q37","Q18")

base2 <- base %>% dplyr::select(c(liste_contexte_top, liste_opinion_top, liste_candidat))
base2 <- lapply(base2, as.factor) %>% as.data.frame()



#=== 6.2) Bayesian Newtwork 
# RCRS13*SEXE + RAGE*SEXE + SEXE*GR + Q14*Q17E+ Q14*Q17D + Q14*Q37 + Q14*Q18

# Whitelist
{wl1 = matrix(c(names(base2)[4], names(base2)[6]), ncol = 2)
  wl2 = matrix(c(names(base2)[4], names(base2)[2]), ncol = 2)
  wl3 = matrix(c(names(base2)[4], names(base2)[7]), ncol = 2)
  wl4 = matrix(c(names(base2)[13], names(base2)[9]), ncol = 2)
  wl5 = matrix(c(names(base2)[13], names(base2)[10]), ncol = 2)
  wl6 = matrix(c(names(base2)[13], names(base2)[15]), ncol = 2)
  wl7 = matrix(c(names(base2)[13], names(base2)[16]), ncol = 2)
  wl = rbind(wl1, wl2, wl3, wl4, wl5, wl6, wl7)
  rm(wl1, wl2, wl3, wl4, wl5, wl6, wl7)
}

# Blacklist
{
  bl1 = matrix(c(names(base2)[8:16], rep(names(base2)[1], 9)),ncol = 2)
  bl2 = matrix(c(names(base2)[8:16], rep(names(base2)[2], 9)),ncol = 2)
  bl3 = matrix(c(names(base2)[8:16], rep(names(base2)[3], 9)),ncol = 2)
  bl4 = matrix(c(names(base2)[8:16], rep(names(base2)[4], 9)),ncol = 2)
  bl5 = matrix(c(names(base2)[8:16], rep(names(base2)[5], 9)),ncol = 2)
  bl6 = matrix(c(names(base2)[8:16], rep(names(base2)[6], 9)),ncol = 2)
  bl7 = matrix(c(names(base2)[8:16], rep(names(base2)[7], 9)),ncol = 2)
  bl8 = matrix(c(rep(names(base2)[17],16), names(base2)[1:16]),ncol = 2)
  bl = rbind(bl1, bl2, bl3, bl4, bl5, bl6, bl7,bl8)
  rm(bl1, bl2, bl3, bl4, bl5, bl6, bl7,bl8)
}


# Algorithme gs
baye <- gs(base2, whitelist = wl, blacklist = bl, test = "smc-x2", alpha = 0.05, optimized = TRUE, debug = TRUE)
highlight.opts <- list(nodes = c("Q38", "y"), arcs = c("Q38", "y", "Q17E", "Q38", "Q17D", "Q38", "Q37", "Q38"), col = "red", fill = "darkolivegreen3")
graphviz.plot(baye, highlight  = highlight.opts, layout="neato", shape="ellipse",  main = "Visualisation des réseaux bayésiens")

# Algorithme hc
baye1 <- hc(base2,  whitelist = wl, blacklist = bl, debug=TRUE, optimized=TRUE)
highlight <- list(arcs = c("Q38", "y", "Q17E", "y"), col = "red")
graphviz.plot(baye1, highlight  = highlight, layout="neato", shape="ellipse",  main = "Visualisation des réseaux bayésiens")
fit = bn.fit(baye1, base2)
prediction <- fit$y
proba_vote <- as.data.frame(prediction$prob)

saveRDS(proba_vote, "proba_vote.rds")

# ================================================================================#
