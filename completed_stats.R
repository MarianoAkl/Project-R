data(penguins,package = "palmerpenguins")

# Fonction (a) : statistiques pour une variable numérique

stats_for_quanti <- function(var_quanti){
  # vérification du type de l'argument
  if (!is.vector(var_quanti,mode = "numeric")) 
    stop("L'argument fourni n'est pas un vecteur de type numérique.")
  
  # statistiques de la variable
  out <- list()
  out$nombre_observations <- c("Total :" = length(var_quanti),
                               "Manquantes (NA) :" = sum(is.na(var_quanti)))
  out$quantiles <- quantile(var_quanti,
                            probs=c(0,.1,.25,.5,.75,.9,1),
                            na.rm=TRUE,digits=3)
  out$moyenne <- mean(var_quanti,na.rm=TRUE)
  out$ecarttype <- sd(var_quanti,na.rm = TRUE)
  out$observations <- var_quanti
  
  # création d'une classe pour out
  class(out) <- "stats_for_quanti"
  
  # sortie de la fonction
  return(out)
}

# tests sur la fonction
stats_for_quanti(penguins)
stats_for_quanti(c("Bonjour"))
stats_for_quanti(5)
sortie_test <- stats_for_quanti(penguins$bill_length_mm)
str(sortie_test)
# Tout est correct

# Méthode print pour un objet retourné par la fonction (a)
print.stats_for_quanti <- function(x,...){
  names(x$moyenne) <- "Moyenne"
  names(x$ecarttype) <- "E.T"
  print(c(x$quantiles,x$moyenne,x$ecarttype))
  invisible(x)
}

# tests sur la méthode (a)
stats_for_quanti(penguins$bill_length_mm)

sortie_test <- stats_for_quanti(penguins$bill_length_mm)
sortie_test

print(sortie_test)
# tout est correct
# Méthode summary pour un objet retourné par la fonction (a)
summary.stats_for_quanti <- function(x,...){
  out <- unclass(x)
  cat("Variable numérique \n\n")
  cat("Nombre d'observations :\n")
  for (i in seq_along(out$nombre_observations)) {
    cat(sprintf("%-15s %d \n", names(out$nombre_observations)[i], 
                out$nombre_observations[i]))
  }
  cat("\n")
  cat("Quantiles :\n")
  for (i in seq_along(out$quantiles)) {
    cat(sprintf("%-5s %0.3f \n", names(out$quantiles)[i], 
                out$quantiles[i]))
  }
  cat("\n")
  cat("La moyenne est ",out$moyenne," et l'écart-type est ",out$ecarttype)
}
# tests
summary(sortie_test)
# toute est Ok

# Fonction (b) : statistiques pour un facteur
stats_for_quali <- function(var_quali){
  # vérification du type de l'argument
  if (!is.factor(var_quali)) 
    stop("L'argument fourni n'est pas de type facteur.")
  
  # statistiques de la variable
  out <- list()
  out$nombre_observations <- c("Total :" = length(var_quali),
                               "Manquantes (NA) :" = sum(is.na(var_quali)))
  
  var_quali_copie <- var_quali[!is.na(var_quali)]
  
  tab <- table(var_quali_copie)
  out$frequences <- data.frame(Modalite = levels(var_quali_copie),
                               Frequence = as.integer(tab),
                               Proportion = prop.table(as.numeric(tab)))

  
  out$Mode <- which.max(tab)
  out$observations <- var_quali
  
  # création d'une classe pour out
  class(out) <- "stats_for_quali"
  
  # sortie de la fonction
  return(out)
}
# tests
stats_for_quali(c("Bonjour"))
sortie_b <- stats_for_quali(penguins$species)
str(sortie_b)
# tout est Ok

# Méthode print pour un objet retourné par la fonction (b)
print.stats_for_quali <- function(x,...){
  print(x$frequences,row.names = FALSE)
  invisible(x)
}
# tests
sortie_b
# tout est Ok

# Méthode summary pour un objet retourné par la fonction (b)
summary.stats_for_quali <- function(x,...){
  out <- unclass(x)
  cat("Variable categorique \n\n")
  cat("Nombre d'observations :\n")
  for (i in seq_along(out$nombre_observations)) {
    cat(sprintf("%-15s %d \n", names(out$nombre_observations)[i], 
                out$nombre_observations[i]))
    }
  cat("\n")
  cat("Fréquences :\n")
  print(out$frequences,row.names = FALSE)
  cat("\n")
  cat("Le mode est la catégorie ",out$Mode)
}
# tests
summary(sortie_b)
# toute est Ok