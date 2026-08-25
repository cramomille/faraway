
#                           ENREGISTREMENT DES RESULTATS DES PARTIES DE FARAWAY
#
#                                                                antoine beroud
#                                                                     aout 2025


# FONCTION 1 -----------------------------------------------------------------
#' @titles Analyse des scores par carte
#'
#' @param dir une chaine de caracteres correspondant au chemin vers le dossier 
#' ou les parties ont ete enregistrees grace a la fonction 'save_game()'
#' @param n une valeur numerique correspondant au nombre de meilleures parties a 
#' retenir (Inf = toutes)
#' @param j une chaine de caracteres correspondant au nom d'une joueuse pour ne 
#' prendre en compte que ses parties
#'
#' @return 
#' La fonction affiche :
#'   - mean   : score moyen pour la carte
#'   - median : score median pour la carte
#'   - min    : score minimal pour la carte
#'   - max    : score maximal pour la carte
#'   
#'@examples
#'
#' 
#' @export
analyze_cards <- function(dir = "games_results", 
                          n = Inf, 
                          j = NULL) {
  
  # Chargement des parties enregistrees
  fichiers <- list.files(dir, pattern = "\\.RData$", full.names = TRUE)
  dfs <- list()
  for (f in fichiers) {
    e <- new.env()
    load(f, envir = e)
    if (!exists("df", envir = e)) next
    dfs[[f]] <- e$df
  }
  all_df <- do.call(rbind, dfs)
  rownames(all_df) <- NULL
  
  # Filtrage si une joueuse est specifiee
  if (!is.null(j)) {
    all_df <- all_df[all_df$joueuse == j, , drop = FALSE]
    if (nrow(all_df) == 0) {
      message("aucune partie trouvee pour la joueuse : ", j)
      return(invisible(NULL))
    }
  }
  
  # Conservation des colonnes contenant les scores
  cartes <- setdiff(names(all_df), c("date", "joueuse", "total"))
  
  # Conservation des 'n' meilleures parties
  top_df <- all_df[order(-all_df$total), ]
  if (is.finite(n) && n < nrow(top_df)) {
    top_df <- top_df[seq_len(n), ]
  }
  
  # Calcul des statistiques par carte
  mean <- colMeans(top_df[, cartes, drop = FALSE])
  median <- apply(top_df[, cartes, drop = FALSE], 2, median)
  min <- apply(top_df[, cartes, drop = FALSE], 2, min)
  max <- apply(top_df[, cartes, drop = FALSE], 2, max)
  
  # Construction du tableau de resultat
  result <- data.frame(
    card   = names(mean),
    mean   = round(mean, 0),
    median = round(median, 0),
    min    = round(min, 0),
    max    = round(max, 0),
    row.names = NULL
  )
  
  # Affichage
  print(result, row.names = FALSE)
  
  # GRAPHIQUE -----------------------------------------------------------------
  # Construction du titre en fonction des filtres
  titre <- "Scores par carte"
  filtres <- c()
  if (!is.null(j)) filtres <- c(filtres, j)
  if (!is.null(n) && is.finite(n)) filtres <- c(filtres, paste0("top ", n))
  if (length(filtres) > 0) titre <- paste(titre, "(", paste(filtres, collapse = ", "), ")")
  
  x <- 1:9
  cols <- c("mean" = "#ea5153", "median" = "#8292ca", "min" = "#fdc543", "max" = "#5cb885")
  
  # Creation du graphique
  plot(x, result$mean, type = "n", ylim = c(0, 30),
       xaxt = "n", xlab = "Carte", ylab = "Score",
       main = titre)
  axis(1, at = x, labels = result$card, las = 2)
  
  # Creation des courbes
  lines(x, result$mean, type = "b", col = cols["mean"], lwd = 2, pch = 16)
  lines(x, result$median, type = "b", col = cols["median"], lwd = 2, pch = 17)
  lines(x, result$min, type = "b", col = cols["min"], lwd = 2, pch = 15)
  lines(x, result$max, type = "b", col = cols["max"], lwd = 2, pch = 18)
  
  # Ajout de la legende
  legend("topleft", legend = names(cols), col = cols, lty = 1, lwd = 2, pch = 16)
  
  invisible(result)
}


# FONCTION 2 -----------------------------------------------------------------
#' @title Lecture des resultats de toutes les parties et creation d'un 
#' classement des parties et des joueuses
#'
#' @param dir une chaine de caracteres correspondant au chemin vers le dossier 
#' ou les parties ont ete enregistrees grace a la fonction 'save_game()'
#' @param n une valeur numerique correspondant au nombre de resultats a afficher 
#' (Inf = tout)
#' @param j une chaine de caracteres correspondant au nom d'une joueuse pour ne 
#' prendre en compte que ses parties
#'
#' @return 
#' La fonction renvoie un data.frame avec le classement demande ainsi qu'un 
#' visuel dans la console
#' 
#' @examples
#' rank_games()
#' 
#' @export
rank_games <- function(dir = "games_results", 
                       n = Inf, 
                       j = NULL) {
  
  # CHECK PARAMS --------------------------------------------------------------
  if (!dir.exists(dir)) stop("le dossier ", dir, " n'existe pas")
  
  fichiers <- list.files(dir, pattern = "\\.RData$", full.names = TRUE)
  if (length(fichiers) == 0) stop("aucune partie trouvee dans : ", dir)
  
  # PROCESSING ----------------------------------------------------------------
  dfs <- list()
  for (f in fichiers) {
    e <- new.env()
    load(f, envir = e)
    if (!exists("df", envir = e)) next
    dfs[[f]] <- e$df
  }
  
  all_df <- do.call(rbind, dfs)
  rownames(all_df) <- NULL
  
  # Tri par total decroissant
  classement <- all_df[order(-all_df$total), c("joueuse", "total")]
  
  # Ajout des rangs avec gestion des egalites
  classement$rang <- rank(-classement$total, ties.method = "min")
  classement <- classement[, c("rang", "joueuse", "total")]
  
  # Filtrage si un nom de joueuse est fourni
  if (!is.null(j)) {
    classement <- classement[classement$joueuse == j, , drop = FALSE]
    if (nrow(classement) == 0) {
      message("Aucune partie trouvee pour la joueuse : ", j)
      return(invisible(classement))
    }
  }
  
  # Limitation si 'n' est fourni
  if (is.finite(n) && n < nrow(classement)) {
    classement <- classement[seq_len(n), ]
  }
  
  # Affichage dans la console
  for (i in seq_len(nrow(classement))) {
    cat(sprintf("%02d : %s - %d\n", 
                classement$rang[i], 
                classement$joueuse[i], 
                classement$total[i]))
  }
  
  # Affichage de la distrubition des scores
  scores <- all_df$total
  min_s <- min(scores)
  max_s <- max(scores)
  
  b <- 5
  
  breaks <- seq(
    floor(min_s / b) * b,
    ceiling(max_s / b) * b,
    by = b
  )
  
  h <- hist(scores, breaks = breaks, plot = FALSE)
  
  # Conversion des densites en %
  h$counts <- h$counts / length(scores) * 100
  
  plot(h,
       ylim = c(0, 20),
       col = "#8f96cb", 
       border = "#111",
       main = sprintf("Distribution des scores (min = %d, max = %d)", min_s, max_s),
       xlab = "Scores", 
       ylab = "Pourcentages")
  
  # Si une joueuse est renseignee, superposition en %
  if (!is.null(j)) {
    scores_j <- all_df$total[all_df$joueuse == j]
    if (length(scores_j) > 0) {
      h_j <- hist(scores_j,
                  breaks = h$breaks, # memes classes pour comparer
                  plot = FALSE)
      h_j$counts <- h_j$counts / length(scores_j) * 100
      plot(h_j, 
           col = rgb(0.95, 0.7, 0.8, 0.3), 
           border = "#111", 
           add = TRUE)
      legend("topright",
             legend = c("all", j),
             fill = c("#8f96cb", rgb(0.95, 0.7, 0.8, 0.3)),
             border = "#fff")
    }
  }
  
  invisible(classement)
}



# EXPLORATIONS ---------------------------------------------------------------
rank_games(dir = "data/")
rank_games(dir = "data/", n = 10)

rank_games(dir = "data/", n = 10, j = "rose")
rank_games(dir = "data/", n = 10, j = "antoine")
rank_games(dir = "data/", n = 10, j = "valentine")
rank_games(dir = "data/", n = 10, j = "emilie")

analyze_cards(dir = "data/")
analyze_cards(dir = "data/", n = 50)

analyze_cards(dir = "data/", j = "rose")
analyze_cards(dir = "data/", j = "antoine") 
analyze_cards(dir = "data/", j = "valentine") 

analyze_cards(dir = "data/", n = 10, j = "rose")
analyze_cards(dir = "data/", n = 10, j = "antoine") 
analyze_cards(dir = "data/", n = 10, j = "valentine") 




