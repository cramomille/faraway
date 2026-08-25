#                           ENREGISTREMENT DES RESULTATS DES PARTIES DE FARAWAY
#
#                                                                antoine beroud
#                                                                     aout 2025


# FONCTION -------------------------------------------------------------------
#' @title Enregistrement des scores d'une partie de Faraway
#'
#' @param scores une liste de vecteurs (entre 2 et 6) :
#'   - premier element : une chaine de caracteres correspondant au nom de la 
#'   joueuse
#'   - neuf autres elements : des valeurs numeriques correspondants aux scores 
#'   des 8 cartes (de la 8e a la 1ere) et a celui des cartes sanctuaires
#' @param dir chemin du dossier ou la partie est enregistree
#' @param id identifiant a utiliser pour remplacer une partie existante 
#' (si NULL, le numero de partie est automatiquement determine a partir du plus
#' grand numero deja present dans le dossier)
#'
#' @return 
#' La fonction renvoie le chemin vers le fichier Rdata cree ou remplace lors de 
#' l'enregistrement et contenant les scores de la partie ainsi qu'un visuel sur 
#' les scores et les resultats totaux des joueuses
#' 
#' @examples
#' save_game(
#'   scores = list(
#'     c("antoine", 1, 3, 6, 9, 0, 0, 6, 10, 15), 
#'     c("rose", 5, 10, 5, 6, 10, 3, 4, 9, 5)
#'   ),
#'   dir = "games_results"
#' )
#'        
#' @export
save_game <- function(scores, 
                      dir = "data", 
                      id = NULL) {
  
  # CHECK PARAMS -------------------------------------------------------------
  if (!is.list(scores)) stop("le parametre 'scores' doit etre une liste de vecteurs")
  if (length(scores) < 2 || length(scores) > 6) stop("il ne peut y avoir qu'entre 2 et 6 joueuses")
  
  for (i in seq_along(scores)) {
    v <- scores[[i]]
    
    # Verification de la longueur des vecteurs
    if (length(v) != 10) {
      stop("le vecteur de la joueuse ", i, " doit contenir exactement 1 nom et 9 scores")
    }
    
    # Verification du nom de la joueuse
    if (!is.character(v[1]) || length(v[1]) != 1) {
      stop("le premier element du vecteur ", i, " doit etre le nom de la joueuse")
    }
    
    # Verification des scores des joueuses
    vals <- suppressWarnings(as.numeric(v[-1]))
    if (any(is.na(vals))) {
      stop("les 9 scores de la joueuse ", v[1], " doivent etre numeriques")
    }
    if (any(vals < 0)) {
      stop("les 9 scores de la joueuse ", v[1], " doivent etre positifs ou nuls")
    }
    if (any(vals %% 1 != 0)) {
      stop("les 9 scores de la joueuse ", v[1], " doivent etre des entiers")
    }
  }
  
  # PROCESSING ---------------------------------------------------------------
  # Creation du dossier contenant les fichiers s'il n'existe pas
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  # Liste des parties existantes
  fichiers <- list.files(
    dir,
    pattern = "^faraway_game_\\d{4}\\.csv$",
    full.names = TRUE
  )
  
  # Transformation du vecteur en data.frame
  df <- do.call(rbind, lapply(scores, function(x) {
    data.frame(
      joueuse = as.character(x[1]),
      t(as.numeric(x[-1])),
      stringsAsFactors = FALSE
    )
  }))
  rownames(df) <- NULL
  
  # Renommage des colonnes des scores
  names(df) <- c("joueuse", "C8", "C7", "C6", "C5", "C4", "C3", "C2", "C1", "CS")
  
  # Verification des noms de joueuses precedemment enregistrees
  joueuses <- character()

  if (length(fichiers) > 0) {
    for (f in fichiers) {
      ancien_df <- read.csv(f, stringsAsFactors = FALSE)
      joueuses <- union(joueuses, ancien_df$joueuse)
    }
    
    nouvelles <- setdiff(df$joueuse, joueuses)
    
    if (length(nouvelles) > 0) {
      message(
        "nouvelles joueuses detectees : ",
        paste(nouvelles, collapse = ", ")
      )
      
      answer <- readline("confirmer l'enregistrement ? (o/n) : ")
      
      if (tolower(answer) != "o") {
        stop("ENREGISTREMENT ANNULE")
      }
    }
  }
  
  # Gestion de l'identifiant
  # S'il n'y en a pas un de fourni, creation d'un identifiant
  if (is.null(id)) {
    if (length(fichiers) == 0) {
      id_num <- 1
    } else {
      ids <- sub("^faraway_game_(\\d{4})\\.csv$", "\\1", basename(fichiers))
      ids <- suppressWarnings(as.integer(ids))
      ids <- ids[!is.na(ids)]
      id_num <- ifelse(length(ids) == 0, 1, max(ids) + 1)
    }
    id <- sprintf("%04d", id_num)
  } else {
    # S'il y en a un de fourni, verification de son format
    if (!grepl("^faraway_game_\\d{4}$", id)) {
      stop("le parametre 'id' doit etre une chaine a 4 chiffres")
    }
    if (file.exists(file.path(dir, paste0(id, ".csv")))) {
      answer <- readline(
        paste0("ATTENTION : la partie ", id, " existe deja, confirmer l'ecrasement ? (o/n) : ")
      )
      if (tolower(answer) != "o") {
        stop("ENREGISTREMENT ANNULE")
      }
    } else {
      message("ATTENTION : la partie ", id, " va etre creee (pas d'ecrasement)")
    }
  }
  
  # Nommage final du fichier
  file <- file.path(dir, paste0("faraway_game_", id, ".csv"))
  
  # Enregistrement et affichage dans la console
  write.csv(df, file = file)
  print(df)
  
  return(file)
}

# PARTIES --------------------------------------------------------------------
## 2024 ----
### [01 - 10] ----
save_game(
  list(
    c("antoine", 0, 0, 9, 0, 2, 0, 24, 19, 7),
    c("rose", 6, 6, 8, 10, 4, 12, 10, 12, 7),
    c("franck", 1, 4, 4, 6, 8, 8, 8, 18, 7)
  ), dir = "data"
)
save_game(
  list(
    c("gabriel", 0, 0, 6, 7, 6, 2, 20, 14, 11),
    c("alain", 0, 16, 4, 0, 12, 3, 4, 5, 16),
    c("franck", 0, 9, 4, 0, 12, 12, 15, 15, 5)
  ), dir = "data"
)
save_game(
  list(
    c("rose", 2, 0, 9, 6, 3, 0, 4, 14, 7),
    c("antoine", 4, 10, 8, 0, 18, 12, 10, 6, 14)
  ), dir = "data"
)
save_game(
  list(
    c("baltazar", 6, 0, 0, 15, 12, 9, 8, 12, 14),
    c("gabriel", 4, 2, 10, 4, 2, 4, 14, 16, 5),
    c("franck", 0, 0, 0, 10, 8, 2, 8, 0, 19),
    c("rose", 8, 0, 0, 0, 0, 0, 12, 20, 11)
  ), dir = "data"
)
save_game(
  list(
    c("rose", 0, 0, 2, 0, 10, 15, 8, 13, 9),
    c("franck", 0, 0, 4, 6, 12, 0, 12, 19, 3),
    c("valentine", 0, 0, 4, 0, 9, 0, 16, 9, 9)
  ), dir = "data"
)
save_game(
  list(
    c("franck", 4, 0, 2, 9, 0, 6, 14, 0, 5),
    c("rose", 0, 4, 4, 6, 12, 12, 10, 15, 8),
    c("valentine", 0, 7, 0, 9, 6, 6, 18, 8, 7)
  ), dir = "data"
)
save_game(
  list(
    c("rose", 6, 8, 3, 10, 0, 18, 0, 16, 4),
    c("franck", 10, 6, 6, 2, 9, 0, 3, 8, 24),
    c("valentine", 0, 2, 0, 9, 9, 13, 6, 19, 7)
  ), dir = "data"
)
save_game(
  list(
    c("valentine", 0, 0, 2, 0, 2, 12, 9, 9, 17),
    c("rose", 0, 0, 7, 9, 4, 8, 0, 6, 16),
    c("franck", 0, 3, 0, 13, 15, 12, 9, 4, 9)
  ), dir = "data"
)
save_game(
  list(
    c("baltazar", 19, 6, 0, 16, 12, 12, 9, 7, 21),
    c("gabriel", 6, 2, 4, 10, 6, 6, 18, 9, 14),
    c("franck", 2, 10, 9, 7, 0, 10, 13, 10, 11),
    c("rose", 1, 0, 20, 14, 21, 8, 6, 4, 18)
  ), dir = "data"
)
save_game(
  list(
    c("valentine", 0, 18, 0, 0, 6, 9, 4, 0, 18),
    c("rose", 3, 0, 0, 0, 5, 2, 6, 20, 0)
  ), dir = "data"
)

### [11 - 20] ----
save_game(
  list(
    c("antoine", 0, 4, 8, 5, 15, 0, 4, 8, 4),
    c("valentine", 0, 3, 0, 10, 1, 6, 9, 21, 11),
    c("franck", 0, 10, 2, 0, 13, 9, 0, 16, 13),
    c("rose", 0, 3, 6, 12, 9, 12, 18, 12, 6)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 12, 0, 4, 6, 20, 8, 0, 18, 12),
    c("rose", 0, 5, 0, 8, 6, 10, 6, 0, 12)
  ), dir = "data"
)
save_game(
  list(
    c("rose", 8, 0, 0, 6, 12, 4, 10, 18, 0),
    c("eve", 3, 2, 16, 10, 3, 0, 10, 14, 18),
    c("antoine", 13, 9, 12, 6, 0, 12, 7, 17, 9),
    c("valentine", 4, 2, 0, 0, 20, 12, 4, 19, 3)
  ), dir = "data"
)
save_game(
  list(
    c("valentine", 8, 6, 2, 6, 9, 8, 8, 10, 17),
    c("rose", 10, 10, 9, 9, 7, 12, 0, 16, 5)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 0, 3, 0, 0, 0, 0, 3, 12, 15),
    c("rose", 0, 0, 12, 10, 8, 3, 9, 9, 7)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 0, 9, 6, 9, 9, 3, 8, 5, 12),
    c("rose", 0, 12, 8, 9, 10, 0, 0, 20, 10)
  ), dir = "data"
)
save_game(
  list(
    c("rose", 6, 0, 0, 6, 9, 9, 8, 8, 12),
    c("antoine", 0, 20, 7, 0, 9, 16, 0, 8, 10)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 0, 5, 3, 0, 0, 18, 16, 12, 12),
    c("valentine", 0, 0, 8, 10, 10, 0, 0, 20, 3),
    c("eve", 0, 3, 4, 6, 0, 19, 0, 12, 13),
    c("rose", 0, 2, 6, 6, 0, 10, 17, 14, 7)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 0, 12, 0, 0, 0, 4, 0, 20, 6),
    c("rose", 0, 6, 7, 12, 9, 8, 10, 15, 5)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 2, 15, 0, 9, 7, 12, 8, 15, 7),
    c("rose", 0, 18, 0, 12, 0, 12, 9, 15, 14)
  ), dir = "data"
)

### [21 - 30] ----
save_game(
  list(
    c("antoine",  9,  0,  3,  8, 15,  0,  3,  9, 10),
    c("rose",     0, 18,  4, 10,  8,  9,  8, 10,  5)
  ), dir = "data"
)
save_game(
  list(
    c("antoine",  2,  0,  0, 12,  0,  0,  5, 10, 25),
    c("rose",     6,  0,  0, 14,  0,  9,  4, 10, 13)
  ), dir = "data"
)
save_game(
  list(
    c("mateo",    8,  0,  9, 14,  4, 16, 10,  3, 11),
    c("rose",     0,  7,  8,  6,  9, 12,  8, 20,  7),
    c("antoine", 20,  0,  0, 15,  8,  4,  3,  2, 16)
  ), dir = "data"
)
save_game(
  list(
    c("rose",     9, 13, 16,  9,  6,  2, 12, 12,  3),
    c("franck",   0,  3, 10,  0,  0,  9, 20, 10, 18),
    c("antoine",  3,  7,  4,  8,  0, 24, 14, 12, 11)
  ), dir = "data"
)
save_game(
  list(
    c("franck",   3,  0,  0,  0,  0,  8, 20,  8,  8),
    c("mateo",    0,  2,  0,  2,  0, 12, 14, 24,  4),
    c("rose",    18,  6,  4, 19, 15,  6,  8,  4, 21),
    c("antoine",  0,  0,  9,  6,  7,  6, 17, 16, 19)
  ), dir = "data"
)
save_game(
  list(
    c("rose",     0,  0,  6, 19, 15, 20, 20, 8, 8),
    c("antoine",  0,  0, 18,  0, 0, 20, 9, 20, 25)
  ), dir = "data"
)
save_game(
  list(
    c("rose",     0,  8,  4, 15, 16,  0,  7, 18, 17),
    c("franck",   4,  0,  6,  5,  4, 19,  6, 20,  6),
    c("antoine",  0,  0,  9,  8, 10, 12, 10,  0, 13)
  ), dir = "data"
)
save_game(
  list(
    c("franck",   2,  0,  0,  0, 10,  8, 18, 10, 12),
    c("antoine",  3,  0,  0, 20, 15, 12,  0,  5, 11),
    c("rose",     2, 19,  3,  0,  9,  0, 10, 24,  3)
  ), dir = "data"
)
save_game(
  list(
    c("rose",    15,  0, 13,  0,  6,  9,  7,  6, 19),
    c("antoine", 12, 10,  0,  0, 17, 10,  4, 10, 15)
  ), dir = "data"
)
save_game(
  list(
    c("franck",   7,  4,  3,  8, 12, 10, 18, 15, 12),
    c("rose",     2,  0,  0,  6,  4,  0,  0,  6, 15),
    c("antoine",  0,  0, 10,  0, 12,  8,  0, 24,  0)
  ), dir = "data"
)

### [31 - 39] ----
save_game(
list(
  c("franck",   0,  8,  0, 10,  0, 16, 12,  6,  8),
  c("antoine",  0,  0, 10,  0,  8, 12,  0,  6, 19),
  c("rose",     4,  4,  6,  4,  6, 15, 16,  9,  7)
), dir = "data"
)
save_game(
list(
  c("antoine",  0,  0,  0,  0, 10,  0, 18, 20, 15),
  c("rose",    15,  4, 16,  0,  4, 18, 12,  6,  9)
), dir = "data"
)
save_game(
list(
  c("antoine",  0,  0,  0, 15,  0, 12,  6,  8, 15),
  c("rose",     4,  7, 12,  6, 19, 15,  9, 12,  0)
), dir = "data"
)
save_game(
list(
  c("antoine",  0,  2, 10, 10, 12, 10, 24, 10,  7),
  c("rose",     0,  0, 15,  7, 10,  0, 20, 15, 13)
), dir = "data"
)
save_game(
list(
  c("franck",   0,  0,  4,  0, 17, 21, 15, 21,  6),
  c("rose",     2,  0,  7, 16,  4, 12, 10,  8,  8),
  c("antoine",  6,  9,  6,  6, 18, 14,  3, 16, 22)
), dir = "data"
)
save_game(
list(
  c("samuel",      2,  0,  0,  6,  6,  4,  0,  7, 19),
  c("stephane",    0,  0,  2, 10,  2,  8,  4, 16, 11),
  c("christelle", 18,  9,  0, 20, 24, 10,  5,  0,  6),
  c("oceane",      0,  0,  6,  6,  0, 12,  6, 20,  1),
  c("antoine",     0,  0,  0,  0,  2, 19, 12,  6, 15)
), dir = "data"
)
save_game(
list(
  c("antoine",  9,  3,  5,  6,  0, 14,  8,  2, 20),
  c("rose",    18,  0,  6,  2,  6,  0,  0,  8,  6)
), dir = "data"
)
save_game(
  list(
    c("rose",     0,  3,  0, 12,  0, 12, 20,  4, 16),
    c("mateo",   17, 13,  7, 15,  0, 20,  9, 10,  9),
    c("antoine",  6,  0, 16,  8,  4,  0, 20,  0, 14)
  ), dir = "data"
)
save_game(
  list(
    c("mateo",    0,  0,  3,  8,  7, 24,  6, 16,  4),
    c("antoine",  0,  0, 19,  0, 14, 12, 12,  0, 14),
    c("rose",     0,  8,  0, 15, 20,  2, 18, 18,  4)
  ), dir = "data"
)


## 2025 ----
# debut des extensions
### [40 - 50] ----
save_game(
  list(
    c("franck",  2,  0,  0,  0, 12, 20, 14, 12, 15),
    c("rose",    0,  0,  6,  0,  0,  0,  4, 20, 19)
  ), dir = "data"
)
save_game(
  list(
    c("franck",  0,  4,  2,  0,  0,  0, 13,  0, 13),
    c("rose",    0, 20,  6,  7,  3, 28,  6, 10, 18)
  ), dir = "data"
)
save_game(
  list(
    c("rose",    13,  6,  0,  0, 18, 18, 12,  7, 15),
    c("antoine",  9,  8,  6, 14, 28, 12,  6,  0, 17)
  ), dir = "data"
)
save_game(
  list(
    c("rose",     0,  0,  0,  0,  0, 12,  0, 10, 17),
    c("emilie",   9,  0,  0,  0,  7,  0, 18,  0, 10),
    c("antoine",  0,  9, 20, 10,  8, 24,  4,  3,  8)
  ), dir = "data"
)
save_game(
  list(
    c("rose",     9, 14,  9,  7, 12, 10, 21, 10, 12),
    c("emilie",   0,  0, 14, 12,  6,  9,  0,  0, 26),
    c("antoine",  0,  0,  0,  6, 12,  0, 21, 17, 20)
  ), dir = "data"
)
save_game(
  list(
    c("rose",    14, 18, 15, 20,  9,  7,  6,  3, 12),
    c("antoine",  4,  4,  0,  4, 12, 10,  8, 12, 24)
  ), dir = "data"
)
save_game(
  list(
    c("antoine",  0,  6, 13, 12,  0,  0, 20,  0, 11),
    c("emilie",  16,  0, 15, 18, 21,  2,  0,  5,  2),
    c("rose",     5, 12, 18,  0, 20, 14,  9,  3, 13)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 12,  5,  4,  0,  0,  4, 17,  4, 26),
    c("emilie",  20,  5,  0,  8,  0,  0,  6, 10, 14),
    c("rose",     9,  3,  7,  8,  6,  6, 14, 18, 17)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 12,  3,  0,  0,  4, 14, 19, 16, 12),
    c("emilie",   6,  6,  0, 16,  3,  4,  0, 18, 15),
    c("rose",    14,  0, 17,  6, 10, 10, 10, 10,  6)
  ), dir = "data"
)
save_game(
  list(
    c("emilie",   0,  4, 12,  5,  0,  7,  3, 13, 12),
    c("antoine", 19, 10,  0,  2,  4, 15,  0,  4,  5),
    c("rose",     6, 15, 12, 13, 12, 11,  8,  0,  4)
  ), dir = "data"
)
save_game(
  list(
    c("emilie",   4,  0,  8,  0, 10,  0,  2, 12,  2),
    c("antoine",  6,  0,  0,  0,  0,  6,  6, 28,  6),
    c("rose",     3,  0,  0,  8,  0,  0,  4, 20, 10)
  ), dir = "data"
)

### [51 - 54] ----
save_game(
  list(
    c("rose",     0,  9,  7,  3, 18, 21,  6,  0,  7),
    c("emilie",   0,  0,  0,  5,  0, 20,  6, 17, 25),
    c("antoine", 10, 10,  0,  8,  6, 10, 21,  0, 18)
  ), dir = "data"
)
save_game(
  list(
    c("antoine", 20,  0,  6, 0,  0, 15, 15, 15, 16),
    c("rose",     0, 28, 10, 8, 17, 18,  2,  0, 13)
  ), dir = "data"
)
save_game(
  list(
    c("rose",    18, 10,  0,  0, 12, 19,  0, 10, 11),
    c("antoine",  6,  0,  0,  0, 15,  0, 28, 16,  8)
  ), dir = "data"
)
save_game(
  list(
    c("rose",     0, 15,  9,  4, 12, 14,  5,  6, 13),
    c("antoine",  7,  3,  0,  0,  0, 13, 19, 16, 14)
  ), dir = "data"
)

## 2026 ----
### [55 - 60] ----
save_game(
  list(
    c("rose",       0,  4,  0,  0,  6,  6, 16,  9, 11),
    c("valentine", 10, 21,  7, 10, 15,  6,  0,  2, 11),
    c("antoine",    0,  0,  3,  0,  3,  5, 10,  6, 22)
  ), dir = "data"
)
save_game(
  list(
    c("rose",     0,  9, 14, 15, 12,  8,  6, 12, 11),
    c("franck",   6,  0,  5,  0,  8,  8, 14, 10, 19),
    c("antoine", 21,  0,  6,  2,  4, 18, 18, 14, 23)
  ), dir = "data"
)
save_game(
  list(
    c("mateo",   12, 19,  7, 15,  9,  4,  0, 12,  4),
    c("rose",     0,  0,  2,  9,  7,  8, 15, 18,  4),
    c("antoine", 24, 14, 14,  7,  0,  0,  5, 13, 21)
  ), dir = "data"
)
save_game(
  list(
    c("emilie",   0,  0,  0,  0, 18,  8,  5, 13, 21),
    c("rose",     0,  0,  6,  8,  8,  0, 15,  9, 17),
    c("antoine", 14,  0,  6,  4, 19, 12,  4,  0, 24)
  ), dir = "data"
)
save_game(
  list(
    c("franck",   7,  0,  4,  9,  6,  7,  0,  0, 30),
    c("rose",     6,  3,  8,  0, 10,  0, 20, 10, 12),
    c("antoine", 26,  9,  0,  0,  0, 19,  0,  2, 14)
  ), dir = "data"
)
save_game(
  list(
    c("mateo",    0,  2,  0,  9,  4, 14,  0,  6,  7),
    c("rose",     3,  2,  0,  0, 15, 18,  5, 21, 15),
    c("franck",   6,  4,  0,  7,  4, 26, 15, 15,  8),
    c("antoine",  8,  9,  4,  8, 12,  9, 10, 20, 17)
  ), dir = "data"
)

### [61 - 70] ----
save_game(
  list(
    c("mateo",   24, 15,  0, 18, 12,  4,  8,  0, 17),
    c("eve",     15,  8, 12,  4, 18,  9,  9, 19, 18),
    c("antoine", 17,  4,  1,  0, 32,  0,  9,  9, 13)
  ), dir = "data"
)
save_game(
  list(
    c("franck",   0,  0,  0,  0, 14, 19,  3,  5, 30),
    c("eve",     10,  4,  0, 14, 10,  7,  8,  2, 16),
    c("mateo",    0,  6,  0,  0, 28, 20,  6,  0,  0),
    c("rose",    10, 18,  6,  2,  9,  9,  6,  3,  9),
    c("antoine", 20, 14, 19,  0, 16,  0,  3,  8,  6)
  ), dir = "data"
)
