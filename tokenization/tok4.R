# Chargement des packages
#library(tokenizers)
library(dplyr)

# Fonction pour tokeniser le texte
tokenize_text <- function(text) {
  # Supprimer le signe "["
  text <- gsub("\\[", "", text)
  
  # Supprimer le signe "]"
  text <- gsub("\\]", "", text)
  
  # Utiliser gsub() avec une expression régulière pour supprimer les crochets et leur contenu
  text <- gsub("\\{.*?\\}", "", text, perl = TRUE)
    
  # Ajouter un espace après l'apostrophe
  text <- gsub("'", "' ", text)
  
  # Ajouter un espace après l'apostrophebis
  text <- gsub("’", "' ", text)
  
  # ajouter espace avant et après tirets
  
  text <- gsub("-", " - ", text)
  
  #ajouter espace avant point interro
  
  text <- gsub("\\?", " ?", text)
  
  # Ajouter un espace avant et après les signes de ponctuation spécifiés
  text <- gsub("([(),;.!:«»])", " \\1 ", text)
  
  # Diviser le texte en tokens en utilisant des espaces comme séparateur
  tokens <- unlist(strsplit(text, "\\s+"))
  
  # Supprimer les espaces vides et les chaînes vides
  tokens <- tokens[tokens != " " & tokens != ""]
  
  return(tokens)
}


# Fonction pour créer les colonnes requises
process_tokens <- function(tokens, file_name) {
  df <- data.frame(
    line_number = seq_along(tokens),
    file_name = rep(file_name, length(tokens)),
    tokens = tokens,
    lemma = rep("", length(tokens)),
    POS = rep("", length(tokens)),
    MORPH = rep("", length(tokens))
  )
  
  df$context_left <- sapply(seq_along(tokens), function(i) {
    context_tokens <- tokens[pmax(1, i - 5):pmin(length(tokens), i - 1)]
    paste(context_tokens, collapse = " ")
  })
  
  df$token <- tokens
  
  df$context_right <- sapply(seq_along(tokens), function(i) {
    context_tokens <- tokens[pmax(1, i + 1):pmin(length(tokens), i + 5)]
    paste(context_tokens, collapse = " ")
  })
  
  return(df)
}

# Fonction pour traiter un fichier
process_file <- function(file_path) {
  text <- readLines(file_path, warn = FALSE)
  tokens <- tokenize_text(text)
  file_name <- basename(file_path)
  result <- process_tokens(tokens, file_name)
  return(result)
}

# Répertoire d'entrée et de sortie
input_directory <- "input"
output_directory <- "output"

# Liste des fichiers texte à traiter
files_to_process <- list.files(path = input_directory, pattern = "\\.txt$", full.names = TRUE)

# Liste pour stocker les résultats
results_list <- list()

# Traitement de chaque fichier
for (file_path in files_to_process) {
  result <- process_file(file_path)
  results_list[[file_path]] <- result
}

# Combinaison de tous les résultats
final_result <- do.call(rbind, results_list)

# Sauvegarde du tableau au format tsv dans le répertoire de sortie
output_file <- file.path(output_directory, "output_table.tsv")
write.table(final_result, output_file, sep = "\t", row.names = FALSE)

cat("Le tableau a été sauvegardé avec succès dans", output_file, "\n")


