# Installazione e caricamento librerie
if (!require("rlas")) install.packages("rlas")

library(rlas)

# --------------------------------------------------
# INPUT UTENTE: Inserire il percorso della cartella
# --------------------------------------------------
cartella_nuvole <- "E:/Università/Tesi magistrale/Elaborazioni R/Directory"

# Rilevamento automatico di tutti i file .las (o .laz) presenti nella cartella
file_paths <- list.files(
  path = cartella_nuvole, 
  pattern = "\\.(las|laz)$", # Cerca sia .las che .laz (case-insensitive)
  full.names = TRUE, 
  ignore.case = TRUE
)

# Verifica se sono stati trovati file
if (length(file_paths) == 0) {
  cat("ATTENZIONE: Nessun file .las o .laz trovato nella cartella specificata.\n")
} else {
  cat("Trovati", length(file_paths), "file nella cartella:\n")
  print(basename(file_paths))
}

# Impostiamo i parametri di controllo
soglia_punti <- 100000
nuvole_list <- list()

# --------------------------------------------------
# CICLO DI DIAGNOSTICA SU TUTTI I FILE TROVATI
# --------------------------------------------------
if (length(file_paths) > 0) {
  
  for (i in seq_along(file_paths)) {
    path <- file_paths[i]
    nome_file <- basename(path)
    
    cat("\n==================================================\n")
    cat("ANALISI PRELIMINARE NUVOLA", i, "di", length(file_paths), "\n")
    cat("File:", nome_file, "\n")
    cat("==================================================\n")
    
    # Caricamento veloce con rlas
    df_nuvola <- read.las(path)
    n_totale <- nrow(df_nuvola)
    
    cat("Numero totale punti:", n_totale, "\n")
    
    # --------------------------------------------------
    # DIAGNOSTICA DINAMICA DEI RITORNI (RETURN NUMBER)
    # --------------------------------------------------
    if ("ReturnNumber" %in% names(df_nuvola)) {
      max_ritorni <- max(df_nuvola$ReturnNumber, na.rm = TRUE)
      cat("\n--- Distribuzione dei Ritorni (Max ritorni rilevati:", max_ritorni, ") ---\n")
      
      tab_ritorni <- table(df_nuvola$ReturnNumber)
      
      # Ciclo dinamico dal 1° al valore massimo rilevato nella nuvola
      for (r in 1:max_ritorni) {
        conteggio <- ifelse(as.character(r) %in% names(tab_ritorni), tab_ritorni[as.character(r)], 0)
        percentuale <- round((conteggio / n_totale) * 100, 2)
        
        cat(sprintf("  • Ritorno %d: %d punti (%s%%)\n", r, conteggio, percentuale))
      }
    } else {
      cat("\nAttributo 'ReturnNumber' non presente nel file.\n")
    }
    
    # --------------------------------------------------
    # GESTIONE CONDIZIONALE DEL CAMPIONAMENTO
    # --------------------------------------------------
    if (n_totale <= soglia_punti) {
      punti_work <- df_nuvola
      cat("\nStato elaborazione: Meno di", soglia_punti, "punti. Uso integrale (100%).\n")
    } else {
      set.seed(50)
      idx_campione <- sample(1:n_totale, soglia_punti)
      punti_work <- df_nuvola[idx_campione, ]
      cat("\nStato elaborazione: Oltre", soglia_punti, "punti. Campione estratto di", soglia_punti, "punti.\n")
    }
    
    # Salviamo il dataframe elaborato con il nome originale del file
    nuvole_list[[nome_file]] <- punti_work
  }
}

# --------------------------------------------------
# FUNZIONE PER IL CALCOLO DELLE METRICHE SU GRIGLIA
# --------------------------------------------------
calcola_metriche_nuvola <- function(df, grid_size = 5) {
  
  # Assegnazione di ogni punto a una tessera (grid_x, grid_y)
  df$grid_x <- floor(df$X / grid_size) * grid_size
  df$grid_y <- floor(df$Y / grid_size) * grid_size
  
  # Calcolo dell'area reale dell'estensione del ritaglio (Bounding Box XY in m2)
  area_tot_m2 <- (max(df$X) - min(df$X)) * (max(df$Y) - min(df$Y))
  
  # Densità media globale riferita alla superficie totale del ritaglio (punti/m2)
  densita_tot <- nrow(df) / ifelse(area_tot_m2 > 0, area_tot_m2, 1)
  
  # Calcolo delle metriche per singola tessera
  metriche_list <- by(df, list(df$grid_x, df$grid_y), function(sub) {
    if (nrow(sub) < 5) return(NULL) # Ignora celle con pochissimi punti
    
    n_punti <- nrow(sub)
    area_cell <- grid_size * grid_size
    densita_locale <- n_punti / area_cell
    
    # Quota e Rugosità
    z_min <- min(sub$Z)
    z_max <- max(sub$Z)
    rugosita_sd <- sd(sub$Z)       # Deviazione standard delle altezze
    rugosita_range <- z_max - z_min # Range altimetrico
    
    # Stima Pendenza (Slope) tramite fit di un piano locale Z ~ X + Y
    fit <- lm(Z ~ X + Y, data = sub)
    coefs <- coef(fit)
    dz_dx <- ifelse(is.na(coefs["X"]), 0, coefs["X"])
    dz_dy <- ifelse(is.na(coefs["Y"]), 0, coefs["Y"])
    
    slope_deg <- atan(sqrt(dz_dx^2 + dz_dy^2)) * (180 / pi)
    
    # Proporzione sottobosco reale: calcolata rispetto al piano inclinato locale
    z_residui <- abs(residuals(fit))
    punti_sottobosco <- sum(z_residui >= 0.10 & z_residui <= 1.50)
    prop_sottobosco <- (punti_sottobosco / n_punti) * 100
    
    # Penetrazione: Ratio tra ultimi ritorni di impulsi multipli e primi ritorni
    n_primi <- sum(sub$ReturnNumber == 1)
    n_ultimi_multipli <- sum(sub$ReturnNumber > 1 & sub$ReturnNumber == sub$NumberOfReturns)
    ratio_penetrazione <- if (n_primi > 0) (n_ultimi_multipli / n_primi) * 100 else 0
    
    # Output completo della tessera
    return(data.frame(
      Grid_X = sub$grid_x[1],
      Grid_Y = sub$grid_y[1],
      N_Punti = n_punti,
      Densita_pts_m2 = round(densita_locale, 2), # Densità locale della tessera 5x5m
      Densita_pts_tot = round(densita_tot, 2),   # Densità complessiva dell'intera nuvola
      Rugosita_SD_m = round(rugosita_sd, 3),
      Rugosita_Range_m = round(rugosita_range, 3),
      Slope_deg = round(slope_deg, 2),
      Prop_Sottobosco_pct = round(prop_sottobosco, 2),
      Ratio_Ritorni_Multipli_pct = round(ratio_penetrazione, 2)
    ))
  })
  
  # Unione dei risultati delle tessere in una singola tabella
  risultato <- do.call(rbind, metriche_list)
  rownames(risultato) <- NULL
  return(risultato)
}

# --------------------------------------------------
# ESECUZIONE DEL CALCOLO SU TUTTI I FILE PROCESSATI
# --------------------------------------------------
risultati_metriche <- list()

for (nome_file in names(nuvole_list)) {
  cat("\nCalcolo metriche per la tessera (5x5m) su:", nome_file, "...\n")
  
  df_work <- nuvole_list[[nome_file]]
  dt_metriche <- calcola_metriche_nuvola(df_work, grid_size = 5)
  
  # Salviamo la tabella con le metriche
  risultati_metriche[[nome_file]] <- dt_metriche
  
  cat("Elaborate", nrow(dt_metriche), "tessere da 5x5m per il file", nome_file, "\n")
}

# --------------------------------------------------
# FUNZIONE PER IL CALCOLO DELLE STATISTICHE DI SINTESI
# --------------------------------------------------
calcola_sintesi_nuvola <- function(df_tessere, nome_nuvola) {
  
  # Colonne numeriche delle metriche incluse nella sintesi
  colonne_metriche <- c("Densita_pts_m2", "Densita_pts_tot", "Rugosita_SD_m", 
                        "Rugosita_Range_m", "Slope_deg", "Prop_Sottobosco_pct", 
                        "Ratio_Ritorni_Multipli_pct")
  
  colonne_valid <- intersect(colonne_metriche, names(df_tessere))
  sintesi_list <- list()
  
  for (col in colonne_valid) {
    vals <- df_tessere[[col]]
    vals <- vals[!is.na(vals)] # Rimuove eventuali valori NA
    
    sintesi_list[[col]] <- data.frame(
      Nuvola = nome_nuvola,
      Metrica = col,
      Media = round(mean(vals), 3),
      Min = round(min(vals), 3),
      Max = round(max(vals), 3),
      Dev_Std = round(sd(vals), 3),
      Range = round(max(vals) - min(vals), 3),
      Varianza = round(var(vals), 3)
    )
  }
  
  return(do.call(rbind, sintesi_list))
}

View(risultati_metriche)
# --------------------------------------------------
# AGGREGAZIONE SU TUTTE LE NUVOLE NELLA LISTA
# --------------------------------------------------
lista_sintesi <- list()

for (nome_file in names(risultati_metriche)) {
  df_tess <- risultati_metriche[[nome_file]]
  lista_sintesi[[nome_file]] <- calcola_sintesi_nuvola(df_tess, nome_file)
}

# Uniamo tutto in un'unica tabella finale chiara e compatta
report_sintesi_globale <- do.call(rbind, lista_sintesi)
rownames(report_sintesi_globale) <- NULL

# Visualizziamo il report sintetico finale
View(report_sintesi_globale)
write.csv(report_sintesi_globale,"report_sintesi_globale.csv",row.names = FALSE)
