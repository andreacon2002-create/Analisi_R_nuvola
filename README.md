# 🌲 Diagnostica nuvola in R

Questo repository contiene uno script in R sviluppato per analizzare delle nuvole, cosa che in prima istanza, all'inzio di lavori di pulizia della nuvola stessa o elaborazioni, può andare a guidare le scelte che faremo o indirizzarci verso alcuni algoritmi, rispetto ad altri.


## 🛠️ Librerie
- **Linguaggio:** R (versione 4.x o superiore)
- **Pacchetto chiave:** "rlas" per la lettura e la gestione efficiente di nuvole di punti LiDAR e dati spaziali

## Cosa fa lo script

L'algoritmo implementa una pipeline automatizzata in R per la diagnostica e l'estrazione di metriche strutturali da nuvole di punti LiDAR (.las / .laz), strutturata in quattro fasi:
- Lettura e Diagnostica Preliminare: Scansione della directory, caricamento dei file tramite rlas e analisi della distribuzione dei ritorni laser (Return Number).
- Campionamento Adattivo: Se il numero totale di punti di una nuvola supera una soglia prefissata (100.000 punti), l'algoritmo sottoseziona automaticamente il dataset estraendo un campione casuale rappresentativo (tramite seed riproducibile); in caso contrario, elabora il 100\% dei punti.
- Tassellazione e Calcolo delle Metriche (Griglia 5 x 5 m):Lo spazio viene suddiviso in una griglia regolare. Per ogni tessera vengono estratte le seguenti metriche strutturali:
    -     Densità: Conteggio locale dei punti (punti/m^2) e densità globale.
    -     Morfologia e Rugosità: Estremi altimetrici, range e deviazione standard delle quote (Z).
    -     Pendenza (Slope): Inclinazione locale calcolata interpolando un piano di regressione
    -     Sottobosco: Percentuale di punti compresi tra 0.10 m e 1.50 m rispetto al piano inclinato locale (analisi dei residui).
    - Penetrazione: Rapporto percentuale tra ultimi ritorni multipli e primi ritorni.
- Aggregazione e Sintesi Globale: I risultati delle tessere vengono assemblati per singolo file e aggregati in indicatori statistici descrittivi (media, min, max, deviazione standard, range e varianza), salvati in un report globale in formato .csv.

## ⚙️ Istruzioni per l'uso
1. lo script lavora all'interno della cartella dove viene salvato, per cui le nuvole che di cui si vuole fare la diagnosi devono essere portante anch'esse all'interno della stessa.
---
*Progetto sviluppato da Andrea Conforto.*
