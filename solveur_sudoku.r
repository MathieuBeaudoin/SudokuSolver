probleme <- read.csv("sudoku.csv", header=F, sep=";")
probleme[is.na(probleme)] <- 0

# Pour generer les matrices 3x3
sous_mat <- function(i, j, matrice) {
  m <- i-(i-1)%%3
  n <- j-(j-1)%%3
  return(matrice[m:(m+2),n:(n+2)])
}

# Verifie la legalite d'un chiffre dans une case particuliere
permis <- function(i, j, c, matrice){
  if(matrice[i,j]==c) return(T)
  else {
    if(sum(matrice[i,]==c)>0 |
       sum(matrice[,j]==c)>0 |
       sum(sous_mat(i,j,matrice)==c)>0) return(F)
    else return(T)
  }
}

# Conversion des indices
mat2vec <- function(i,j) return((i-1)*9 + j)
vec2mat <- function(k) {
  i <- ((k-1)-(k-1)%%9)/9 + 1
  j <- (k - 9*(i-1))
  return(c(i,j))
}

# Determine s'il est possible de placer le chiffre ailleurs
# dans la rangee, la colonne ET la sous-matrice
permis_ailleurs <- function(i,j,k,matrice) {
  permis <- matrix(matrice[,k], byrow=T, nrow=9)
  return(sum(permis[i,])>1 & sum(permis[,j])>1 & sum(sous_mat(i,j,permis))>1 )
}

resoudre <- function(sudoku) {
  
  # Vecteur de valeurs figees
  figer <- rep(0,81)
  for(i in 1:81) if(sudoku[vec2mat(i)[1],vec2mat(i)[2]] != 0) figer[i] <- 1
  
  # Sortir si le casse-tete est complet
  if(sum(figer)==81) return(sudoku)
  
  # Matrice de valeurs legales
  matrice_permis <- matrix(rep(0,81*9),nrow=81)
  for(n in 1:81){
    coords_sudoku <- vec2mat(n)
    for(m in 1:9) {
      if(figer[n]==1) {
        if(sudoku[coords_sudoku[1],coords_sudoku[2]] == m) matrice_permis[n,m] <- 1
        else matrice_permis[n,m] <- 0
      }
      else {
        if(permis(coords_sudoku[1],coords_sudoku[2],m,sudoku)) matrice_permis[n,m] <- 1
      }
    }
  }
  
  ### ELIMINATION ###
  
  # Decompte du nombre de valeurs legales pour chaque case du sudoku
  en_jeu <- which(figer==0)
  possibles <- rep(1, 81)
  for(h in 1:81) {
    if(h %in% en_jeu) possibles[h] <- sum(matrice_permis[h,])
  }
  
  # Si certaines cases n'ont aucune valeur legale, on sort
  if(sum(possibles==0)>0) return(F)
  
  # Sinon, on verifie s'il existe des cases avec une seule solution possible
  modifier <- (1-figer)*possibles==1
  if(sum(modifier)>0) {
    for(h in 1:81) {
      if(modifier[h]) {
        coords <- vec2mat(h)
        sudoku[coords[1],coords[2]] <- which(matrice_permis[h,]==1)
      }
    }
    # On repete!
    sudoku <- resoudre(sudoku)
  }
  
  # Sinon, on prend le minimum de nombre de solutions possibles parmi les
  # cases ou le nombre n'est pas deja fige
  else {
    masterbreak <- F  # Interrupteur lie au premier test
    choix_min <- min(possibles[which(possibles!=1)])
    modifier <- (1-figer)*possibles == choix_min
    
    # Premier test: existe-t-il des cases ou l'un des chiffres permis n'a 
    # nulle part d'autre ou aller?
    for(h in 1:81) {
      if(modifier[h]) {
        coords <- vec2mat(h)
        i <- coords[1]
        j <- coords[2]
        for(k in which(matrice_permis[h,]==1)) {
          if(!permis_ailleurs(i,j,k,matrice_permis)) {
            sudoku[i,j] <- k
            sudoku <- resoudre(sudoku)
            masterbreak <- T
            break
          }
        }
      }
      if(masterbreak) break
    }
    # Si le premier test echoue, on y va par force brute
    for(h in 1:81) {
      if(masterbreak) break
      if(modifier[h]) {
        coords <- vec2mat(h)
        i <- coords[1]
        j <- coords[2]
        for(k in which(matrice_permis[h,]==1)) {
          sudoku_provisoire <- sudoku
          sudoku_provisoire[i,j] <- k
          sudoku_provisoire <- resoudre(sudoku_provisoire)
          if(sudoku_provisoire!=F) {
            sudoku <- sudoku_provisoire
            masterbreak <- T
            break
          }
        }
      }
    }
  }
  return(sudoku)
}

resultat <- resoudre(probleme)
resultat
