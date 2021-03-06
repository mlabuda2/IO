library(genalg)

fitness<- function(chr) {
  klauzule_true <- 0
  #Iteracja po każdym elemencie DF
  for (row in 1:nrow(cnf_data)){
    for (var in 1:3){
      element <- cnf_data[row, var]
      if (element < 0){
        if (!(chr[abs(element)] == 1)){
          klauzule_true <- klauzule_true + 1
          break
        }  
      }
      else{
        if (chr[element] == 1){
          klauzule_true <- klauzule_true + 1
          break
        }
      }  
    }
  }
  #print(klauzule_true)
  return(-klauzule_true)
}

program <- function(pop=100, rep=50, mut=0.05, elitism = T, sciezka='/home/mati/IO/problems/10_30_1.dimacs') {
  
  czytaj_plik <- function(sciezka) {
    plik <- read.table(sciezka, header=TRUE, quote="\"")
    plik <- plik[1:3]
    return(plik)
  }
  
  licz_zmienne <- function(plik) {
    return(substr(colnames(plik[3]), 2, 5))
  }
  
  cnf_data <<- czytaj_plik(sciezka)
  ile_zmiennych <<- licz_zmienne(cnf_data)
  print(cnf_data)
  print(ile_zmiennych)
  
  g <- rnorm(100000)
  h <- rep(NA, 100000)
  
  # Start the clock!
  ptm <- proc.time()
  h <- g + 1

  cnfGenAlg <<- rbga.bin(size = strtoi(ile_zmiennych), popSize = pop, iters = rep,
                         mutationChance = mut, elitism = T, evalFunc = fitness)
  print("Czas działania algorytmu genetycznego")
  print(proc.time() - ptm)
  plot(cnfGenAlg)
}
