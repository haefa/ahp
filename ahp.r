#Perbandingan Kriteria
#Pairwise matrix:
#                approval  sumberDaya  stakeholder  dukunganPublik
# approval           1          4            1             6
# sumberDaya        1/4         1           1/3            4
# stakeholder        1          3            1             5
# dukunganPublik    1/6        1/4          1/5            1
<<<<<<< HEAD

pairwiseKriteria = rep(1, 16)
pairwiseSubApproval = rep(1, 9)
pairwiseSubSD = rep(1, 9)
pairwiseAltAdministrasi = rep(1, 9)
pairwiseAltHukum = rep(1, 9)
pairwiseAltPolitik = rep(1, 9)
pairwiseAltFinansial = rep(1, 9)
pairwiseAltAhli = rep(1, 9)
pairwiseAltTeknologi = rep(1, 9)
pairwiseAltStakeholder = rep(1, 9)
pairwiseAltDukungan = rep(1, 9)

pairwise = c(1,1/4,4,1/6,4,1,4,1/4,1/4,1/4,1,1/5,6,4,5,1)

nilaiEigen = function(pairwiseKriteria){
  jumlahEigen = sqrt(length(pairwiseKriteria))
  matKriteria = matrix(data = pairwiseKriteria, ncol = jumlahEigen, byrow = T)
  eigSebelum = numeric(jumlahEigen)
  selisihEig = rep(1, jumlahEigen)
  matOlah = matKriteria
  while (all(selisihEig > 0.0001)) {
    eigSekarang = c()
    matOlah = matOlah %*% matOlah
    for (j in 1:dim(matOlah)[1]) {
      eigSekarang[j] = sum(matOlah[j,])/sum(matOlah)
    }
    selisihEig = abs(eigSekarang - eigSebelum)
    eigSebelum = eigSekarang
    # print(eigSekarang)
=======
pairwiseKriteria = c(1,1,1,1,1,1,1,1,1)
jumlahEigen = sqrt(length(pairwiseKriteria))
matKriteria = matrix(data = pairwiseKriteria, ncol = jumlahEigen, byrow = T)
eigSebelum = numeric(jumlahEigen)
selisihEig = rep(1, jumlahEigen)
matOlah = matKriteria
while (all(selisihEig > 0.0001)) {
  eigSekarang = c()
  matOlah = matOlah %*% matOlah
  for (j in 1:dim(matOlah)[1]) {
    eigSekarang[j] = sum(matOlah[j,])/sum(matOlah)
>>>>>>> d52d939f0ba3e9bfa4f68218bbcaaebdba331fb4
  }
  return(eigSekarang)
}

konsistensi = function(pairwiseVector, eigen){
  jumlahEigen = sqrt(length(pairwiseVector))
  matriksPairwise = matrix(data = pairwiseVector, ncol = jumlahEigen, byrow = T)
  weightedSumVector = matriksPairwise %*% eigen
  consistencyVector = weightedSumVector/eigen
  rataan = mean(consistencyVector)
  n = length(eigen)
  randomIndex = ri(n)
  ci = (rataan - n) / (n - 1)
  return(ci/randomIndex)
}

ri = function(n){
  nilai = c(0.00, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41)
  for (i in 1:length(nilai)) {
    if (n == i+1) return(nilai[i])
  }
}

