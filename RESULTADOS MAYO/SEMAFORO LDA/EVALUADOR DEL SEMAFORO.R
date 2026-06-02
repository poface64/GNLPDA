rm(list=ls())
#### Cargar las funciones necesarias ####
source("FUNCIONES PARA EL SEMAFORO.R")

# Hacer las cebollas de semaforo #

# Caso 1 (150 obs, 3 vars)
##cebollaC1 = cebollaND(n_samples = 150, n_dims = 3, n_classes = 3, noise = 0.1)
##write.csv(cebollaC1,"cebollcaC1.csv",row.names = F)
#plot(cebollaC1[,-4],pch = 16,col = cebollaC1$class)
# Caso 2 (150 obs, 10 vars)
##cebollaC2 = cebollaND(n_samples = 150, n_dims = 10, n_classes = 3, noise = 0.1)
##write.csv(cebollaC2,"cebollcaC2.csv",row.names = F)
# Caso 3 (15 obs, 3 vars)
##cebollaC3 = cebollaND(n_samples = 15, n_dims = 3, n_classes = 3, noise = 0.1)
##write.csv(cebollaC3,"cebollcaC3.csv",row.names = F)
# Caso 4 (15 obs, 10 vars)
##cebollaC4 = cebollaND(n_samples = 15, n_dims = 10, n_classes = 3, noise = 0.1)
##write.csv(cebollaC4,"cebollcaC4.csv",row.names = F)
# Caso 5 (15 obs, 15 vars)
##cebollaC5 = cebollaND(n_samples = 15, n_dims = 15, n_classes = 3, noise = 0.1)
##write.csv(cebollaC5,"cebollcaC5.csv",row.names = F)
# Caso 6 (15 obs, 17 vars)
##cebollaC6 = cebollaND(n_samples = 15, n_dims = 17, n_classes = 3, noise = 0.1)
##write.csv(cebollaC6,"cebollcaC6.csv",row.names = F)

#### Cargar los datos para el semaforo ####
cebolla1 = read.csv("cebollcaC1.csv")
cebolla2 = read.csv("cebollcaC2.csv")
cebolla3 = read.csv("cebollcaC3.csv")
cebolla4 = read.csv("cebollcaC4.csv")
cebolla5 = read.csv("cebollcaC5.csv")
cebolla6 = read.csv("cebollcaC6.csv")

#### Pruebas con el discriminante clasico ####

# Caso 1 (150 obs, 3 vars)
md1 = LDAM(cebollaC1[,-ncol(cebollaC1)],cebollaC1[,ncol(cebollaC1)])
md1$diagnostico
#plot(cebollaC1[,-4],pch = 16,col = cebollaC1$class)
# Caso 2 (150 obs, 10 vars)
md2 = LDAM(cebollaC2[,-ncol(cebollaC2)],cebollaC2[,ncol(cebollaC2)])
md2$diagnostico
# Caso 3 (15 obs, 3 vars)
md3 = LDAM(cebollaC3[,-ncol(cebollaC3)],cebollaC3[,ncol(cebollaC3)])
md3$diagnostico
# Caso 4 (15 obs, 10 vars)
md4 = LDAM(cebollaC4[,-ncol(cebollaC4)],cebollaC4[,ncol(cebollaC4)])
md4$diagnostico
# Caso 5 (15 obs, 15 vars)
md5 = LDAM(cebollaC5[,-ncol(cebollaC5)],cebollaC5[,ncol(cebollaC5)])
md5$diagnostico
# Caso 6 (15 obs, 17 vars)
md6 = LDAM(cebollaC6[,-ncol(cebollaC6)],cebollaC6[,ncol(cebollaC6)])
md6$diagnostico

#### Rehacer esto pero con la expansión ####

### Hacer las expansiones ###
cebollaC1E = data.frame(polexpj(cebollaC1[,-ncol(cebollaC1)]),
                        class = cebollaC1$class)
cebollaC2E = data.frame(polexpj(cebollaC2[,-ncol(cebollaC2)]),
                        class = cebollaC2$class)
cebollaC3E = data.frame(polexpj(cebollaC3[,-ncol(cebollaC3)]),
                        class = cebollaC3$class)
cebollaC4E = data.frame(polexpj(cebollaC4[,-ncol(cebollaC4)]),
                        class = cebollaC4$class)
cebollaC5E = data.frame(polexpj(cebollaC5[,-ncol(cebollaC5)]),
                        class = cebollaC5$class)
cebollaC6E = data.frame(polexpj(cebollaC6[,-ncol(cebollaC6)]),
                        class = cebollaC6$class)

# Caso 1 (150 obs, 3 vars)
md1E = LDAM(cebollaC1E[,-ncol(cebollaC1E)],cebollaC1E[,ncol(cebollaC1E)])
md1E$diagnostico
plot(Re(md1E$proyecciones),col = cebollaC1E$class,
     pch = 16,
     main = "LDA Clasic 1. Obs. 150, 9 vars.")

# Caso 2 (150 obs, 10 vars)
md2E = LDAM(cebollaC2E[,-ncol(cebollaC2E)],cebollaC2E[,ncol(cebollaC2E)])
md2E$diagnostico
plot(Re(md2E$proyecciones),col = cebollaC2E$class,
     pch = 16,
     main = "LDA Clasic 2. Obs. 150, 65 vars.")

# Caso 3 (15 obs, 3 vars)
md3E = LDAM(cebollaC3E[,-ncol(cebollaC3E)],cebollaC3E[,ncol(cebollaC3E)])
md3E$diagnostico
plot(Re(md3E$proyecciones),col = cebollaC3E$class,
     pch = 16,
     main = "LDA Clasic 3. Obs. 15, 9 vars.")

# Caso 4 (15 obs, 10 vars)
md4E = LDAM(cebollaC4E[,-ncol(cebollaC4E)],cebollaC4E[,ncol(cebollaC4E)])
md4E$diagnostico

# Caso 5 (15 obs, 15 vars)
md5E = LDAM(cebollaC5E[,-ncol(cebollaC5E)],cebollaC5E[,ncol(cebollaC5E)])
md5E$diagnostico

# Caso 6 (15 obs, 17 vars)
md6E = LDAM(cebollaC6E[,-ncol(cebollaC6E)],cebollaC6E[,ncol(cebollaC6E)])
md6E$diagnostico



DiagnosticoC = rbind.data.frame(
                 md1E$diagnostico,
                 md2E$diagnostico,
                 md3E$diagnostico,
                 md4E$diagnostico,
                 md5E$diagnostico,
                 md6E$diagnostico)
write.csv(DiagnosticoC,"DiagnosticoC.csv",row.names = F)


#### Revisar los casos con MASS LDA ####
library(MASS)
# Caso 1 (150 obs, 3 vars)
mmd1E = lda(cebollaC1E[,-ncol(cebollaC1E)],cebollaC1E[,ncol(cebollaC1E)])
m1 = predict(mmd1E)
plot(Re(m1$x),col = cebollaC1E$class,
     pch = 16,
     main = "LDA improved 1. Obs. 150, 9 vars.")

# Caso 2 (150 obs, 10 vars)
mmd2E = lda(cebollaC2E[,-ncol(cebollaC2E)],cebollaC2E[,ncol(cebollaC2E)])
m2 = predict(mmd2E)
plot(Re(m2$x),col = cebollaC2E$class,
     pch = 16,
     main = "LDA improved 2. Obs. 150, 65 vars.")

# Caso 3 (15 obs, 3 vars)
mmd3E = lda(cebollaC3E[,-ncol(cebollaC3E)],cebollaC3E[,ncol(cebollaC3E)])
m3 = predict(mmd3E)
plot(Re(m3$x),col = cebollaC3E$class,
     pch = 16,
     main = "LDA improved 3. Obs. 15, 9 vars.")

# Caso 4 (15 obs, 10 vars)
mmd4E = lda(cebollaC4E[,-ncol(cebollaC4E)],cebollaC4E[,ncol(cebollaC4E)])
m4 = predict(mmd4E)
plot(Re(m4$x),col = cebollaC4E$class,
     pch = 16,
     main = "LDA improved 4. Obs. 15, 65 vars.")

# Caso 5 (15 obs, 15 vars)
mmd5E = lda(cebollaC5E[,-ncol(cebollaC5E)],cebollaC5E[,ncol(cebollaC5E)])
m5 = predict(mmd5E)
plot(Re(m5$x),col = cebollaC5E$class,
     pch = 16,
     main = "LDA improved 5. Obs. 15, 135 vars.")

# Caso 6 (15 obs, 17 vars)
mmd6E = lda(cebollaC6E[,-ncol(cebollaC6E)],cebollaC6E[,ncol(cebollaC6E)])
m6 = predict(mmd6E)
plot(Re(m6$x),col = cebollaC6E$class,
     pch = 16,
     main = "LDA improved 6. Obs. 15, 170 vars.")

#############

# Cargar la base de datos #
library(kfda)

#### Probar rapido los 6 modelos ####

mk1 = kfda(cebollaC1E)
plot(mk1$LDs,col = cebollaC1E$class ,pch = 16,
     main = "KFDA 1")
mk2 = kfda(cebollaC2E)
plot(mk2$LDs,col = cebollaC2E$class ,pch = 16,
     main = "KFDA 2")
mk3 = kfda(cebollaC3E)
plot(mk3$LDs,col = cebollaC3E$class ,pch = 16,
     main = "KFDA 3")
mk4 = kfda(cebollaC4E)
plot(mk4$LDs,col = cebollaC4E$class ,pch = 16,
     main = "KFDA 4")
mk5 = kfda(cebollaC5E)
plot(mk5$LDs,col = cebollaC5E$class ,pch = 16,
     main = "KFDA 5")
mk6 = kfda(cebollaC6E)
plot(mk6$LDs,col = cebollaC6E$class ,pch = 16,
     main = "KFDA 6")





