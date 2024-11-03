install.packages("readr")
install.packages("skimr")
install.packages("kableExtra")
install.packages("ggplot2")
install.packages("dplyr")

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("SummarizedExperiment")
BiocManager::install("S4Vectors")

library(readr)
library(skimr)
library(SummarizedExperiment)
library(S4Vectors)
library(kableExtra)
library(ggplot2)
library(dplyr)


url <- "https://raw.githubusercontent.com/nutrimetabolomics/metaboData/refs/heads/main/Datasets/2024-Cachexia/human_cachexia.csv"
datos <- read.csv(url)
head(datos)
View(datos)

rownames(datos) <- datos[[1]]
data_matrix <- as.matrix(datos[, -1])  

row_metadata <- DataFrame(
  GeneID = rownames(data_matrix),  
  Description = "Análisis de la orina del paciente"  
)

col_metadata <- DataFrame(
  SampleID = colnames(data_matrix),
  Condition = rep("Control/Cachexia", length(colnames(data_matrix)))  
)

se <- SummarizedExperiment(
  assays = list(counts = data_matrix),
  rowData = row_metadata,
  colData = col_metadata
)

se

# DATOS estadísticos de la base de caquexia
str(datos)
datos$Muscle.loss <- as.factor(datos$Muscle.loss) #Enfermo o no

summary(datos[, -c(1, 2)])  

# Gráficos comparativos

#creatina

ggplot(datos, aes(x = Muscle.loss, y = Creatine, fill = Muscle.loss)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, 
               notch = TRUE) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(title = "Distribución de Creatina según Estado de Pérdida de Músculo",
       x = "Pérdida de Músculo", 
       y = "Niveles de Creatina") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    legend.position = "none"
  )

t_test_result <- t.test(datos$Creatine ~ datos$Muscle.loss)
print(t_test_result)

#Creatinina

ggplot(datos, aes(x = Muscle.loss, y = Creatinine, fill = Muscle.loss)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, 
               notch = TRUE) + 
  scale_fill_brewer(palette = "Pastel1") + 
  labs(title = "Distribución de Creatinina según Estado de Pérdida de Músculo",
       x = "Pérdida de Músculo", 
       y = "Niveles de Creatinina") +
  theme_minimal() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14), 
    axis.title.y = element_text(size = 14), 
    axis.text = element_text(size = 12), 
    legend.position = "none"
  )

t_test_result <- t.test(datos$Creatinine ~ datos$Muscle.loss)
print(t_test_result)

#X3.Hydroxybutyrate

ggplot(datos, aes(x = Muscle.loss, y = X3.Hydroxybutyrate, fill = Muscle.loss)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, 
               notch = TRUE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de 3-Hidroxibutirato según Estado",
       x = "Pérdida de Músculo", y = "3-Hidroxibutirato") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )


t_test_result <- t.test(datos$X3.Hydroxybutyrate ~ datos$Muscle.loss)
print(t_test_result)

#acetona

ggplot(datos, aes(x = Muscle.loss, y = Acetone, fill = Muscle.loss)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, 
               notch = TRUE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de Acetona según Estado",
       x = "Pérdida de Músculo", y = "Acetona") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )


t_test_result <- t.test(datos$Acetone ~ datos$Muscle.loss)
print(t_test_result)


#Carnitina

ggplot(datos, aes(x = Muscle.loss, y = Carnitine, fill = Muscle.loss)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, 
               notch = TRUE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de Carnitina según Estado",
       x = "Pérdida de Músculo", y = "Carnitina") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )


t_test_result <- t.test(datos$Carnitine ~ datos$Muscle.loss)
print(t_test_result)

#citrato

ggplot(datos, aes(x = Muscle.loss, y = Citrate, fill = Muscle.loss)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, 
               notch = TRUE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de Citrato según Estado",
       x = "Pérdida de Músculo", y = "Citrato") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )


t_test_result <- t.test(datos$Citrate ~ datos$Muscle.loss)
print(t_test_result)

#Lactato

ggplot(datos, aes(x = Muscle.loss, y = Lactate, fill = Muscle.loss)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2, 
               notch = TRUE) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Distribución de Lactato según Estado",
       x = "Pérdida de Músculo", y = "Lactato") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "none"
  )


t_test_result <- t.test(datos$Lactate ~ datos$Muscle.loss)
print(t_test_result)
#####

# Acotación base de datos
install.packages("dplyr")
library(dplyr)

datos_estudio <- datos %>%
  select(Muscle.loss, 9, 14, 19, 20, 21, 22, 37)

head(datos_estudio)
View(datos_estudio)

f<- function(x){
  ifelse (is.numeric(x), 
          hist(x, breaks=5),
          barplot(table(x))
  )
}
par(mfrow=c(3,3))
apply(datos_estudio,2,f)

######

datosnum <- datos_estudio[sapply(datos_estudio, is.numeric)]
datosnum <- scale(datosnum, center = TRUE, scale=FALSE)
apply(datosnum,2, mean)

n<- dim(datos_estudio)[1]
S<-cov(datosnum)*(n-1)/n
show(S)

R<-cor(datosnum)
show(R)

EIG <- eigen(S)
show(EIG)

eigenVecs1 <- EIG$vectors
PCAS1 <- datosnum %*% eigenVecs1
head(PCAS1)

par(mfrow=c(1,1))

bgSurv<- colSurv <- ifelse(datos$Muscle.loss=="cachexic", "red", "blue")
pchSurv <- ifelse(datos$Muscle.loss=="cachexic",1,19)
plot(PCAS1[,1], PCAS1[,2], main = "Cachexia. 2 primeras PCs",
     xlab=xlabel, ylab=ylabel, 
     pch=pchSurv, col=colSurv, bg=bgSurv)
legend( "bottomright"  , inset = c(0.01,0.01), cex =1, 
        bty = "y", legend = c("cachexic", "control"), 
        text.col = c("red", "blue"),
        col = c("red", "blue"), 
        pt.bg = c("red","blue"), 
        pch = c(1,19)
)

########################

datos$Muscle.loss <- as.factor(datos$Muscle.loss)

library(dplyr)

t_tests <- datos %>% 
  summarise(across(where(is.numeric), 
                   ~t.test(. ~ Muscle.loss)$p.value))
print(t_tests)

#Las variables con p-valores bajos indican diferencias significativas entre los grupos, lo cual podría señalar importancia en la cachexia.

#X1.6.Anhydro.beta.D.glucose - 0.0353
#X2.Aminobutyrate - 0.00786
#X2.Hydroxyisobutyrate - 0.00489

datos_estandarizados <- as.data.frame(scale(datos[ , -which(names(datos) %in% c("Patient.ID", "Muscle.loss"))]))
datos_estandarizados$Muscle.loss <- datos$Muscle.loss
modelo_logistico <- glm(Muscle.loss ~ ., data = datos_estandarizados, family = "binomial")
summary(modelo_logistico)

#Las variables con valores de importancia más altos en la salida o en el gráfico varImpPlot son las que probablemente tengan mayor influencia en la cachexia.

####################

PCAS2 <- princomp(datosnum)
names(PCAS2)

PCAS3 <- prcomp(datosnum)
names(PCAS3)

PCAS2$sdev
PCAS3$sdev

sqrt(EIG$values)

PCAS2$loadings

PCAS3$rotation[,3]

EIG$vectors

head(PCAS2$scores)

