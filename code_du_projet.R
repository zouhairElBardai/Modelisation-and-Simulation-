#les bibliotheques nécessaires
library(aod)
library(ggplot2)
#L’importation d’un fichier de données
mydata <- read.csv("etudiant_sup.csv",sep=",",dec=".",header=T)
#afficher les premières lignes
head(mydata)
# le résulat observé
plot(table(mydata$admit), main ="le résultat observé")
#afficher statistique descriptive
summary(mydata)

## pour calculer les écart types de chaque variables
sapply(mydata, sd)

#cette instruction pour afficher table de bidirectionnelle des résultats catégoriels avec leurs prédicteurs

xtabs(~admit + rank, data = mydata)
#pour convertir rank comme une variable catégorielle.
mydata$rank <- factor(mydata$rank)
#Régression logistique
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
#résultat 
summary(mylogit)


# CI utilisant le log-vraisemblance profilé
confint(mylogit)

## CI utilisant des erreurs standard
confint.default(mylogit)
#wald.test
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)
#wald.test avec la deffirence des rank
l <- cbind(0, 0, 0, 1, -1, 0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

## rapports de cotes uniquement
exp(coef(mylogit))

## rapports de cotes et IC à 95%
exp(cbind(OR = coef(mylogit), confint(mylogit)))

#pour calculer la probabilité prédite d'admission à chaque valeur de rang,
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = 
                                      mean(gpa), rank = factor(1:4)))
#afficher le bloc de données
newdata1
#un tableau de probabilités prédites.
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
#afficher le bloc de données
newdata1
#pour créer 100 valeurs de gre entre 200 et 800, à chaque valeur de rang
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, 
                                                  length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

#les valeurs prédites et les limites de confiance en probabilités
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

#afficher les premières lignes
head(newdata3)

#un graphique avec les probabilités prédites et des intervalles de confiance à 95%.
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),
                                                                                                                      size = 1)

#différence de déviance
with(mylogit, null.deviance - deviance)
#degrés de liberté
with(mylogit, df.null - df.residual)
# p-value
with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual,
                     lower.tail = FALSE))
#la vraisemblance du journal du modèle
logLik(mylogit)