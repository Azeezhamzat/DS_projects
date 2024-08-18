library(ggrain)
library(ggpubr)
library(ggsci)

#Sepal Length
ggplot(iris, aes(Species, Sepal.Length, fill=Species)) +
  geom_rain(color="green") +
  theme_pubr() +
  ggtitle("Rain Cloud Plot")


ggplot(iris, aes(Species, Sepal.Length, fill=Species)) +
  geom_rain(color="green") +
  theme_pubr() +
  ggtitle("Rain Cloud Plot") +
  scale_fill_aaas()

ggplot(iris, aes(Species, Sepal.Length, fill=Species)) +
  geom_rain(color="green") +
  theme_pubr() +
  ggtitle("Rain Cloud Plot")+
  scale_fill_bmj()


# Petal Length
ggplot(iris, aes(Species, Petal.Length, fill=Species)) +
  geom_rain(color="green") +
  theme_pubr() +
  ggtitle("Rain Cloud Plot")

ggplot(iris, aes(Species, Petal.Length, fill=Species)) +
  geom_rain(color="green") +
  theme_pubr() +
  ggtitle("Rain Cloud Plot") +
  scale_fill_cosmic()
