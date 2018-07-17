# estudo sobre os dados da população mundial
# fonte de dados: ONU
# Rodrigo Silva - 30/12/2017

# importando biblioteca
library(tidyverse)
library(ggplot2)
library(gridExtra)

# carregado o dataset
dataset <- read.csv("WPP2017_TotalPopulationBySex.csv", header=T)

# visualizando a estrutura
str(dataset)
head(dataset)

# gerando um subset com apenas os dados de: LOCATION, TIME, POPMALE, POPFEMALE, POPTOTAL
data <- subset(dataset, select = c("Location", "Time", "PopMale", "PopFemale", "PopTotal"))

# verificando o subset
str(data)

# criando um subset do Brasil
brasil <- subset(data, Location == "Brazil")
brasil <- droplevels(brasil)

# verificando o subset
str(brasil)
head(brasil)

# plotando os dados de população pelo tempo
plot(brasil)
plot(brasil$Time, brasil$PopTotal)#, type = "l", col = "red")

# criando um subset com dados da população mundial, por ano
mundo <- dataset %>%
  group_by(Time) %>%
  summarise(Male = sum(PopMale),
            Female = sum(PopFemale),
            Total = sum(PopTotal))

# verificando os dados
str(mundo)
head(mundo)

# plotando os dados do mundo
plot_total <- ggplot(mundo,aes(Time,Total)) +  geom_line(colour = "red")
plot_male <- ggplot(mundo,aes(Time,Male)) +  geom_line(colour = "blue")
plot_female <- ggplot(mundo,aes(Time,Female)) +  geom_line(colour = "yellow")

print(plot_total, plot_male, plot_female)

ggplot(mundo,aes(Time,Total)) +  geom_line(colour = "red") +
ggplot(mundo,aes(Time,Male)) +  geom_line(colour = "blue") +
ggplot(mundo,aes(Time,Female)) +  geom_line(colour = "yellow")


# o ideal aqui é remover todos os dados de 2017 para frente pois há duplicatas de informações com os 
# dados de projeção da população que a própria ONU fez
# seria ideal pegar os dados do Brasil de nascidos vivos e mortalidade e tentar encontrar a regressão
# que se encaixa com a projeção da população brasileira. Talvez esse seja um bom trabalho para o 
# congresso na argentina.

mundo_2017 <- subset(dataset, select = c("Location", "Time", "PopMale", "PopFemale", "PopTotal"), Time <= 2017)

# plotando a população de todos os paises

chart <- ggplot() + 
         geom_line(data = mundo_2017, 
                  aes(x = Time, y = PopTotal, color = Location)) +
         theme(legend.position = "none")

print(chart)

# verificando o plot apenas dos continentes

continentes <- dataset %>%
  select(Location, Time, VarID, PopMale, PopFemale, PopTotal) %>%
  filter(Time <= 2017) %>%
  group_by(Time,VarID) %>%
  summarise(Male = sum(PopMale),
            Female = sum(PopFemale),
            Total = sum(PopTotal))

head(continentes)
str(continentes)
tail(continentes)
  
chart <- ggplot() + 
  geom_line(data = continentes, 
            aes(x = Time, y = Total, group = VarID, color = VarID))

print(chart)


## Pra levar em conta um calculo real sobre o crescimento populacional do mundo é necessário antes enteder
## o que são essas variaveis "Variant" dentro do dataset. Contudo elas já mostram pontos importantes a 
## serem considerados: fertilidade, mortalidade e migração. Estes são 3 fatores que incidem diretamente
## em como a população está crescendo ou não.

## de certa forma é quase impraticavel que a população mundial esteja crescendo aceleradamente visto
## que a fertilidade tem diminuido, os indices de mortalidade estão se mantendo quase que estaveis  
## mesmo com a longeividade cada vez maior. A migração é um dos pontos mais interessantes, para que 
## o Canadá tem investido cada vez mais em imigrantes se teoricamente a população dele só cresce?

# brasil
brasil <- subset(dataset, Location == "Brazil")
brasil <- droplevels(brasil)

head(brasil)
tail(brasil)

chart_br <- ggplot() + 
  geom_line(data = brasil, 
            aes(x = Time, y = PopTotal, group = interaction(Variant, VarID), color = Variant))

# print(chart)

# canada
canada <- subset(dataset, Location == "Canada")
canada <- droplevels(canada)

head(canada)
tail(canada)

chart_ca <- ggplot() + 
  geom_line(data = canada, 
            aes(x = Time, y = PopTotal, group = interaction(Variant, VarID), color = Variant))

# print(chart)

# grid.arrange(chart_br, chart_ca, ncol=2)

# criando um subset com brasil e canada

br_ca <- subset(dataset, Location %in% c("Brazil", "Canada"))

chart_br_ca <- ggplot() + 
  geom_line(data = br_ca, 
            aes(x = Time, y = PopTotal, group = interaction(Variant, VarID), color = Variant)) +
  facet_grid( Location ~ .)

print(chart_br_ca)

# mundo

continentes <- subset(dataset, Location %in% c("South America","Oceania","Central America",
                                               "Europe","Asia","Africa"))

america <- subset(dataset, Location %in% c("Mexico","Canada","United States of America"))

america <- dataset %>%
  select(LocID, Location,VarID,Variant,Time,MidPeriod,PopMale,PopFemale,PopTotal) %>%
  group_by(VarID,Variant,Time,MidPeriod) %>%
  summarise(PopMale = sum(PopMale),
            PopFemale = sum(PopFemale),
            PopTotal = sum(PopTotal)) %>%
  mutate(LocID = 0) %>%
  mutate(Location = "North America")

america <- droplevels(america)

america <- america %>%
  select(LocID, Location,VarID,Variant,Time,MidPeriod,PopMale,PopFemale,PopTotal) 

america <- droplevels(america)

america$LocID <- as.integer(america$LocID)
america$Location <- as.factor(america$Location)

america <- as.data.frame(america)

str(america)
head(america)

continentes <- droplevels(continentes)
continentes <- rbind(continentes, america)

head(continentes)
str(continentes)

chart_mundo <- ggplot() +
  geom_line(data = continentes,
            aes(x = Time, y = PopTotal, group = interaction(Variant, VarID), color = Variant)) +
  facet_grid( Location ~ .)

print(chart_mundo)
