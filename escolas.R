install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggpubr")
install.packages("reshape")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")

library(ggpubr)
library(ggplot2)
library(plyr)
library(reshape)
library(scales)
library(googleVis)
library(readr)
library(dplyr)
library(tidyr)



df1 <- read_csv("media_alunos_turma_municipios_2010 - Municipios SE, Sul e CO.csv", 
                skip = 9,
                col_names= c("Ano", "Regiao", "UF", "Cod_municipio", "Municipio", "Localizacao",
                             "Rede","Total_infantil","Creche","Total_fundamental", "Anos_iniciais",
                             "Anos_finais","1_ano","2_ano","3_ano","4_ano","5_ano","6_ano","7_ano",
                             "8_ano","9_ano","Turmas_unificadas","Total_medio","1_medio","2_medio",
                             "3_medio","4_medio","Medio_nao_seriado"),
                col_types = list(
                  Total_medio = col_double(),
                  Total_infantil = col_double(),
                  Total_fundamental = col_double()
                  
                ))


str(df1)

df2 <- read_csv("media_alunos_turma_municipios_2010 - Municipios NO e NE.csv", 
                skip = 9,
                col_names= c("Ano", "Regiao", "UF", "Cod_municipio", "Municipio", "Localizacao",
                             "Rede","Total_infantil","Creche","Total_fundamental", "Anos_iniciais",
                             "Anos_finais","1_ano","2_ano","3_ano","4_ano","5_ano","6_ano","7_ano",
                             "8_ano","9_ano","Turmas_unificadas","Total_medio","1_medio","2_medio",
                             "3_medio","4_medio","Medio_nao_seriado"),
                col_types = list(
                  Total_medio = col_double(),
                  Total_infantil = col_double(),
                  Total_fundamental = col_double()
                  
                ))

str(df1)


########join regions and organize data##########
dfMedias <- data.frame(rbind(df1, df2)) %>%
  select(Ano, Regiao, Rede, UF, Municipio, Total_medio,Total_infantil, Total_fundamental ) %>%
  filter( Rede == 'Estadual' | Rede == 'Municipal'| Rede == 'Privada') %>%
  replace(., is.na(.), 0.0) %>%
  gather(ensino, value, Total_medio:Total_infantil:Total_fundamental) %>%
  filter( value > 0) %>%
  group_by(Ano, Regiao, Rede, UF, Municipio, ensino) %>%
  summarise(value = mean(value))

##### Mean students per region #####
df_mean_all <- dfMedias %>%
  group_by(Regiao, ensino) %>%
  summarise(value=mean(value))

######## mean of students at private/state/municipal #############
df_total_alunos_rede <- dfMedias %>%
  group_by(Rede) %>%
  summarise(value=mean(value))

###########Mean  of students by state#######

df_mean_uf <- dfMedias %>%
  group_by(UF) %>%
  summarise(total=mean(value)) %>%
  mutate(UF_completo = paste("BR", UF, sep="-") )

##### CHARTS ############

chart_media_all <- ggplot(df_mean_all, aes(x = df_mean_all$Regiao, y = df_mean_all$value, fill = factor(df_mean_all$ensino))) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(fill = "Ensino") +
  xlab("Regiões do Brasil") + ylab("Média") + 
  ggtitle("Média de alunos por turma no ensino fundamental x médio x infantil no Brasil")

plot(chart_media_all)

chart_rede_total <- ggplot(df_total_alunos_rede, aes(x = df_total_alunos_rede$Rede, y = df_total_alunos_rede$value, fill = factor(df_total_alunos_rede$Rede))) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(fill = "Redes de ensino") +
  xlab("Rede") + ylab("Média") + 
  ggtitle("Média de alunos por rede de ensino no Brasil")

plot(chart_rede_total)



chart_geo_mean_alunos <- gvisGeoChart(df_mean_uf, locationvar = "UF_completo", colorvar = "total", hovervar = "UF",
                                       options=list(region="BR", 
                                                    displayMode="regions", 
                                                    resolution="provinces",
                                                    width=800, height=800,
                                                    title='TESTING MAPS',
                                                    colorAxis="{colors:['#ccebff', '#005c99']}")) 

plot(chart_geo_mean_alunos)




