install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggpubr")
install.packages("reshape")

library(ggpubr)
library(ggplot2)
library(plyr)
library(reshape)
library(scales)
library(googleVis)

#import dataFrame
df1 <- data.frame(read.csv(file = 'media_alunos_turma_municipios_2010 - Municípios SE, Sul e CO.csv',skip = 9, encoding = "UTF-8", sep = ','))
df2 <- data.frame(read.csv(file = 'media_alunos_turma_municipios_2010 - Municípios NO e NE.csv',skip = 9, encoding = "UTF-8", sep = ','))

#rename columns
colnames(df1) <- c("Ano", "Regiao", "UF", "Cod_municipio", "Municipio", "Localizacao","Rede","Total_infantil","Creche","Total_fundamental", "Anos_iniciais","Anos_finais","1_ano",
                  "2_ano","3_ano","4_ano","5_ano","6_ano","7_ano","8_ano","9_ano","Turmas_unificadas","Total_medio","1_medio","2_medio","3_medio","4_medio","Medio_nao_seriado")

colnames(df2) <- c("Ano", "Regiao", "UF", "Cod_municipio", "Municipio", "Localizacao","Rede","Total_infantil","Creche","Total_fundamental", "Anos_iniciais","Anos_finais","1_ano",
                  "2_ano","3_ano","4_ano","5_ano","6_ano","7_ano","8_ano","9_ano","Turmas_unificadas","Total_medio","1_medio","2_medio","3_medio","4_medio","Medio_nao_seriado")

# join regions
dfMedias <- data.frame(rbind(df1, df2))

dfMedias <- subset(dfMedias, Rede == 'Estadual' | Rede == 'Municipal'| Rede == 'Privada')

# transform (--) to 0.0
dfMedias <- data.frame(lapply(dfMedias, function(x){gsub("--", 0.0, x)}))

# convert to numeric
dfMedias$Total_medio <- as.numeric(dfMedias$Total_medio)
dfMedias$Total_infantil <- as.numeric(dfMedias$Total_infantil)
dfMedias$Total_fundamental <- as.numeric(dfMedias$Total_fundamental)


##### total of students per region #####
# calculate mean by Regiao
df_sum_reg <- ddply(dfMedias, .(Regiao), summarize,  fundamental=sum(Total_fundamental), 
                    medio=sum(Total_medio), infantil=sum(Total_infantil))

# ignore values that don't have Regiao
df_sum_reg <- df_sum_reg[df_sum_reg$Regiao != '',]


df_sum_reg <- melt(df_sum_reg, id = c("Regiao"))


##### Mean by region ########
# calculate mean by Regiao
df_mean_all <- ddply(dfMedias, .(Regiao), summarize,  fundamental=mean(Total_fundamental), 
                           medio=mean(Total_medio), infantil=mean(Total_infantil))

# ignore values that don't have Regiao
df_mean_all <- df_mean_all[df_mean_all$Regiao != '',]


df_mean_all <- melt(df_mean_all, id = c("Regiao"))




######## mean of students at private/state/municipal #############
df_total_alunos_rede <- ddply(dfMedias, .(Rede), summarize,  total=sum(Total_fundamental + Total_medio + Total_infantil))


###### Mean dropping students #########
df_numero_alunos <- ddply(dfMedias, .(Ano), summarize,  fundamental=sum(Total_fundamental), 
                          medio=sum(Total_medio), infantil=sum(Total_infantil))

df_numero_alunos <- df_numero_alunos[(df_numero_alunos$Ano == '2010'),]
df_numero_alunos$desistem_ensino_infantil <- (((df_numero_alunos$fundamental/df_numero_alunos$infantil) *100) - 100) * -1
df_numero_alunos$desistem_ensino_fundamental <- (((df_numero_alunos$medio/df_numero_alunos$fundamental) *100) - 100) * -1
df_numero_alunos$chegam_ensino_medio <- ((df_numero_alunos$desistem_ensino_infantil + df_numero_alunos$desistem_ensino_fundamental) - 100) * -1
df_numero_alunos <- subset(df_numero_alunos, select = c('Ano', 'desistem_ensino_infantil', 'desistem_ensino_fundamental', 'chegam_ensino_medio'))
df_numero_alunos <- melt(df_numero_alunos, id = c("Ano"))

###### Total of students by state
df_sum_uf <- ddply(dfMedias, .(UF), summarize,  total=sum(Total_fundamental + Total_medio + Total_infantil))

# ignore values that don't have Regiao
df_sum_uf <- df_sum_uf[df_sum_uf$UF != '',]

#format UF
df_sum_uf$UF_completo <- mapply(function(x) gsub(x, paste("BR-",x, sep=""), x),
                                df_sum_uf$UF)

###### Mean  of students by state
df_mean_uf <- ddply(dfMedias, .(UF), summarize,  total=mean(Total_fundamental),
                    total2 = mean(Total_medio),
                    total3 = mean(Total_infantil))

df_mean_uf$total <- (df_mean_uf$total + df_mean_uf$total2 + df_mean_uf$total3) / 3

# ignore values that don't have Regiao
df_mean_uf <- df_mean_uf[df_sum_uf$UF != '',]

# Format UF
df_mean_uf$UF_completo <- mapply(function(x) gsub(x, paste("BR-",x, sep=""), x),
                                 df_mean_uf$UF)
##### CHARTS ############

chart_total_regiao_ensino <- ggplot(df_sum_reg, aes(x = df_sum_reg$Regiao, y = df_sum_reg$value/1000, fill = factor(df_sum_reg$variable))) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(fill = "Ensino") +
  xlab("Regiões do Brasil") + ylab("Total (Mil)") + 
  ggtitle("Total de alunos por ensino nas regiões do Brasil")


chart_media_all <- ggplot(df_mean_all, aes(x = df_mean_all$Regiao, y = df_mean_all$value, fill = factor(df_mean_all$variable))) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(fill = "Ensino") +
  xlab("Regiões do Brasil") + ylab("Média") + 
  ggtitle("Média de alunos por turma no ensino fundamental x médio x infantil no Brasil")



chart_rede_total <- ggplot(df_total_alunos_rede, aes(x = df_total_alunos_rede$Rede, y = df_total_alunos_rede$total/1000000, fill = factor(df_total_alunos_rede$Rede))) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(fill = "Redes de ensino") +
  xlab("Rede") + ylab("Total (M)") + 
  ggtitle("Total de alunos por rede de ensino no Brasil")


ggarrange(chart_total_regiao_ensino, chart_media_all, chart_rede_total,  ncol = 2, nrow = 2)

chart_desistencia <- ggplot(df_numero_alunos, aes(x="", y=df_numero_alunos$value, fill=df_numero_alunos$variable))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+
  labs(fill = "Período escolar")+
  ggtitle("Porcentagem de alunos que chegam ao ensino médio no Brasil")+
  ylab("% de alunos")


chart_geo_total_alunos <- gvisGeoChart(df_sum_uf, locationvar = "UF_completo", colorvar = "total", hovervar = "UF",
                          options=list(region="BR", 
                                       displayMode="regions", 
                                       resolution="provinces",
                                       width=800, height=800,
                                       title='TESTING MAPS',
                                       colorAxis="{colors:['#ccebff', '#005c99']}")) 

plot(chart_geo_total_alunos)

chart_geo_mean_alunos <- gvisGeoChart(df_mean_uf, locationvar = "UF_completo", colorvar = "total", hovervar = "UF",
                                       options=list(region="BR", 
                                                    displayMode="regions", 
                                                    resolution="provinces",
                                                    width=800, height=800,
                                                    title='TESTING MAPS',
                                                    colorAxis="{colors:['#ccebff', '#005c99']}")) 

plot(chart_geo_mean_alunos)


