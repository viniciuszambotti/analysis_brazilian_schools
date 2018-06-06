install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggpubr")
install.packages("reshape")

library(ggpubr)
library(ggplot2)
library(plyr)
library(reshape)

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

# transform (--) to 0.0
dfMedias <- data.frame(lapply(dfMedias, function(x){gsub("--", 0.0, x)}))

##### Means Ensino Médio ########

# convert to numeric
dfMedias$Total_medio <- as.numeric(dfMedias$Total_medio)
# calculate mean by Regiao
df_medio_uf <- ddply(dfMedias, .(Regiao), summarize,  Media=mean(Total_medio))
# ignore values that don't have Regiao
df_medio_uf <- df_medio_uf[df_medio_uf$Regiao != '',]

##### Means Ensino Infantil ########
# convert to numeric
dfMedias$Total_infantil <- as.numeric(dfMedias$Total_infantil)
# calculate mean by Regiao
df_infantil_uf <- ddply(dfMedias, .(Regiao), summarize,  Media=mean(Total_infantil))
# ignore values that don't have Regiao
df_infantil_uf <- df_infantil_uf[df_infantil_uf$Regiao != '',]

##### Means Ensino Fundamental ########
# convert to numeric
dfMedias$Total_fundamental <- as.numeric(dfMedias$Total_fundamental)
# calculate mean by Regiao
df_fundamental_uf <- ddply(dfMedias, .(Regiao), summarize,  Media=mean(Total_fundamental))
# ignore values that don't have Regiao
df_fundamental_uf <- df_fundamental_uf[df_fundamental_uf$Regiao != '',]

##### Means all ########
# calculate mean by Regiao
df_mean_all <- ddply(dfMedias, .(Regiao), summarize,  fundamental=mean(Total_fundamental), 
                           medio=mean(Total_medio), infantil=mean(Total_infantil))

# ignore values that don't have Regiao
df_mean_all <- df_mean_all[df_mean_all$Regiao != '',]


df_mean_all <- melt(df_mean_all, id = c("Regiao"))




##### CHARTS ############

# Bar chart Ensino médio
chart_mean_medio <- ggplot(df_medio_uf, aes(x = df_medio_uf$Regiao, y = df_medio_uf$Media, fill = factor(df_medio_uf$Regiao))) + 
            geom_bar(stat = "identity") + 
            labs(fill = "Regiões") +
            xlab("Regiões do Brasil") + ylab("Média") + 
            ggtitle("Média de notas no ensino médio no Brasil")


# Bar chart Ensino infantil
chart_mean_infantil <- ggplot(df_infantil_uf, aes(x = df_infantil_uf$Regiao, y = df_infantil_uf$Media, fill = factor(df_infantil_uf$Regiao))) + 
          geom_bar(stat = "identity") + 
          labs(fill = "Regiões") +
          xlab("Regiões do Brasil") + ylab("Média") + 
          ggtitle("Média de notas no ensino infantil no Brasil")

# Bar chart Ensino infantil
chart_mean_fundamental <- ggplot(df_fundamental_uf, aes(x = df_fundamental_uf$Regiao, y = df_fundamental_uf$Media, fill = factor(df_fundamental_uf$Regiao))) + 
  geom_bar(stat = "identity") + 
  labs(fill = "Regiões") +
  xlab("Regiões do Brasil") + ylab("Média") + 
  ggtitle("Média de notas no ensino fundamental no Brasil")

# Bar chart ALL
chart_media_all <- ggplot(df_mean_all, aes(x = df_mean_all$Regiao, y = df_mean_all$value, fill = factor(df_mean_all$variable))) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(fill = "Ensino") +
  xlab("Regiões do Brasil") + ylab("Média") + 
  ggtitle("Média de notas no ensino fundamental x médio x infantil no Brasil")


ggarrange(chart_mean_medio, chart_mean_infantil, chart_mean_fundamental,
          ncol = 2, nrow = 2)


chart_media_all




