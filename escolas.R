install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggpubr")
install.packages("reshape")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("raster")

library(ggpubr)
library(ggplot2)
library(plyr)
library(reshape)
library(scales)
library(googleVis)
library(readr)
library(dplyr)
library(tidyr)
library(raster)




df1 <- read_csv2("ATU_ESCOLAS_2016.csv", 
                skip = 9,
                col_names= c("Ano", "Regiao", "UF", "Cod_municipio", "Municipio", "Cod.escola", "Nome_escola", "Localizacao",
                             "Rede","Total_infantil","Creche","Pre_escola","Total_fundamental", "Anos_iniciais",
                             "Anos_finais","1_ano","2_ano","3_ano","4_ano","5_ano","6_ano","7_ano",
                             "8_ano","9_ano","Turmas_unificadas","Total_medio","1_medio","2_medio",
                             "3_medio","4_medio","Medio_nao_seriado"),
                col_types = list(
                  Total_medio = col_double(),
                  Total_infantil = col_double(),
                  Total_fundamental = col_double()
                  
                ))


str(df1)

df3 <- read_csv2("TX_REND_ESCOLAS_2016.csv", skip = 10,
                col_names = c("Ano", "Regiao", "UF", "Cod.Municipio", "Nome_municipio","Cod.Escola", "Nome_escola", "Loc", "Rede", 
                              "Total_aprovacao_fundamental","apv_fund_anos_iniciais","apv_fund_anos_finais","apv_fund_1", "apv_fund_2", "apv_fund_3", 
                              "apv_fund_4", "apv_fund_5", "apv_fund_6", "apv_fund_7", 
                              "apv_fund_8", "apv_fund_9","Total_aprovacao_medio", "apv_medio_1", "apv_medio_2", 
                              "apv_medio_3", "apv_medio_4", "apv_medio_nao_seriado","Total_reprovacao_fundamental",
                              "rep_fund_anos_iniciais","rep_fund_anos_finais", "rep_fund_1", "rep_fund_2", "rep_fund_3", 
                              "rep_fund_4", "rep_fund_5", "rep_fund_6", "rep_fund_7", 
                              "rep_fund_8", "rep_fund_9","Total_reprovacao_medio", "rep_medio_1", "rep_medio_2", 
                              "rep_medio_3", "rep_medio_4", "rep_medio_nao_seriado","Total_abandono_fundamental",
                              "abd_fund_anos_iniciais","abd_fund_anos_finais", "abd_fund_1", "abd_fund_2", "abd_fund_3", 
                              "abd_fund_4", "abd_fund_5", "abd_fund_6", "abd_fund_7", 
                              "abd_fund_8", "abd_fund_9", "Total_abandono_medio",
                              "abd_medio_1", "abd_medio_2", 
                              "abd_medio_3", "abd_medio_4", "abd_medio_nao_seriado"),
                col_types = list(
                 Total_aprovacao_fundamental = col_double(),
                  Total_aprovacao_medio = col_double(),
                  Total_reprovacao_fundamental = col_double(),
                  Total_reprovacao_medio = col_double(),
                  Total_abandono_fundamental = col_double(),
                  Total_abandono_medio = col_double()
                  
                ))


########join regions and organize data##########
dfMedias <- df1 %>%
  dplyr::select(Ano, Regiao, Rede, UF, Total_medio,Total_infantil, Total_fundamental ) %>%
  filter( Rede == 'Estadual' | Rede == 'Municipal'| Rede == 'Privada') %>%
  replace(., is.na(.), 0.0) %>%
  gather(ensino, value, Total_medio:Total_infantil:Total_fundamental) %>%
  filter( value > 0) %>%
  group_by(Ano, Regiao, Rede, UF, ensino) %>%
  summarise(value = mean(value))

dfRendimento <- df3 %>%
                dplyr::select(Ano, Rede, UF, Total_aprovacao_fundamental,Total_aprovacao_medio, 
                              Total_reprovacao_fundamental, Total_reprovacao_medio,
                Total_abandono_fundamental, Total_abandono_medio) %>%
                filter( Rede == 'Estadual' | Rede == 'Municipal'| Rede == 'Particular') %>%
                replace(., is.na(.), 0.0) %>%
                gather(ensino, perc, Total_aprovacao_fundamental:Total_aprovacao_medio:Total_reprovacao_fundamental:Total_reprovacao_medio:
                         Total_abandono_fundamental:Total_abandono_medio)%>%
                filter( perc > 0) %>%
                group_by(Ano, Rede, UF, ensino) %>%
                summarise(mean = mean(perc), median = median(perc)) %>%
                arrange(mean)



##### Mean students per region #####
df_mean_region <- dfMedias %>%
  group_by(Regiao) %>%
  summarise(value=mean(value))


##### Mean students per region #####
df_mean_all <- dfMedias %>%
  group_by(Regiao, ensino) %>%
  summarise(value=mean(value))

######## mean of students at private/state/municipal #############
df_total_alunos_rede <- dfMedias %>%
  group_by(Rede) %>%
  summarise(cv=cv(value), value=mean(value)) 


###########Mean  of students by state#######
df_mean_uf <- dfMedias %>%
  group_by(UF) %>%
  summarise(total=mean(value)) %>%
  mutate(UF_completo = paste("BR", UF, sep="-"), z_score = scale(total), status = if_else(z_score > 0, 1, 0)) %>%
  arrange(total)


###########Mean  of approval by state#######

df_mean_uf_fundamental <- dfMedias %>%
  filter( ensino == 'Total_fundamental' | ensino == 'Total_medio') %>%
  group_by(UF) %>%
  summarise(total=mean(value)) %>%
  mutate(UF_completo = paste("BR", UF, sep="-"), z_score = scale(total), status = if_else(z_score > 0, 1, 0)) %>%
  arrange(total)

df_approval_uf <- dfRendimento %>%
  filter( ensino == 'Total_aprovacao_fundamental' | ensino == 'Total_aprovacao_medio') %>%
  group_by(UF) %>%
  summarise(total_aprovacao=mean(mean)) %>%
  arrange(total_aprovacao)
  
df_corr_approval_mean  <- full_join(df_approval_uf, df_mean_uf_fundamental, by = "UF")

##### CHARTS ############
chart_media_regiao <- ggplot(df_mean_region, aes(x = df_mean_region$Regiao, y = df_mean_region$value, fill = factor(df_mean_region$Regiao))) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  scale_color_manual("Line", values = c('black'))+
  labs(fill = "Redes de ensino") +
  xlab("Região") + ylab("Média") + 
  ggtitle("Média de alunos por região do Brasil")

plot(chart_media_regiao)


chart_media_all <- ggplot(df_mean_all, aes(x = df_mean_all$Regiao, y = df_mean_all$value, fill = factor(df_mean_all$ensino))) + 
  geom_bar(stat = "identity", position=position_dodge()) + 
  labs(fill = "Ensino") +
  xlab("Regiões do Brasil") + ylab("Média") + 
  ggtitle("Média de alunos por turma no ensino fundamental x médio x infantil no Brasil")

plot(chart_media_all)

chart_rede_total <- ggplot(df_total_alunos_rede, aes(x = df_total_alunos_rede$Rede, y = df_total_alunos_rede$value, fill = factor(df_total_alunos_rede$Rede))) + 
    geom_bar(stat = "identity", position=position_dodge()) + 
    geom_point(data=df_total_alunos_rede, aes(x = df_total_alunos_rede$Rede, y = df_total_alunos_rede$cv/5)) +
    geom_line(data=df_total_alunos_rede, aes(x = df_total_alunos_rede$Rede, y = df_total_alunos_rede$cv/5, group = 1, colour='Coefficient of Variation'), size=1) +
    geom_text(aes(x = df_total_alunos_rede$Rede, y= df_total_alunos_rede$cv/5 + 1, label = round(df_total_alunos_rede$cv, digits  = 2))) +
    scale_color_manual("Line", values = c('black'))+
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


chart_uf_zscore = ggplot(df_mean_uf, aes(x = reorder(UF, z_score), y = z_score, label =  round(z_score, digits  = 2))) + 
  geom_point(stat='identity', aes(col=factor(status)), size=8) +
  scale_color_manual(name="Status", labels =  c("Abaixo da média", "Acima da média") ,values=c("#00ba38", "#f8766d")) + 
  geom_text(color="white", size=3) +
  xlab("Estados") + ylab("Z Score") + 
  ggtitle("Estados em relação a média de alunos por sala") + 
  coord_flip()

plot(chart_uf_zscore)


# Correlation % of approval and students by class
chart_corr_media_apr <- ggplot(df_corr_approval_mean, aes(x = total, y = total_aprovacao, label = factor(UF))) +
  geom_point(stat='identity', aes(col=UF), size=4)+
  geom_smooth(method=lm) + 
  ggtitle("Correlação entre % de aprovação e média de alunos por sala (Médio e fundamental)") + 
  xlab("Média de alunos por sala") + ylab("% de Aprovação")

cor(df_corr_approval_mean$total, df_corr_approval_mean$total_aprovacao)

plot(chart_corr_media_apr)

