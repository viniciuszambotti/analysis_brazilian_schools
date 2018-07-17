# Brazilian schools analysis
[LinkedIn](https://www.linkedin.com/in/vinicius-zambotti-768160b2/)

Os dados dessa análise são de origem pública, e podem ser encontrados aqui:http://portal.inep.gov.br/indicadores-educacionais.

Eu estou apenas considerando os ensinos infantil, fundamental e médio, em escolas estaduais, municipais e privadas no ano de 2016


## Dados de entrada

Um dos arquivos é em relação a média de alunos por sala de aula e o outro é a taxa de aprovação dos alunos

	 

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
		
	df3 <- read_csv2("TX_REND_ESCOLAS_2016.csv", skip = 10,
                col_names = c("Ano", "Regiao", "UF", "Cod.Municipio", "Nome_municipio","Cod.Escola", "Nome_escola", "Loc", "Rede", 
                              "Total_aprovacao_fundamental","apv_fund_anos_iniciais","apv_fund_anos_finais",
			      "apv_fund_1", "apv_fund_2", "apv_fund_3", 
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


# Transformação dos dados
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
		gather(ensino, perc,Total_aprovacao_fundamental:Total_aprovacao_medio:
		Total_reprovacao_fundamental:Total_reprovacao_medio:
			 Total_abandono_fundamental:Total_abandono_medio)%>%
		filter( perc > 0) %>%
		group_by(Ano, Rede, UF, ensino) %>%
		summarise(mean = mean(perc), median = median(perc)) %>%
		arrange(mean)

	  
# Resultados

## Média de alunos em sala de aula por regiões do Brasil

	df_mean_all <- dfMedias %>%
	  group_by(Regiao, ensino) %>%
	  summarise(value=mean(value))

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/bar1.png)
	
## Média de alunos em sala de aula por redes de ensino
	df_total_alunos_rede <- dfMedias %>%
  	group_by(Rede) %>%
 	summarise(value=mean(value))

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/bar2.png)


## Média de alunos em sala de aula por estados
	df_mean_uf <- dfMedias %>%
	  group_by(UF) %>%
	  summarise(total=mean(value)) %>%
	  mutate(UF_completo = paste("BR", UF, sep="-") )

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/map_mean.PNG)

## Relação dos estados brasileiros com a média de alunos por sala de aula
	df_mean_uf <- dfMedias %>%
	  group_by(UF) %>%
	  summarise(total=mean(value)) %>%
	  mutate(UF_completo = paste("BR", UF, sep="-"), z_score = scale(total), status = if_else(z_score > 0, 1, 0)) %>%
	  arrange(total)

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/point1.png)

## Correlação entre % de alunos aprovados e média de alunos por sala de aula (-0,22 - fraca)

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

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/point2.png)
