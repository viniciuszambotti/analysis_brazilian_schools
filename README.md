# Brazilian schools analysis
[LinkedIn](https://www.linkedin.com/in/vinicius-zambotti-768160b2/)

 This analysis was made using public information that can be found here: http://dados.gov.br/dataset/media-de-alunos-por-turma-na-educacao-basica.

 I'm only considering  kindergarten, elementary school and high school in private, state and municipal school.


## Input files

I'm using two files to cover the whole Brazil

	 

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


# Data wrangling
	dfMedias <- data.frame(rbind(df1, df2)) %>%
	  select(Ano, Regiao, Rede, UF, Municipio, Total_medio,Total_infantil, Total_fundamental ) %>%
	  filter( Rede == 'Estadual' | Rede == 'Municipal'| Rede == 'Privada') %>%
	  replace(., is.na(.), 0.0) %>%
	  gather(ensino, value, Total_medio:Total_infantil:Total_fundamental) %>%
	  filter( value > 0) %>%
	  group_by(Ano, Regiao, Rede, UF, Municipio, ensino) %>%
	  summarise(value = mean(value))
	  
# Results

## Mean of students for each classroom by region in  kindergarten, elementary School and high school.

	df_mean_all <- dfMedias %>%
	  group_by(Regiao, ensino) %>%
	  summarise(value=mean(value))

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/bar1.png)
	
## Mean of students for each classroom by teaching network in  kindergarten, elementary School and high school.
	df_total_alunos_rede <- dfMedias %>%
  	group_by(Rede) %>%
 	summarise(value=mean(value))

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/bar2.png)


## Mean of students by state
	df_mean_uf <- dfMedias %>%
	  group_by(UF) %>%
	  summarise(total=mean(value)) %>%
	  mutate(UF_completo = paste("BR", UF, sep="-") )

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/map_mean.PNG)
