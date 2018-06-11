# Brazilian schools analysis
[LinkedIn](https://www.linkedin.com/in/vinicius-zambotti-768160b2/)

 This analysis was made using public information that can be found here: http://dados.gov.br/dataset/media-de-alunos-por-turma-na-educacao-basica.

 I'm only considering  kindergarten, elementary school and high school in private, state and municipal school.


## Input files

I'm using two files to cover the whole Brazil

	 

    df1 <- data.frame(read.csv(file = 'media_alunos_turma_municipios_2010 - Municípios SE, Sul e CO.csv',skip = 9, encoding = "UTF-8", sep = ','))
    df2 <- data.frame(read.csv(file = 'media_alunos_turma_municipios_2010 - Municípios NO e NE.csv',skip = 9, encoding = "UTF-8", sep = ','))


# Results

## Total of students by region in  kindergarten, elementary School and high school.

	df_sum_reg <- ddply(dfMedias, .(Regiao), summarize,  fundamental=sum(Total_fundamental), 
                    medio=sum(Total_medio), infantil=sum(Total_infantil))
	df_sum_reg <- df_sum_reg[df_sum_reg$Regiao != '',]
	df_sum_reg <- melt(df_sum_reg, id = c("Regiao"))
	
	
![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/bar1.png)
   


## Mean of students by region in  kindergarten, elementary School and high school.

	df_mean_all <- ddply(dfMedias, .(Regiao), summarize,  fundamental=mean(Total_fundamental), 
                           medio=mean(Total_medio), infantil=mean(Total_infantil))

	# ignore values that don't have Regiao
	df_mean_all <- df_mean_all[df_mean_all$Regiao != '',]

	df_mean_all <- melt(df_mean_all, id = c("Regiao"))

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/bar2.png)
	
## Total of students by teaching network in  kindergarten, elementary School and high school.
	df_total_alunos_rede <- ddply(dfMedias, .(Rede), summarize,  total=sum(Total_fundamental + Total_medio + Total_infantil))

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/bar3.png)

## Percentage of students that reaches high school
	df_numero_alunos <- ddply(dfMedias, .(Ano), summarize,  fundamental=sum(Total_fundamental), 
	                          medio=sum(Total_medio), infantil=sum(Total_infantil))

	df_numero_alunos <- df_numero_alunos[(df_numero_alunos$Ano == '2010'),]
	df_numero_alunos$desistem_ensino_infantil <- (((df_numero_alunos$fundamental/df_numero_alunos$infantil) *100) - 100) * -1
	df_numero_alunos$desistem_ensino_fundamental <- (((df_numero_alunos$medio/df_numero_alunos$fundamental) *100) - 100) * -1
	df_numero_alunos$chegam_ensino_medio <- ((df_numero_alunos$desistem_ensino_infantil + df_numero_alunos$desistem_ensino_fundamental) - 100) * -1
	df_numero_alunos <- subset(df_numero_alunos, select = c('Ano', 'desistem_ensino_infantil', 'desistem_ensino_fundamental', 'chegam_ensino_medio'))
	df_numero_alunos <- melt(df_numero_alunos, id = c("Ano"))

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/dropoff.png)
## Total of students by state
	df_sum_uf <- ddply(dfMedias, .(UF), summarize,  total=sum(Total_fundamental + Total_medio + Total_infantil))

	# ignore values that don't have Regiao
	df_sum_uf <- df_sum_uf[df_sum_uf$UF != '',]

	#format UF
	df_sum_uf$UF_completo <- mapply(function(x) gsub(x, paste("BR-",x, sep=""), x),
	                                df_sum_uf$UF)

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/map_total.PNG)

## Mean of students by state
	df_mean_uf <- ddply(dfMedias, .(UF), summarize,  total=mean(Total_fundamental),
	                    total2 = mean(Total_medio),
	                    total3 = mean(Total_infantil))

	df_mean_uf$total <- (df_mean_uf$total + df_mean_uf$total2 + df_mean_uf$total3) / 3

	# ignore values that don't have Regiao
	df_mean_uf <- df_mean_uf[df_sum_uf$UF != '',]

	# Format UF
	df_mean_uf$UF_completo <- mapply(function(x) gsub(x, paste("BR-",x, sep=""), x),
	                                 df_mean_uf$UF)

![enter image description here](https://raw.githubusercontent.com/viniciuszambotti/analysis_brazillian_schools/master/images/map_mean.PNG)
