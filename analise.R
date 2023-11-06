######## Libs
library(readr)
library(tidyverse)
library(lubridate)

######## Funções

leitura <- function(){

  #leitura e combinação de todos os dados
  total_dados = data.frame()
  for (i in 2006:2019){
    lido = read_csv(paste0(paste0("dados_ironman_",i),".csv"))
    lido$ano = i
    total_dados = rbind(total_dados,lido)
  }
  
  # anexando 2022
  lido = read_csv("dados_ironman_2022.csv")
  lido$ano = 2022
  total_dados = rbind(total_dados,lido)
  colnames(total_dados) = c("numero","nome","pais","genero","categoria","rank_categoria","tempo_total","rank_total","tempo_natacao","rank_natacao","tempo_bike","rank_bike","tempo_corrida","rank_corrida","status_finisher","ano")

  return(total_dados)
}

######## Leitura

total_dados = leitura()
im_2023 = read_csv("dados_ironman_2023.csv")
im_2023 = im_2023 %>% select(-T1,-T2,-numero)
colnames(im_2023) = c("nome","categoria","tempo_natacao","tempo_bike","tempo_corrida","tempo_total","rank_total","rank_categoria")
im_2023$ano = 2023
total_dados = total_dados %>% 
  filter(status_finisher == "Finisher") %>% 
  select(nome,categoria,tempo_natacao,tempo_bike,tempo_corrida,tempo_total,rank_total,rank_categoria,ano) %>% 
  filter(is.na(tempo_natacao) == FALSE & is.na(tempo_corrida) == FALSE & is.na(tempo_bike) == FALSE)

#juntando dados para historico 2006 a 2023
total_dados = rbind(total_dados,im_2023)

######## Limpeza

#alterando formato dos tempos
total_dados = total_dados %>% 
  mutate(tempo_bike = hms(tempo_bike),tempo_natacao = hms(tempo_natacao),tempo_corrida = hms(tempo_corrida),tempo_total=hms(tempo_total)) 

#padronizando os nomes dos finalistas
total_dados = total_dados %>% mutate(nome = tolower(nome))

#reordenando nomes de 2017
total_dados_2017 = total_dados %>% filter(ano == 2017)
aux = str_split(total_dados_2017$nome,", ")
nome = c()
sobrenome = c()
for (i in 1:length(aux)) {
  sobrenome = append(sobrenome,aux[[i]][1])
  nome = append(nome,aux[[i]][2])
}
total_dados_2017$nome = paste(nome,sobrenome)
total_dados = total_dados %>% filter(ano != 2017) %>% rbind(total_dados_2017)

#limpeza dos dados de natacao
total_dados$tempo_natacao = period_to_seconds(total_dados$tempo_natacao)
total_dados$tempo_natacao = ifelse(total_dados$tempo_natacao >= 7200, total_dados$tempo_natacao/60, total_dados$tempo_natacao)
total_dados$tempo_natacao = seconds_to_period(total_dados$tempo_natacao)

#limpeza nas categorias
total_dados$categoria = ifelse(total_dados$categoria == "M75-79","M70+",ifelse(total_dados$categoria == "M70-99","M70+",ifelse(total_dados$categoria == "M70-74","M70+",total_dados$categoria)))
total_dados$categoria = ifelse(total_dados$categoria == "F75-79","F70+",ifelse(total_dados$categoria == "F70-99","F70+",ifelse(total_dados$categoria == "F70-74","F70+",total_dados$categoria)))
total_dados = total_dados %>% filter(!categoria %in% c("CHL","PC","MPC","Anjo"))

######## Análise

### quantidade de finishers

# geral
q_finisher = total_dados %>% group_by(ano) %>% count()
plot1 <- ggplot(q_finisher,aes(x=ano, y = n)) +  geom_line() + geom_point() + xlab("Ano") + ylab("Quantidade") + ggtitle("Quantidade de finalistas - categorias masculinas")

#masculino
q_finisher_genre = total_dados %>% mutate(genre = ifelse(grepl('M', categoria),"Masculino","Feminino")) %>% group_by(genre,ano) %>% count() %>% ungroup()
ggplot(q_finisher_genre %>% rename(`Gênero` = genre) ,aes(x=ano, y = n, color = `Gênero`)) +  geom_line() + geom_point() + labs(x = "Ano", y = "Quantidade", fill = "Gênero",title = "Quantidade de finalistas - por gênero") +
  theme_minimal() +
  theme(legend.position = "top")
q_finisher_m = q_finisher_m %>% group_by(categoria) %>% summarise(media_participantes = round(mean(n)))

#masculino
q_finisher_m = total_dados %>% filter(grepl('M', categoria)) %>% group_by(categoria,ano) %>% count() %>% ungroup()
plot2 <-ggplot(q_finisher_m ,aes(x=ano, y = n, color = categoria)) +  geom_line() + geom_point() + xlab("Ano") + ylab("Quantidade") + ggtitle("Quantidade de finalistas - categorias masculinas")
 q_finisher_m = q_finisher_m %>% group_by(categoria) %>% summarise(media_participantes = round(mean(n)))

#feminino
q_finisher_f = total_dados %>% filter(grepl('F', categoria)) %>% group_by(categoria,ano) %>% count() %>% ungroup()
plot3 <- ggplot(q_finisher_f ,aes(x=ano, y = n, color = categoria)) +  geom_line() + geom_point() + xlab("Ano") + ylab("Quantidade") + ggtitle("Quantidade de finalistas - categorias femininas")
q_finisher_f = q_finisher_f %>% group_by(categoria) %>% summarise(media_participantes = round(mean(n)))

### evolução do tempo

#tempo total de prova por categoria
media_categoria = total_dados  %>% drop_na() %>% group_by(categoria,ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(period_to_seconds(tempo_total))))) %>% ungroup() 
evolucao_categoria_max_ano = media_categoria %>% group_by(categoria) %>% filter(ano == max(ano)) %>% ungroup()
evolucao_categoria_min_ano = media_categoria %>% group_by(categoria) %>% filter(ano == min(ano)) %>% ungroup()
evolucao_categoria = data.frame(categoria = evolucao_categoria_max_ano$categoria, diferenca = round(seconds_to_period(period_to_seconds(evolucao_categoria_max_ano$media_tempo_total) - period_to_seconds(evolucao_categoria_min_ano$media_tempo_total))))
ggplot(media_categoria %>% mutate(media_tempo_total = as.numeric(media_tempo_total)) ,aes(x=ano, y = media_tempo_total, color = categoria)) +  geom_line() + geom_point() + xlab("Ano") + ylab("Tempo") + ggtitle("Média de tempo por categoria") + scale_y_time(labels = scales::time_format(format = "%H:%M:%S"))

#tempo total de prova por genero
media_categoria_gen = total_dados %>% drop_na() %>% mutate(genre = ifelse(grepl('M', categoria),"Masculino","Feminino")) %>% group_by(genre,ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(period_to_seconds(tempo_total))))) %>% ungroup()  
ggplot(media_categoria_gen %>% mutate(media_tempo_total = as.numeric(media_tempo_total)) %>% rename(`Gênero`= genre) ,aes(x=ano, y = media_tempo_total, color = `Gênero`)) +  geom_line() + geom_point() + labs(x = "Ano", y = "Tempo", fill = "Gênero",title = "Tempo médio - por gênero") + scale_y_time(labels = scales::time_format(format = "%H:%M:%S"))

#tempo da natacao de prova por categoria
media_categoria_nat = total_dados %>% drop_na() %>% group_by(categoria,ano) %>% summarise(media_tempo_natacao = round(seconds_to_period(mean(period_to_seconds(tempo_natacao))))) %>% ungroup() 
evolucao_categoria_max_ano = media_categoria_nat %>% group_by(categoria) %>% filter(ano == max(ano)) %>% ungroup()
evolucao_categoria_min_ano = media_categoria_nat %>% group_by(categoria) %>% filter(ano == min(ano)) %>% ungroup()
evolucao_categoria_nat = data.frame(categoria = evolucao_categoria_max_ano$categoria, diferenca = round(seconds_to_period(period_to_seconds(evolucao_categoria_max_ano$media_tempo_natacao) - period_to_seconds(evolucao_categoria_min_ano$media_tempo_natacao))))

#tempo da bike de prova por categoria
media_categoria_bike = total_dados %>% drop_na() %>% group_by(categoria,ano) %>% summarise(media_tempo_bike = round(seconds_to_period(mean(period_to_seconds(tempo_bike))))) %>% ungroup() 
evolucao_categoria_max_ano = media_categoria_bike %>% group_by(categoria) %>% filter(ano == max(ano)) %>% ungroup()
evolucao_categoria_min_ano = media_categoria_bike %>% group_by(categoria) %>% filter(ano == min(ano)) %>% ungroup()
evolucao_categoria_bike = data.frame(categoria = evolucao_categoria_max_ano$categoria, diferenca = round(seconds_to_period(period_to_seconds(evolucao_categoria_max_ano$media_tempo_bike) - period_to_seconds(evolucao_categoria_min_ano$media_tempo_bike))))

#tempo da corrida de prova por categoria
media_categoria_cor = total_dados %>% drop_na() %>% group_by(categoria,ano) %>% summarise(media_tempo_corrida = round(seconds_to_period(mean(period_to_seconds(tempo_corrida))))) %>% ungroup() 
evolucao_categoria_max_ano = media_categoria_cor %>% group_by(categoria) %>% filter(ano == max(ano)) %>% ungroup()
evolucao_categoria_min_ano = media_categoria_cor %>% group_by(categoria) %>% filter(ano == min(ano)) %>% ungroup()
evolucao_categoria_cor = data.frame(categoria = evolucao_categoria_max_ano$categoria, diferenca = round(seconds_to_period(period_to_seconds(evolucao_categoria_max_ano$media_tempo_corrida) - period_to_seconds(evolucao_categoria_min_ano$media_tempo_corrida))))

### análise do impacto dos melhores da modalidade

#PRO masculino
top_m = total_dados %>% filter(categoria == "MPRO") %>% 
  mutate(tempo_total = period_to_seconds(tempo_total), tempo_natacao = period_to_seconds(tempo_natacao), tempo_bike = period_to_seconds(tempo_bike), tempo_corrida = period_to_seconds(tempo_corrida))
top_modal_m = top_m %>% 
  group_by(ano) %>% 
  summarise(top1nat = min(tempo_natacao), top1bike = min(tempo_bike), top1cor = min(tempo_corrida))

top_m$melhor_nat = c(0)
top_m$melhor_bike = c(0)
top_m$melhor_cor = c(0)

top_remold_m = data.frame()
for (i in 1:nrow(top_modal_m)) {
  ano_aux = top_m %>% filter(ano == top_modal_m$ano[i])
  for (j in 1:nrow(ano_aux)) {
    if(top_modal_m$top1nat[i] == ano_aux$tempo_natacao[j]){ano_aux$melhor_nat[j] = 1}
    if(top_modal_m$top1bike[i] == ano_aux$tempo_bike[j]){ano_aux$melhor_bike[j] = 1}
    if(top_modal_m$top1cor[i] == ano_aux$tempo_corrida[j]){ano_aux$melhor_cor[j] = 1}
  }
  top_remold_m = rbind(top_remold_m,ano_aux)
}

top_remold_m = top_remold_m %>% mutate(podio = ifelse(rank_categoria <= 3, 1 , 0)) %>% select(ano,podio,melhor_nat,melhor_bike,melhor_cor)
top_e_podio_m = top_remold_m %>% filter(podio == 1) %>% group_by(melhor_nat,melhor_bike,melhor_cor) %>% count()
top_e_podio_m$porc = top_e_podio_m$n/sum(top_e_podio_m$n)
  
#analise do podio masculino
podio_geral_m = top_m %>% filter(rank_categoria <= 3)

#evolução do tempo total de prova
media_podio_geral_m = podio_geral_m  %>% drop_na() %>% group_by(ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(tempo_total)))) %>% ungroup() 
media_podio_geral_m$tipo = "Geral"

#evolução do tempo da natacao de prova
media_podio_geral_m_nat = podio_geral_m  %>% drop_na() %>% group_by(ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(tempo_natacao)))) %>% ungroup() 
media_podio_geral_m_nat$tipo = "Natação"

#evolução do tempo da bike de prova 
media_podio_geral_m_bike = podio_geral_m  %>% drop_na() %>% group_by(ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(tempo_bike)))) %>% ungroup() 
media_podio_geral_m_bike$tipo = "Ciclismo"

#evolução do tempo da corrida de prova
media_podio_geral_m_cor = podio_geral_m  %>% drop_na() %>% group_by(ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(tempo_corrida)))) %>% ungroup() 
media_podio_geral_m_cor$tipo = "Corrida"

media_podio_geral_m = rbind(rbind(rbind(media_podio_geral_m,media_podio_geral_m_nat),media_podio_geral_m_bike),media_podio_geral_m_cor)

ggplot(media_podio_geral_m %>% mutate(media_tempo_total = as.numeric(media_tempo_total)),aes(x=ano, y = media_tempo_total, color = tipo)) +  geom_line() + geom_point() + labs(x = "Ano", y = "Tempo", color = "Tipo",title = "Evolução histórica - categoria MPRO") +
  theme_minimal() +
  theme(legend.position = "top") + scale_y_time(labels = scales::time_format(format = "%H:%M:%S"))

#PRO feminino
top_f = total_dados %>% filter(categoria == "FPRO") %>% 
  mutate(tempo_total = period_to_seconds(tempo_total), tempo_natacao = period_to_seconds(tempo_natacao), tempo_bike = period_to_seconds(tempo_bike), tempo_corrida = period_to_seconds(tempo_corrida))
top_fodal_f = top_f %>% 
  group_by(ano) %>% 
  summarise(top1nat = min(tempo_natacao), top1bike = min(tempo_bike), top1cor = min(tempo_corrida))

top_f$melhor_nat = c(0)
top_f$melhor_bike = c(0)
top_f$melhor_cor = c(0)

top_remold_f = data.frame()
for (i in 1:nrow(top_fodal_f)) {
  ano_aux = top_f %>% filter(ano == top_fodal_f$ano[i])
  for (j in 1:nrow(ano_aux)) {
    if(top_fodal_f$top1nat[i] == ano_aux$tempo_natacao[j]){ano_aux$melhor_nat[j] = 1}
    if(top_fodal_f$top1bike[i] == ano_aux$tempo_bike[j]){ano_aux$melhor_bike[j] = 1}
    if(top_fodal_f$top1cor[i] == ano_aux$tempo_corrida[j]){ano_aux$melhor_cor[j] = 1}
  }
  top_remold_f = rbind(top_remold_f,ano_aux)
}

top_remold_f = top_remold_f %>% mutate(podio = ifelse(rank_categoria <= 3, 1 , 0)) %>% select(ano,podio,melhor_nat,melhor_bike,melhor_cor)
top_e_podio_f = top_remold_f %>% filter(podio == 1) %>% group_by(melhor_nat,melhor_bike,melhor_cor) %>% count()
top_e_podio_f$porc = top_e_podio_f$n/sum(top_e_podio_f$n)

#analise do podio feminino
podio_geral_f = top_f %>% filter(rank_categoria <= 3)

#evolução do tempo total de prova
media_podio_geral_f = podio_geral_f  %>% drop_na() %>% group_by(ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(tempo_total)))) %>% ungroup() 
media_podio_geral_f$tipo = "Geral"

#evolução do tempo da natacao de prova
media_podio_geral_f_nat = podio_geral_f  %>% drop_na() %>% group_by(ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(tempo_natacao)))) %>% ungroup() 
media_podio_geral_f_nat$tipo = "Natação"

#evolução do tempo da bike de prova 
media_podio_geral_f_bike = podio_geral_f  %>% drop_na() %>% group_by(ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(tempo_bike)))) %>% ungroup() 
media_podio_geral_f_bike$tipo = "Ciclismo"

#evolução do tempo da corrida de prova
media_podio_geral_f_cor = podio_geral_f  %>% drop_na() %>% group_by(ano) %>% summarise(media_tempo_total = round(seconds_to_period(mean(tempo_corrida)))) %>% ungroup() 
media_podio_geral_f_cor$tipo = "Corrida"

media_podio_geral_f = rbind(rbind(rbind(media_podio_geral_f,media_podio_geral_f_nat),media_podio_geral_f_bike),media_podio_geral_f_cor)
ggplot(media_podio_geral_f %>% mutate(media_tempo_total = as.numeric(media_tempo_total)),aes(x=ano, y = media_tempo_total, color = tipo)) +  geom_line() + geom_point() + labs(x = "Ano", y = "Tempo", color = "Tipo",title = "Evolução histórica - categoria FPRO") +
  theme_minimal() +
  theme(legend.position = "top") + scale_y_time(labels = scales::time_format(format = "%H:%M:%S"))

# podium por país
data <- read_csv("paises_podium.csv", 
                          col_types = cols(Ano = col_integer()))

data_long <- data %>%
  pivot_longer(cols = c(Masculino, Feminino), names_to = "Gender", values_to = "Country") %>%
  group_by(Gender, Country) %>%
  summarise(Gold_Medals = n())

max_count <- max(data_long$Gold_Medals)

# Create the ggplot graph
ggplot(data_long, aes(x = Country, y = Gold_Medals, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x = "País",
    y = "Quantidade",
    fill = "",
    title = "Campeões no Ironman Brasil por País",
  ) +
  scale_y_continuous(breaks = seq(1, max_count, by = 1)) +
  theme_minimal() +
  theme(legend.position = "top")

# diferença entre pro e amador

pior_pro_masc = total_dados %>% filter(categoria == "MPRO") %>% group_by(ano) %>% summarise(tempo = max(period_to_seconds(tempo_total))) %>% mutate(categoria = "MPRO")
melhor_amador_masc = total_dados %>% drop_na() %>% filter(grepl("M",categoria) & categoria != "MPRO") %>% group_by(ano) %>% summarise(tempo = min(period_to_seconds(tempo_total))) %>% mutate(categoria = "Amador")
comp_pro_amador = rbind(melhor_amador_masc,pior_pro_masc)
ggplot(comp_pro_amador,aes(x=ano, y = tempo, color = categoria)) +  geom_line() + geom_point() + labs(x = "Ano", y = "Tempo", color = "Categoria",title = "Pior tempo MPRO vs melhor tempo amador") +
  theme_minimal() +
  theme(legend.position = "top") + scale_y_time(labels = scales::time_format(format = "%H:%M:%S"))


pior_pro_fem = total_dados %>% filter(categoria == "FPRO") %>% group_by(ano) %>% summarise(tempo = max(period_to_seconds(tempo_total))) %>% mutate(categoria = "FPRO")
melhor_amador_fem = total_dados %>% drop_na() %>% filter(grepl("F",categoria) & categoria != "FPRO") %>% group_by(ano) %>% summarise(tempo = min(period_to_seconds(tempo_total))) %>% mutate(categoria = "Amador")
comp_pro_amador = rbind(melhor_amador_fem,pior_pro_fem)
ggplot(comp_pro_amador,aes(x=ano, y = tempo, color = categoria)) +  geom_line() + geom_point() + labs(x = "Ano", y = "Tempo", color = "Categoria",title = "Pior tempo FPRO vs melhor tempo amador") +
  theme_minimal() +
  theme(legend.position = "top") + scale_y_time(labels = scales::time_format(format = "%H:%M:%S"))
