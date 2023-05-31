
# Carregue os pacotes necessários
library(quantmod)
library(PerformanceAnalytics)
library(sidrar)
library(tidyverse)

# Defina as datas de início e fim desejadas
data_inicio <- as.Date("2002-01-01")
data_fim <- as.Date("2022-12-31")

# Baixe os preços do índice Bovespa usando o pacote quantmod
getSymbols("^BVSP", from = data_inicio, to = data_fim)

# Converta os preços diários em preços semanais
precos_semanais <- to.weekly(BVSP)

# Extraia os preços ajustados de fechamento dos preços semanais
precos_ajustados <- Ad(precos_semanais)

# Converta as datas para o formato de data
datas <- as.Date(index(precos_ajustados))

# Crie um dataframe com os preços ajustados e datas
dados <- data.frame(Ano = as.numeric(format(datas, "%Y")), Preco_Ajustado = as.numeric(precos_ajustados))

# Agrupe os preços por ano e calcule o preço ajustado anual
precos_anuais <- aggregate(Preco_Ajustado ~ Ano, dados, last)

# Plotar os preços anuais ajustados usando ggplot
ggplot(precos_anuais, aes(x = Ano, y = Preco_Ajustado)) +
  geom_col() +
  labs(x = "Ano", y = "Preço Ajustado", title = "Preços Anuais Ajustados do Índice Bovespa")



# pib ----------------------------------------------------------------



## Importação dos dados da poupança e da fbc
data = get_sidra(api='/t/2072/n1/all/v/933/p/all')

data_inicial <- ymd("2000-01-01")
data_final <- ymd("2022-12-31")

# Gera as datas trimestrais
datas_trimestrais <- seq(data_inicial, data_final, by = "3 months")

pib <- data %>% 
  select(Valor)

pib_tri <- data.frame(datas_trimestrais,pib)

# Calcula o PIB anual utilizando o último valor de cada ano
pib_ano <- pib_tri %>%
  mutate(Ano = year(datas_trimestrais)) %>%
  group_by(Ano) %>%
  summarize(Valor_Ajustado = last(Valor)) %>% 
  slice(3:23)


# juntando as duas tabelas ------------------------------------------------

tabcomp <- data.frame(pib_ano$Ano,pib_ano$Valor_Ajustado,precos_anuais$Preco_Ajustado)
colnames(tabcomp) <- c("data","PIB","IBOV")

tabcomp <- tabcomp %>%
  mutate(across(c(PIB, IBOV), log10, .names = "log_{.col}")) %>% 
  select(1,4:5) %>% 
  pivot_longer(2:3,names_to = "Valor")

ggplot(tabcomp, aes(x = data, y = value, color = Valor)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red"),labels = c("IBOVESPA","PIB")) +
  labs(
    title = "IBOV anual x PIB Brasil anual de 2002 a 2022",
    x = "Ano",
    y = "log10",
    color = "Variáveis"
  ) + 
  theme_minimal() +
  ggeasy::easy_center_title()