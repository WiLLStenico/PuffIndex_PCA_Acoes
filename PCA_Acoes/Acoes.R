
# Instalando e carregando pacotes -----------------------------------------

pacotes <- c("plotly", "tidyverse","knitr","kableExtra","car","rgl","gridExtra",
             "PerformanceAnalytics","reshape2","rayshader","psych","pracma",
             "polynom","rqPen","ggrepel","factoextra","sp","tmap","magick","gridExtra","sjmisc", "data.table", "jpeg" )

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


rm(pacotes)

################################################################################
#ACOES
################################################################################
#-------------------------------------------------------

#https://statusinvest.com.br/acoes/busca-avancada

# Carregando a base de dados
acoes <- read_delim("dados/acoes.csv", na = '0', delim = ';',
                    col_types = 'cnnnnnnnnnnnnnnnnnnnnnnnnnnnnn')

acoes[is.na(acoes)] <- 0 #Definindo como 0 qualquer campo N/A

acoes <- acoes %>% filter(acoes$PRECO != 0)
acoes <- acoes %>% filter(acoes$" VALOR DE MERCADO" != 0)
acoes <- acoes %>% filter(acoes$" LIQUIDEZ MEDIA DIARIA" != 0)
acoes <- acoes %>% filter(acoes$"PATRIMONIO / ATIVOS" > 0)

str(acoes)

#VARS DISPONIVEIS(Significado ao fim do código):
#
#"TICKER", "PRECO", "DY", "P/L", "P/VP", "P/ATIVOS", "MARGEM BRUTA", "MARGEM EBIT", "MARG. LIQUIDA", "P/EBIT", "EV/EBIT", "DIVIDA LIQUIDA / EBIT", "DIV. LIQ. / PATRI.", "PSR", "P/CAP. GIRO", "P. AT CIR. LIQ.", "LIQ. CORRENTE", "ROE", "ROA", "ROIC", "PATRIMONIO / ATIVOS", "PASSIVOS / ATIVOS", "GIRO ATIVOS", "CAGR RECEITAS 5 ANOS", "CAGR LUCROS 5 ANOS", " LIQUIDEZ MEDIA DIARIA", " VPA", " LPA", " PEG Ratio", " VALOR DE MERCADO"
##################################################################################################################################

#USANDO:
#"TICKER", "DY", "MARGEM BRUTA", "MARG. LIQUIDA", "DIV. LIQ. / PATRI.", "LIQ. CORRENTE", "ROE", "ROA", "ROIC", "PATRIMONIO / ATIVOS",  "GIRO ATIVOS", "CAGR RECEITAS 5 ANOS", " VPA", " LPA", " PEG Ratio", " VALOR DE MERCADO"
#acoes <- acoes [,c("TICKER", "DY", "MARG. LIQUIDA", "DIV. LIQ. / PATRI.", "LIQ. CORRENTE", "ROE", "ROA", "ROIC", "PATRIMONIO / ATIVOS",  "GIRO ATIVOS", "CAGR RECEITAS 5 ANOS", " VPA", " LPA", " VALOR DE MERCADO")]


#acoes <- acoes [,c("TICKER", "DY", "P/L", "P/VP", "MARGEM BRUTA", "MARGEM EBIT", "MARG. LIQUIDA", "P/EBIT", "EV/EBIT", "DIVIDA LIQUIDA / EBIT", "DIV. LIQ. / PATRI.", "P/CAP. GIRO", "ROE", "ROA", "ROIC", "PATRIMONIO / ATIVOS", "PASSIVOS / ATIVOS", "GIRO ATIVOS", "CAGR RECEITAS 5 ANOS", "CAGR LUCROS 5 ANOS", " LIQUIDEZ MEDIA DIARIA", " VPA", " LPA", " VALOR DE MERCADO")]

acoes <- acoes [,c("TICKER", "DY", "P/L", "P/VP", "MARGEM BRUTA", "MARGEM EBIT", "MARG. LIQUIDA", "P/EBIT", "EV/EBIT", "DIVIDA LIQUIDA / EBIT", "DIV. LIQ. / PATRI.", "PSR", "P/CAP. GIRO", "P. AT CIR. LIQ.", "ROE", "ROA", "ROIC", "PATRIMONIO / ATIVOS", "PASSIVOS / ATIVOS", "GIRO ATIVOS", " VPA", " LPA", " PEG Ratio", " VALOR DE MERCADO")]

###################################################################################
#Lista de ações unicas, selecionando qual possui maior DY quando ON e PN por exemplo

index_add <- NULL
for(i in 1:nrow(acoes)){
  
  #Para cada ação, procura todas as ocorrencias do TICKER no dataframe(Ex: SULA)
  acao_unica <- acoes[acoes$TICKER %flike% substr(acoes[i,1], 1, 4),]  
  
  #Adiciona o Ticker com maior DY na lista
  index_add <- c(index_add, acao_unica[which.max(acao_unica$DY),"TICKER"])
  
}

#Remove os duplicados
index_add <- unique(index_add)

#TODO: Melhorar processo de remoção dos duplicados

#Atualiza dataframe apenas com os TICKERS unicos
acoes <- acoes[ acoes$TICKER %in% index_add, ]


################ Invertendo termos ##################

acoes$"DIVIDA LIQUIDA / EBIT" <- acoes$"DIVIDA LIQUIDA / EBIT" * -1
acoes$"DIV. LIQ. / PATRI." <- acoes$"DIV. LIQ. / PATRI." * -1

acoes$"EV/EBIT" <- acoes$"EV/EBIT" * -1
acoes$"P/EBIT" <- acoes$"P/EBIT" * -1

####################################################



acoes <- column_to_rownames(acoes, var = "TICKER")

#####################################################################

# Analisando as correlações entre variáveis da base de dados
chart.Correlation(acoes, histogram = TRUE, pch = "+")

# Salvando a Matriz de Correlações -----------------------------------
rho_acoes <- cor(acoes)

####################################################################

# Construindo um mapa de calor a partir das correlações
rho_acoes %>% 
  melt() %>% 
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  geom_text(aes(x = Var1, y = Var2, label = round(x = value, digits = 3)),
            size = 4) +
  labs(x = NULL,
       y = NULL,
       fill = "Correlações") +
  scale_fill_gradient2(low = "dodgerblue4", 
                       mid = "white", 
                       high = "brown4",
                       midpoint = 0) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90))

###########################################################################

# O teste de efericidade de Bartlett --------------------------------------
cortest.bartlett(R = rho_acoes)


# O algoritmo prcomp(), do pacote psych, EXIGE que a a matriz de dados fornecida
# a ele já esteja padronizada pelo procedimento zscores:

#padronizando a base(requisito do pacote psych)
acoes_std <- scale(acoes)

# Rodando a PCA
afpc_acoes <- prcomp(acoes_std)
#summary(afpc_acoes)

# Sumarizando resultado:
data.frame(eigenvalue = afpc_acoes$sdev ^ 2,
           var_compartilhada = summary(afpc_acoes)$importance[2,],
           var_cumulativa = summary(afpc_acoes)$importance[3,]) -> relatorio

relatorio %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Visualizando os pesos que cada variável tem em cada componente principal 
# obtido pela PCA
ggplotly(
  data.frame(afpc_acoes$rotation) %>%
    mutate(var = names(acoes)) %>%
    melt(id.vars = "var") %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(x = var, y = value, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~variable) +
    labs(x = NULL, y = NULL, fill = "Legenda:") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90))
)

# Scree Plot - apenas ignorar os warnings
ggplotly(
  fviz_eig(X = afpc_acoes,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod4")
)


#########################################################

#Extraindo as Cargas Fatoriais
k <- sum((afpc_acoes$sdev ^ 2) > 1) 

cargas_fatoriais <- afpc_acoes$rotation[, 1:k] %*% diag(afpc_acoes$sdev[1:k])

# Visualizando as cargas fatoriais
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

#Visualizando as Comunalidades
data.frame(rowSums(cargas_fatoriais ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
data.frame(cargas_fatoriais) %>%
  rename(F1 = X1,
         F2 = X2) %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Scores Fatoriais
scores_fatoriais <- t(afpc_acoes$rotation)/afpc_acoes$sdev 
colnames(scores_fatoriais) <- colnames(acoes_std)

scores_fatoriais

scores_fatoriais %>%
  t() %>%
  data.frame() %>%
  rename(PC1 = 1,
         PC2 = 2) %>%
  #select(PC1, PC2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

###########################################################################

# Proposta da construção de um ranking ------------------------------------

#Calculando a variância compartilhada
var_compartilhada <- (afpc_acoes$sdev ^ 2/sum(afpc_acoes$sdev ^ 2))
var_compartilhada

#-------------------------

for(var in 1:k){
  
  #Calcula score fatorial
  score_D <- scores_fatoriais[var,]
  
  #Estabelecendo o ranking dos indicadores assumido
  F <- t(apply(acoes_std, 1, function(x) x * score_D))
  
  #PARA ESTE CENARIO: CASO MAIOR VALOR DO MODULO DO COMPONENTE SEJA NEGATIVO, 
  #ENTÃO INVERTEMOS O SINAL PARA BENEFICIAR O RANKEAMENTO
  if(score_D[which.max(abs(score_D))] < 0){ 
    F <- data.frame(F) %>%
      mutate(fator = rowSums(.) * -1)
    print(var)
  }else{
    F <- data.frame(F) %>%
      mutate(fator = rowSums(.) * 1)
  }
  F <- data.frame(F) %>%
    mutate(fator_ponderado = rowSums(.) * var_compartilhada[var])
  
  # Importando as colunas de fatores
  acoes[paste("Fator",var)] <- F$fator
  acoes[paste("fator_ponderado",var)] <- F$fator_ponderado
  
  
  if(var == 1){
    fatores_ponderados_colname <- c(paste("fator_ponderado",var))
  }else{
    fatores_ponderados_colname <- c(fatores_ponderados_colname,paste("fator_ponderado",var))
  }
}

#Somando fatores ponderados
acoes$pontuacao <- rowSums(acoes[ , fatores_ponderados_colname])

# Visualizando o ranking final
acoes %>%
  arrange(desc(pontuacao)) %>%
  mutate(POS = row_number())  %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

result  <- acoes %>%
  arrange(desc(pontuacao)) %>%
  mutate(POS = row_number())


view(result[,c("POS","pontuacao")])

result <- result[,c(ncol(result),1:ncol(result))]

view(result)

#Caso quiséssemos salvar o nosso data frame 'dados' em formato *.csv:
write.csv(result, file = "dados/result.csv", row.names = TRUE)

######################################## FIM #####################################

##################################################################################
#SIGNIFICADO DAS VARIAVEIS::
#---------------
# DY:   
#   Indicador utilizado para relacionar os proventos pagos por uma companhia e o preço atual de suas ações.
#
#
# P/L:
#   Dá uma ideia do quanto o mercado está disposto a pagar pelos lucros da empresa.
# #FÓRMULA:
# PREÇO ATUAL/LPA(Lucro por ação)
# 
# 
# PEG Ratio:
#   O PEG Ratio é uma métrica de avaliação para determinar o trade-off relativo entre o preço de uma ação, o lucro gerado por ação e o crescimento esperado da empresa
# #FÓRMULA:
# (P/L) / [(LPA Atual / LPA Anterior) - 1]
# 
# P/VP:
#   Facilita a análise e comparação da relação do preço de negociação de um ativo com seu VPA.
# #FÓRMULA:
# PREÇO DA AÇÃO/VPA(Valor Patrimonial por ação)
# 
# 
# P/Ativos:
#   Preço da ação dividido pelos Ativos totais por ação
# 
# Margem Bruta:
#   Mede, objetivamente, o quanto a empresa ganha com a venda de seus produtos
# #FÓRMULA:
# (LUCRO BRUTO/ RECEITA LÍQUIDA)x 100
# 
# 
# Margem Ebit:
#   Útil para comparar a lucratividade operacional de empresas do mesmo segmento, além de contribuir para avaliar o crescimento da eficiência produtiva de um negócio ao longo do tempo.
# #FÓRMULA:
# (EBITb / RECEITA LÍQUIDA)x 100
# 
# 
# Marg. Líquida:
#   Revela a porcentagem de lucro em relação às receitas de uma empresa.
# #FÓRMULA:
# (LUCRO LÍQUIDO/ RECEITA LÍQUIDA)x 100
# 
# 
# P/EBIT:
#   Indica qual é o preço da ação em relação ao seu resultado EBIT. O EBIT pode ser considerado uma aproximação do lucro operacional da companhia.
# #FÓRMULA:
# PREÇO DA AÇÃOb / EBIT(Lucro antes de juros e impostos)
# 
#
# EV/EBIT:
#   
#   O EV (Enterprise Value ou Valor da Firma), indica quanto custaria para comprar todos os ativos da companhia, descontando o caixa. Este indicador mostra quanto tempo levaria para o valor calculado no EBIT pagar o investimento feito para compra-la.
# #FÓRMULA:
# EVb / EBIT
# 
# 
# Dívida Líquida / EBIT:
#   Indica quanto tempo seria necessário para pagar a dívida líquida da empresa considerando o EBIT atual. Indica também o grau de endividamento da companhia.
# #FÓRMULA:
# DÍVIDA LÍQUIDAb / EBIT
# 
# 
# Dívida Líquida / Patrimônio:
#   Indica quanto de dívida uma empresa está usando para financiar os seus ativos em relação ao patrimônio dos acionistas.
# #FÓRMULA:
# DÍVIDA LÍQUIDAb / PATRIMÔNIO LÍQUIDO
# 
# 
# PSR:
#   Valor de mercado da empresa dividido pela receita operacional líquida ou preço da ação dividido pela receita líquida por ação
# 
# 
# P/Cap. Giro:
#   
#   Preço da ação dividido pelo capital de giro por ação. Capital de Giro é o Ativo Circulante menos Passivo Circulante.
# #FÓRMULA:
# PREÇO DA AÇÃOb / CAPITAL DE GIRO
# 
# 
# Preço sobre Ativo Circ. Líq.:
#   É a diferença entre o ativo circulante o passivo total que constam do balanço patrimonial de uma empresa
# #FÓRMULA:
# PREÇO DA AÇÃOb / ATIVOS CIRCULANTES LÍQUIDOS POR AÇÃO
# 
# 
# ROE:
#   Mede a capacidade de agregar valor de uma empresa a partir de seus próprios recursos e do dinheiro de investidores
# #FÓRMULA:
# (LUCRO LÍQUIDO/ PATRIMÔNIO LÍQUIDO)x 100
# 
# 
# ROIC:
#   Mede a rentabilidade de dinheiro o que uma empresa é capaz de gerar em razão de todo o capital investido, incluindo os aportes por meio de dívidas.
# #FÓRMULA:
# (EBIT - IMPOSTOS)/ (PATRIMÔNIO LÍQUIDO + ENDIVIDAMENTO)
# 
# ROA:
#   
#   O retorno sobre os ativos ou Return on Assets, é um indicador de rentabilidade, que calcula a capacidade de uma empresa gerar lucro a partir dos seus ativos,
# além de indiretamente, indicar a eficiência dos seus gestores.
# #FÓRMULA:
# LUCRO LÍQUIDOb / ATIVO TOTAL
# 
# 
# Liquidez Corrente:
#   Indica a capacidade de pagamento da empresa no curto prazo.
# #FÓRMULA:
# ATIVO CIRCULANTEb / PASSIVO CIRCULANTE
# 
# 
# Patrimônio / Ativos:
#   O Patrimônio de uma empresa é o resultado da subtração dos ativos com os passivos. Este indicador é para mostrar a relação dos ativos no patrimônio da empresa.
# #FÓRMULA:
# PATRIMÔNIOb / ATIVOS
# 
# 
# Passivos / Ativos:
#   
# 
# Giro Ativos:
#   Mede se como uma empresa está utilizando o seu ativo (bens, investimentos, estoque etc.) para produzir riqueza, através da venda de seus produtos e/ou serviços.
# #FÓRMULA:
# RECEITA LÍQUIDAb / TOTAL MÉDIO DE ATIVOS
# 
# 
# CAGR Receitas 5 Anos:
#   O CAGR (Compound Annual Growth Rate), ou taxa de crescimento anual composta, é a taxa de retorno necessária para um investimento crescer de seu saldo inicial para o seu saldo final.
# 
# CAGR Lucros 5 Anos:
#   O CAGR (Compound Annual Growth Rate), ou taxa de crescimento anual composta, é a taxa de retorno necessária para um investimento crescer de seu saldo inicial para o seu saldo final.
# 
# Liquidez média diária:
#   Média dos últimos 30 dias
# 
# VPA:
#   Indica o valor patrimonial por ação.
# 
# LPA:
#   Indica o lucro líquido por ação.
# 
# Valor de mercado:
#   O valor de mercado, refere-se ao preço que o mercado está pagando por uma determinada empresa, baseando-se na lei da oferta e da procura e concorrência de mercado.
#---------------------