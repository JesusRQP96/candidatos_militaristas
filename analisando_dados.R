#######################################################################
#                                                                     #
# Código de Marcio de Lucas - delucasmarcio@gmail.com                 #
# Working Project: Determinantes do Voto em Candidatos Militaristas   #
# Janeiro de 2019.                                                   #
# Obs: No final do Scritp há um codebook das variáveis                # 
#                                                                     #
#######################################################################

## Carregando Bibliotecas
library(tidyr)
library(ggplot2)
library(ggthemes )
library(car)
library(arm)
library(lme4)
library(MuMIn)
library(Matching)

## Direcionando para Source
setwd('~/novo_dissertacao/data/')

## Carregando Dados
# Códigos
cod <- read.csv('codigos_ibge_tse.csv', sep = ',')

# TSE
tse <- read.table('tse_treated.csv', sep = ',',
                  header = T)

# IPEA
ipea <- read.table('final_ipea.txt',sep = ';',
                   header = T)

# Censo
rel <- read.table('final_religiao_mun.txt', sep = ';',
                  header = T)

# Atlas dos Municípios
atlas <- read.table('final_atlas.txt', sep = ';',
                    header = T)

# Base de Dados Completa (TSE, SUS, IBGE, PNAD, IPEA)
df <- read.table('all_data.csv', sep = ',',
                 header = T)


colnames(df)
# Eliminando Missing Values
bb <- df[c('votos_seg','homicidio','alcool','drogas','despesa_seg',
           'partido_seg','n_part_seg','politico_pop','branco_nulo',
           'jovem','branca','prop_evangelico','anos_estudo','renda_media',
           'margem_vit','ano','uf','prop_seg','servicos','renda_dif',
           'tx_inpc','tx_pib','tx_pop','pib_pc','reg','cod_mun')]

bb <- na.omit(bb)

### Gráficos
# Função Multiplot 
# From: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## Evolução da Proporção de Candidatos Militaristas
p1 <- data.frame(y = tapply(tse$prop_seg, tse$ano, mean, na.rm = T) * 100,
                 x = c(2004,2008,2012,2016))
ggplot(p1, aes(x = x, y = y)) +
  labs(title = 'Brasil', y = '% Candidatos Militaristas', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))

## Evolução do Número Absoluto de Candidatos Militaristas
p2.1 <- data.frame(y = tapply(tse$prop_seg * tse$total_pol, 
                              tse$ano, sum, na.rm = T),
                 x = c(2004,2008,2012,2016))

p2.2 <- data.frame(y = tapply(tse$total_pol, 
                              tse$ano, sum, na.rm = T),
                   x = c(2004,2008,2012,2016))

multiplot(
  
ggplot(p2.1, aes(x = x, y = y)) +
  labs(title = 'Total de Candidatos Militaristas', y = '', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))
,
ggplot(p2.2, aes(x = x, y = y)) +
  labs(title = 'Total de Candidatos', y = '', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))

, cols = 2)

## Evolução da Eleição de Politicos Militaristas
p3 <- data.frame(y = tapply(tse$elect_seg, tse$ano, mean, na.rm = T) * 100,
                 x = c(2004,2008,2012,2016))
ggplot(p3, aes(x = x, y = y)) +
  labs(title = 'Brasil', y = '% Vereadores Eleitos', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))

## Evolução do Desempenho de Candidaturas Militaristas
p4 <- data.frame(y = tapply(tse$votos_seg, tse$ano, mean, na.rm = T),
                 x = c(2004,2008,2012,2016))
ggplot(p4, aes(x = x, y = y)) +
  labs(title = 'Brasil', y = 'Desempenho Eleitoral', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))

## Evolução do Financiamento de Candidaturas Militaristas
p5 <- data.frame(y = tapply(tse$despesa_seg, tse$ano, mean, na.rm = T),
                 x = c(2004,2008,2012,2016))

ggplot(p5, aes(x = x, y = y)) +
  labs(title = 'Brasil', y = 'Despesa de Campanha', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))

## Evolução do Financiamento de Candidaturas
p6.1 <- data.frame(y = tapply(tse$total_despesa, tse$ano, 
                              sum, na.rm = T) / 10 ^ 6,
                   x = c(2004,2008,2012,2016))

p6.2 <- data.frame(y = tapply(tse$despesa_seg * tse$sd_despesa + 
                                (tse$total_despesa / tse$total_pol), 
                              tse$ano, sum, na.rm = T) / 10 ^ 6,
                   x = c(2004,2008,2012,2016))

# Inflação
infl <- data.frame(ipca = c(7.6, 5.69, 3.14, 4.46, 5.9,
                            4.31, 5.91, 6.5, 5.84, 5.91,
                            6.41, 10.67, 6.29),
                   ano = 2004:2016)

infl <- data.frame(ipca = c(0, sum(infl$ipca[2:5]),
                            sum(infl$ipca[6:9]),
                            sum(infl$ipca[10:13])),
                   ano = c(2004, 2008, 2012, 2016))

# Descontando Inflação
p6.1$y <- p6.1$y * (1 - infl$ipca / 100)
p6.2$y <- p6.2$y * (1 - infl$ipca / 100)

## Crescimento Médio
# Total das Campanhas de Vereadores
mean((p6.1$y[2:4] - p6.1$y[1:3])  / p6.1$y[1:3])
# Campanhas de Vereadores Militaristas
mean((p6.2$y[2:4] - p6.2$y[1:3])  / p6.2$y[1:3])

multiplot(
  
ggplot(p6.1, aes(x = x, y = y)) +
  labs(title = 'Despesa Total de Campanha', y = '(milhões)', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))
,
ggplot(p6.2, aes(x = x, y = y)) +
  labs(title = 'Despesa de Militaristas', y = '(milhões)', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))

, cols = 2)

## Evolução da Eleição de Politicos Militaristas em Grandes Cidades
p7 <- data.frame(y = with(tse[tse$aptos > quantile(tse$aptos)[4],],{
  tapply(elect_seg, ano, mean, na.rm = T) * 100}),
                 x = c(2004,2008,2012,2016))

ggplot(p7, aes(x = x, y = y)) +
  labs(title = 'Grandes Cidades', y = '% Vereadores Eleitos', x = 'Ano') +
  geom_line(color = 'blue', size = 2.5) +
  geom_point(color = 'blue', size = 4) + 
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5))

## Checando balanceamento entre banco de dados completo e banco
# de dados com atrito
cod$cod_mun <- as.numeric(substr(cod$cod_municipio_ibge,1,6))

select_mun <- cod$cod_municipio_tse[cod$cod_mun %in% bb$cod_mun]

p8 <- with(tse,{
  x1 = mean(votos_seg[cod_mun %in% select_mun],na.rm = T)
  x2 = mean(votos_seg,na.rm = T)
  dd = data.frame(y = c(x1,x2),
                  x = c('Dados Completos','Dados Após Merge'))
  return(dd)
})

p9 <- with(tse,{
  x1 = mean(prop_seg[cod_mun %in% select_mun],na.rm = T)
  x2 = mean(prop_seg,na.rm = T)
  dd = data.frame(y = c(x1,x2),
                  x = c('Dados Completos','Dados Após Merge'))
  return(dd)
})

p10 <- with(tse,{
  x1 = mean(despesa_seg[cod_mun %in% select_mun],na.rm = T)
  x2 = mean(despesa_seg,na.rm = T)
  dd = data.frame(y = c(x1,x2),
                  x = c('Dados Completos','Dados Após Merge'))
  return(dd)
})

p11 <- with(ipea,{
  pib_pc = pib / populacao
  x1 = mean(pib_pc[cod_mun %in% bb$cod_mun],na.rm = T)
  x2 = mean(pib_pc,na.rm = T)
  dd = data.frame(y = c(x1,x2),
                  x = c('Dados Completos','Dados Após Merge'))
  return(dd)
})

p12 <- with(rel,{
  x1 = mean(prop_evangelico[cod_mun %in% bb$cod_mun],na.rm = T)
  x2 = mean(prop_evangelico,na.rm = T)
  dd = data.frame(y = c(x1,x2),
                  x = c('Dados Completos','Dados Após Merge'))
  return(dd)
})

p13 <- with(atlas,{
  x1 = mean(anos_estudo[cod_mun %in% bb$cod_mun],na.rm = T)
  x2 = mean(anos_estudo,na.rm = T)
  dd = data.frame(y = c(x1,x2),
                  x = c('Dados Completos','Dados Após Merge'))
  return(dd)
})

multiplot(
  
ggplot(p8, aes(y = y, x = x)) +
  labs(title = 'Desemp. Eleitoral Cand. Militaristas', y = '', x = '') +
  geom_bar(stat = 'identity', fill = c('blueviolet', 'blue')) +
  geom_text(aes(label = round(y,2)), vjust = - 1, color = 'white', size = 6,
            family = 'serif') +
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5),
        axis.text = element_text(size = 20))

,

ggplot(p9, aes(y = y, x = x)) +
  labs(title = 'Prop. de Cand. Militaristas', y = '', x = '') +
  geom_bar(stat = 'identity', fill = c('blueviolet', 'blue')) +
  geom_text(aes(label = round(y,4)), vjust = 1.6, color = 'white', size = 6,
            family = 'serif') +
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5),
        axis.text = element_text(size = 20))

,

ggplot(p10, aes(y = y, x = x)) +
  labs(title = 'Desp. Campanha Cand. Militaristas', y = '', x = '') +
  geom_bar(stat = 'identity', fill = c('blueviolet', 'blue')) +
  geom_text(aes(label = round(y,5)), vjust = - 1, color = 'white', size = 6,
            family = 'serif') +
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5),
        axis.text = element_text(size = 20))

,

ggplot(p11, aes(y = y, x = x)) +
  labs(title = 'PIB per capta', y = '', x = '') +
  geom_bar(stat = 'identity', fill = c('blueviolet', 'blue')) +
  geom_text(aes(label = round(y,2)), vjust = 1.6, color = 'white', size = 6,
            family = 'serif') +
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5),
        axis.text = element_text(size = 20))

,

ggplot(p12, aes(y = y, x = x)) +
  labs(title = 'Prop. de Evangélicos', y = '', x = '') +
  geom_bar(stat = 'identity', fill = c('blueviolet', 'blue')) +
  geom_text(aes(label = round(y,2)), vjust = 1.6, color = 'white', size = 6,
            family = 'serif') +
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5),
        axis.text = element_text(size = 20))

,

ggplot(p13, aes(y = y, x = x)) +
  labs(title = 'Méd. Anos de Estudo (Pop. + 18 anos)', y = '', x = '') +
  geom_bar(stat = 'identity', fill = c('blueviolet', 'blue')) +
  geom_text(aes(label = round(y,2)), vjust = 1.6, color = 'white', size = 6,
            family = 'serif') +
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5),
        axis.text = element_text(size = 20))

, cols = 2)

## Checando Associação entre VIs e VD
vars_names = c('Tx. Homicídio','Cons. Alcool','Cons. Drogas',
               'Desp. Camp.','Prop. Milit.','Num. Col. Parl.',
               'Pol. 100milhab.','Pref. Margem Vit.',
               'Brancos + Nulos','Prop. Pop. Jovem',
               'Prop. Pop. Brancos','Tx. Cresc. Pop.',
               'Prop. Pop. Evangel.','Anos de Estudo',
               'Pib per capta','Dif. Rendas','Cresc. PIB',
               'Inflação (INPC)','Prop. Serviços')

vars_cod = c('homicidio','alcool','drogas','despesa_seg',
             'prop_seg','n_part_seg','politico_pop',
             'margem_vit','branco_nulo','jovem','branca',
             'tx_pop','prop_evangelico','anos_estudo',
             'pib_pc','renda_dif','tx_pib','servicos')

plot_list = list()
for(i in 1:length(vars_cod)){
  
  v = vars_cod[i]
  dd <- data.frame(y = df$votos_seg,
                   x = df[v])
  
  dd <- dd[!dd$y %in% boxplot.stats(dd$y)$out,]
  
  colnames(dd) <- c('y','x')
  
  titulo = paste(vars_names[i],";", 
                 'r = ', round(cor.test(dd$y,dd$x,method = 'pearson')$estimate,2),
                 'p valor = ', round(cor.test(dd$y,dd$x,method = 'pearson')$p.value,2))
  
  pp = ggplot(data = dd, aes(y = y, x = x)) +
        labs(title = titulo, y = '', x = '') +
        geom_point() +
        geom_smooth(method = lm, se = FALSE) + 
        geom_rangeframe() + 
        theme_tufte(base_size = 18, base_family = 'serif') +
        theme(plot.title = element_text(hjust = + 0.5, size = 14))
      
  plot_list[[i]] <- pp
}

# Plots
multiplot(plotlist = plot_list[1:6], cols = 2)
multiplot(plotlist = plot_list[7:12], cols = 2)
multiplot(plotlist = plot_list[13:18], cols = 2)

## Moldelo Multinível
m1 <- lmer(votos_seg ~ homicidio + alcool + drogas +
             despesa_seg + prop_seg +  partido_seg + n_part_seg + 
             politico_pop + margem_vit + branco_nulo + 
             jovem + branca + tx_pop + prop_evangelico + anos_estudo + 
             pib_pc + renda_dif + tx_pib + tx_inpc + servicos + (1|uf) + 
             (1|reg) + (1|ano), data = bb)

# Checando presença de multicolinearidade
vif(m1)

# Calculando hatvalues para extrair outliers
res <- hatvalues(m1)
res <- ifelse(res > 3 * 20 / nrow(bb),1,0)

# Moldelo Multinível sem outliers
m1n <- lmer(votos_seg ~ homicidio + alcool + drogas +
              despesa_seg + prop_seg +  partido_seg + n_part_seg + 
              politico_pop + margem_vit + branco_nulo + jovem + branca + 
              tx_pop + prop_evangelico + anos_estudo + pib_pc + renda_dif + 
              tx_pib + tx_inpc + servicos + (1|uf) + (1|reg) + (1|ano), 
            data = bb[res == 0,])

# Checando presença de multicolinearidade
vif(m1n)

# Sumário do modelo
summary(m1n)

## Pseudo R2 de Nakagawa & Schielzeth
r.squaredGLMM(m1n)

# Normalidade dos Resíduos
hist(residuals(m1n),
     main = '',
     xlab = 'Resíduos',
     ylab = 'Frequência')

# Resíduals vs Fitted
plot(m1n,
     xlab = 'Previstos',
     ylab = 'Resíduos',
     family = 'serif')

# Obs: Evidências de Heterocedasticidade. Em modelos com apenas 1 nível 
# esse problema é facilmente resolvido adotando-se um estimador de 
# White-Huber dos erros padrão, infelizmente ainda não encontrei uma
# solução para implementar a solução para modelos multiníveis.

# Conjuntos de Variáveis
var_crim = c('homicidio','alcool','drogas')
var_inst = c('despesa_seg','partido_seg','prop_seg','n_part_seg','politico_pop')
var_demg = c('prop_evangelico','branca','jovem','tx_pop')
var_insf = c('branco_nulo','margem_vit')
var_soce = c('anos_estudo','renda_dif','pib_pc','tx_pib','tx_inpc','servicos')

## Plotando Coeficientes
par(mar = c(1,9,5.1,2),family = 'serif')

# Variáveis Criminológicas
coefplot(summary(m1n)$coef[var_crim,1],summary(m1n)$coef[var_crim,2],
         varnames = c('Tx. Homicídio','Cons. Alcool','Cons. Entorpecentes'),
         main = 'Variáveis Criminológicas',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

# Variáveis Institucionais 
coefplot(c(summary(m1n)$coef[var_inst[1],1] / 10,
           summary(m1n)$coef[var_inst[2:3],1],
         summary(m1n)$coef[var_inst[4],1] / 10,
         summary(m1n)$coef[var_inst[5],1]),
         c(summary(m1n)$coef[var_inst[1],2] / 10,
           summary(m1n)$coef[var_inst[2:3],2],
           summary(m1n)$coef[var_inst[4],2] / 10,
           summary(m1n)$coef[var_inst[5],2]),
         varnames = c('Desp. Camp. / 10','Part. Pref.','Prop. Milit.',
                      'Num. Col. Parl. / 10','Pol. 100milhab.'),
         main = 'Variáveis Institucionais',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

# Variáveis Demográficas
coefplot(c(summary(m1n)$coef[var_demg[1:3],1],
           summary(m1n)$coef[var_demg[4],1] / 100),
         c(summary(m1n)$coef[var_demg[1:3],2],
           summary(m1n)$coef[var_demg[4],2] / 100),
         varnames = c('Prop. Pop. Evangel.','Prop. Pop. Brancos','Prop. Pop. Jovem',
                      'Tx. Cresc. Pop. / 100'),
         main = 'Variáveis Demográficas',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

# Variáveis de Insatisfação com a Política
coefplot(summary(m1n)$coef[var_insf,1],summary(m1n)$coef[var_insf,2],
         varnames = c('Brancos + Nulos','Pref. Margem Vit.'),
         main = 'Variáveis Insatisfação Política',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

# Variáveis Socioeconômicas
coefplot(summary(m1n)$coef[var_soce,1],summary(m1n)$coef[var_soce,2],
         varnames = c('Anos de Estudo','Dif. Rendas','Pib per capta',
                      'Cresc. PIB','Inflação (INPC)','Prop. Serviços'),
         main = 'Variáveis Socioeconômicas',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

## Modelo Completo
coef_data <- data.frame(eff = summary(m1n)$coef[2:21,1],
                        sd = summary(m1n)$coef[2:21,2],
                        names = c('Tx. Homicídio','Cons. Alcool','Cons. Drogas',
                                  'Desp. Camp.','Prop. Milit.','Part. Pref.',
                                  'Num. Col. Parl.','Pol. 100milhab.',
                                  'Pref. Margem Vit.','Brancos + Nulos',
                                  'Prop. Pop. Jovem','Prop. Pop. Brancos',
                                  'Tx. Cresc. Pop.','Prop. Pop. Evangel.',
                                  'Anos de Estudo','Pib per capta','Dif. Rendas',
                                  'Cresc. PIB','Inflação (INPC)','Prop. Serviços')) 

coef_data <- coef_data[order(coef_data$eff,decreasing = F),]

coefplot(coef_data$eff,coef_data$sd,
         varnames = as.character(coef_data$names),
         main = 'Modelo Completo',family = 'serif',
         cex.var = 1.2,cex.pts = 1.5)

par(mar = c(5, 4, 4, 2) + 0.1,family = 'serif')

### Usando Descontinuidade para testar efeitos do segundo turno
dd <- tse[c('votos_seg','seg_turno','aptos')]
dd <- na.omit(dd)

## Ponto de Divisão: 200 mil eleitores
max(dd$aptos[dd$seg_turno == 0])
min(dd$aptos[dd$seg_turno == 1])

## Selecionando Casos
dd <- dd[dd$aptos > 185000 & dd$aptos < 215000,]

# Gráfico de Comparação Controle vs Tratamento
p14 <- data.frame(y = c(mean(dd$votos_seg[dd$seg_turno == 0]),
                        mean(dd$votos_seg[dd$seg_turno == 1])),
                  x = c('Sem Segundo Turno','Com Segundo Turno'))

ggplot(p14, aes(y = y, x = x)) +
  labs(title = '', y = '', x = '') +
  geom_bar(stat = 'identity', fill = c('blueviolet', 'blue')) +
  geom_text(aes(label = round(y,2)), vjust = 1.6, color = 'white', size = 6,
            family = 'serif') +
  geom_rangeframe() + 
  theme_tufte(base_size = 18, base_family = 'serif') +
  theme(plot.title = element_text(hjust = + 0.5),
        axis.text = element_text(size = 22))

# Boxplot Controle vs Tratamento
boxplot(dd$votos_seg ~ dd$seg_turno,
        names = c('Sem Segundo Turno','Com Segundo Turno'))

### Testando a Hipótese da Percepção de Corrupção

## Data Época (Manifestações de 2015)
man <- read.table('novo_dissertacao/data/final_manifestacao.txt', 
                  sep = ';',fileEncoding = 'utf-8')

man$cod_mun <- man$cod_municipio_tse

ee <- tse[tse$ano == 2016,]

ee <- merge(ee,man, by = 'cod_mun')

ee <- merge(ee,data.frame(uf = levels(as.factor(ipea$uf)),
                          pop_uf = tapply(ipea$populacao,ipea$uf,
                                          sum, na.rm = T)),
            by = 'uf')
            
ee$manifestacao <- ee$manifestacao / ee$pop_uf

ee <- data.frame(voto_mil = tapply(ee$votos_seg, ee$uf, mean, na.rm = T),
                 manifest = tapply(ee$manifestacao, ee$uf, mean, na.rm = T))


## Excluindo DF (sem dados) e CE (outlier).
ee <- ee[-c(6,7),]

## Gerando Gráfico X vs Y
plot(ee$voto_mil ~ ee$manifest,
     ylab = 'Desempenho Eleitoral de Militaristas',
     xlab = 'Prop. Participação nas Manifestações',
     cex.lab = 1.6)

abline(lm(ee$voto_mil ~ ee$manifest), 
       col = 'red', lwd = 2)

cor.test(ee$voto_mil, ee$manifest)
