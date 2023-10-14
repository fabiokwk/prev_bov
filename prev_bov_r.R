dados <- read.csv('C:/Users/fabio/Desktop/dsbd/antifragil/prev_bov/prev_bov.csv')
# Explorar dados ------------------------------------------------------------------------------------
head(dados)
tail(dados)
str(dados)
# resumo estatístico 
summary(dados)
# verificar se há dados ausentes 
colSums(is.na(dados))
# Modelos ------------------------------------------------------------------------------------
# com todos os preditores
lm_bovespa_todos <- lm(bovespa ~ . -Date, dados)
summary(lm_bovespa_todos)
# sem nikkei 
lm_bovespa_1 <- lm(bovespa ~ . -Date-nikkei, dados)
summary(lm_bovespa_1)
### Residual standard error: 8643 on 3909 degrees of freedom
### Multiple R-squared:  0.892,	Adjusted R-squared:  0.8919 
### F-statistic:  5384 on 6 and 3909 DF,  p-value: < 2.2e-16
# visualização 
par(mfrow=c(2,2))
plot(lm_bovespa_1)

# sinergia newyork:sp500 dolar:nasdaq
lm_bovespa_2 <- lm(bovespa ~ newyork*sp500  + dolar*nasdaq + xangai + londres, dados)
summary(lm_bovespa_2)
### Residual standard error: 7906 on 3907 degrees of freedom
### Multiple R-squared:  0.9097,	Adjusted R-squared:  0.9095 
### F-statistic:  4922 on 8 and 3907 DF,  p-value: < 2.2e-16
# visualização
plot(lm_bovespa_2)

# sinergia xangai + londres + dolar*sp500  + newyork*nasdaq
lm_bovespa_3 <- lm(bovespa ~ + xangai + londres + dolar*sp500  + newyork*nasdaq, dados)
summary(lm_bovespa_3)
### Residual standard error: 7455 on 3907 degrees of freedom
### Multiple R-squared:  0.9197,	Adjusted R-squared:  0.9196 
### F-statistic:  5596 on 8 and 3907 DF,  p-value: < 2.2e-16

# sinergia + londres + xangai:londres + sp500*log(dolar) + newyork:nasdaq
lm_bovespa_4 <- lm(bovespa ~ + log(newyork )
                   + londres 
                   + xangai:londres 
                   + sp500*log(dolar) 
                   + newyork:nasdaq, dados)
summary(lm_bovespa_4)
### Residual standard error: 6878 on 3908 degrees of freedom
### Multiple R-squared:  0.9317,	Adjusted R-squared:  0.9315 
### F-statistic:  7612 on 7 and 3908 DF,  p-value: < 2.2e-16
# visualização
plot(lm_bovespa_4)

# Preditor
# 11/10/2023 117.051
onze_out <- data.frame(
  newyork = 15450.24,
  londres = 7628.2,
  xangai = 17956.83,
  sp500 = 4366.59,
  dolar = 5.05,
  nasdaq = 13619.21
)

# Fazer previsões usando o modelo
prev_onze_out <- predict(lm_bovespa_4, newdata = onze_out)
prev_onze_out

# 11/10/2023 117.051
treze_out <- data.frame(
  newyork = 15329.55,
  londres = 7644.8,
  xangai = 17946.77,
  sp500 = 4360.49,
  dolar = 5.05,
  nasdaq = 13613.59
)

# Fazer previsões usando o modelo
prev_treze_out <- predict(lm_bovespa_4, newdata = treze_out)
prev_treze_out
