# Exercicio3.R
## Alunos: Davi Rodrigues Chiqueti, Michelli Patricia Luvison, Renan dos Santos Silva
### 2024-04-19

# Leitura dos dados
dados <- read.csv2("mobile.csv", dec=".")

# Seleciona uma amostra ao acaso com 600 observações
library(dplyr)
set.seed(123) # Garante a reprodutibilidade da amostra
amostra <- dados %>% sample_n(600) 

# Questão 1: Calcule a média, desvio padrão e coeficiente de variação para as seguintes variáveis:
## A: battery_power em função de touch_screen
bateria_total <- amostra %>% 
  group_by(touch_screen) %>% 
  summarise(
    media = mean(battery_power),
    desvio_padrao = sd(battery_power),
    coeficiente_variacao = (sd(battery_power) / mean(battery_power)) * 100
  )
# Conclusão:
## Podemos perceber que ainda que os smarthphones tenham evoluído muito ao longo dos anos,
## com o touchscreen e outras tecnologias, a capacidade média da bateria continua igual.
## Demonstrando que o consumo de energia não é ponto de foco que as empresas buscam aprimorar.

# B: m_dep em função de touch_screen
profundidade <- amostra %>% 
  group_by(touch_screen) %>% 
  summarise(
    media = mean(m_dep),
    desvio_padrao = sd(m_dep),
    coef_variacao = (sd(m_dep) / mean(m_dep)) * 100
  )
profundidade
# Conclusão:
## Dispositivos com tela sensível ao toque têm uma média de profundidade ligeiramente maior em comparação com os dispositivos sem tela sensível ao toque.
## Além de terem um desvio padrão menor, ou seja, menos valores discrepantes.

# C: int_memory em função de blue
memoria_interna <- amostra %>% 
  group_by(blue) %>% 
  summarise(
    media = mean(int_memory),
    desvio_padrao = sd(int_memory),
    coef_variacao = (sd(int_memory) / mean(int_memory)) * 100
  )
memoria_interna
# Conclusão:
## Dispositivos com Bluetooth têm uma média de memória interna ligeiramente menor em comparação com dispositivos sem Bluetooth.
## O desvio padrão da memória interna para ambos os grupos é relativamente próximo.
## O coeficiente de variação indica que a dispersão em relação à média é um pouco maior para dispositivos com Bluetooth.

library(ggplot2)
# Gráfico do item 1.a
ggplot(amostra, aes(x = as.factor(touch_screen), y = battery_power)) +
  geom_boxplot() +
  labs(title = "Boxplot relacionando a vida da bateria por tela tocável", x = "Tela tocável", y = "Vida da bateria")

# Gráfico do item 1.b
ggplot(amostra, aes(x = as.factor(touch_screen), y = m_dep)) +
  geom_boxplot() +
  labs(title = "Boxplot relacionando a profundidade por tela tocável", x = "Touch Screen", y = "Mobile Depth")

# Gráfico do item 1.c
ggplot(amostra, aes(x = as.factor(blue), y = int_memory)) +
  geom_boxplot() +
  labs(title = "Boxplot relacionando a memória interna por bluetooth", x = "Bluetooth", y = "Internal Memory")

