#Chamando as bibliotecas
library(ggthemes)
library(dplyr)
library(stringr)
library(tidyverse)
library (readr)
library(reshape2)
library(effsize)
library(car)

#-------------------------------Pré-processamento-------------------------------

#Importa os dados
diabetes <- read.csv("diabetic_data.csv", header=TRUE, sep = ",")

# Exibição inicial
glimpse(diabetes)
cat("Dimensões: ", dim(diabetes), "\n")
cat("NAs por coluna:\n")
print(colSums(is.na(diabetes)))

# Padronização e limpeza

diabetes_clean <- diabetes %>%
  filter(gender %in% c("Male", "Female")) %>%
  mutate(
    gender = factor(gender),
    readmitted = factor(readmitted, levels = c("NO", "<30", ">30")),
    age = factor(age, ordered = TRUE)
  )

# ------------------------------- Análise Exploratória -------------------------

# 1) Distribuição de tempo no hospital
ggplot(diabetes_clean, aes(time_in_hospital)) +
  geom_histogram(binwidth = 1, fill = "#69b3a2", color = "black") +
  labs(
    title = "Tempo no Hospital (dias)", x = "Dias", y = "Frequência") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 2) Contagem de readmissão
ggplot(diabetes_clean, aes(readmitted, fill = readmitted)) +
  geom_bar() +
  labs(title = "Status de Readmissão", x = "Readmissão", y = "Contagem") +
  theme_minimal() + theme(legend.position = "none")+
  theme(plot.title = element_text(hjust = 0.5))

# 3) Histograma de quantidade de medicamentos preescristos para o paciente
ggplot(diabetes_clean, aes(num_medications)) +
  geom_histogram(binwidth = 1, fill = "#4C72B0", color = "black") +
  labs( title = "Distribuição de Medicamentos Prescritos", x = "Número de Medicações",
    y = "Frequência") + theme_minimal() + theme(plot.title = element_text(hjust = 0.5))


# 4) Boxplot de tempo de internação por faixa etária
ggplot(diabetes_clean, aes(age, time_in_hospital)) +
  geom_boxplot() +
  labs(title = "Tempo de Internação por Faixa Etária", x = "Faixa etária",
  y = "Dias no Hospital") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))

# 5) Uso de insulina: proporção de pacientes por categoria

ggplot(diabetes_clean, aes(x = insulin, fill = insulin)) +
  scale_x_discrete(labels = c(
    "No"     = "Não usa",
    "Up"     = "Uso crescente",
    "Steady" = "Uso constante",
    "Down"   = "Uso decrescente"
  )) +
  scale_fill_discrete(
    name   = "Insulina",
    labels = c(
      "No"     = "Não usa",
      "Up"     = "Uso crescente",
      "Steady" = "Uso constante",
      "Down"   = "Uso decrescente"
    )
  ) +
  geom_bar() +
  labs(
    title = "Uso da insulina",
    x     = "Insulina",
    y     = "Quantidade de pacientes"
  ) +
  theme_minimal() +
  theme(
    legend.position   = "right",
    plot.title        = element_text(hjust = 0.5)
  )


# 6) Perfil de medicamentos: top 5 mais prescritos
drug_list <- c("metformin", "glipizide", "glyburide", "insulin", "pioglitazone")

med_counts <- diabetes_clean %>%
  select(all_of(drug_list)) %>%
  pivot_longer(
    cols      = all_of(drug_list),
    names_to  = "drug",
    values_to = "use"
  ) %>%
  filter(use != "No") %>%    
  count(drug, name = "count") 

# Plota o top 5
ggplot(med_counts, aes(x = reorder(drug, count), y = count, fill = drug)) +
  geom_col() +
  coord_flip() +
  scale_x_discrete(labels = c(
    "metformin"   = "Metformina",
    "glipizide"   = "Glipizida",
    "glyburide"   = "Gliburida",
    "insulin"     = "Insulina",
    "pioglitazone"= "Pioglitazona"
  )) +
  labs(
    title = "Top 5 Medicamentos Mais Usados (incluindo Insulina)",
    x     = "Medicamento",
    y     = "Número de Pacientes"
  ) +
  theme_minimal() +
  theme(
    legend.position     = "none",
    plot.title          = element_text(hjust = 0.5)
  )

# 7) Uso de top 5 medicamentos por readmissão (percentual)
# Prepara dados long format para os top 5 fármacos
drug_list <- c("metformin", "glipizide", "glyburide", "insulin", "pioglitazone")

med_long <- diabetes_clean %>%
  select(readmitted, all_of(drug_list)) %>%
  pivot_longer(
    cols = all_of(drug_list),
    names_to = "drug",
    values_to = "use"
  ) %>%
  mutate(
    use = ifelse(use == "No", "Não usa", "Usa"),
    drug = dplyr::recode(drug,
                         "metformin" = "Metformina",
                         "glipizide" = "Glipizida",
                         "glyburide" = "Gliburida",
                         "insulin" = "Insulina",
                         "pioglitazone" = "Pioglitazona"
    )
  )

ggplot(med_long, aes(x = readmitted, fill = use)) +
  geom_bar(position = "fill") +
  facet_wrap(~ drug) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proporção de Uso de Medicamentos por Readmissão (Top 5)",
    x = "Readmissão",
    y = "Percentual",
    fill = "Uso do medicamento"
  ) +
  theme_minimal()


# 8) Lab procedures vs num_medications colorido por readmissão
ggplot(diabetes_clean, aes(x = num_lab_procedures, y = num_medications)) +
  geom_density_2d_filled(contour_var = "ndensity") + # Cria um heatmap de densidade
  facet_wrap(~ readmitted) +
  labs(
    title = "Concentração de Pacientes: Procedimentos vs. Medicações",
    subtitle = "Separado por Status de Readmissão",
    x = "Número de Procedimentos de Laboratório",
    y = "Número de Medicações",
    fill = "Densidade"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# 9) Matriz de correlação binária de uso de medicamentos
med_bin <- diabetes_clean %>%
  select(all_of(drug_list)) %>%
  mutate_all(~ as.integer(. != "No"))
corr_mat <- cor(med_bin)
corr_df <- melt(corr_mat)

corr_df_traduzido <- corr_df %>%
  mutate(
    Var1 = dplyr::recode(Var1,
                         "metformin" = "Metformina",
                         "glipizide" = "Glipizida",
                         "glyburide" = "Gliburida",
                         "insulin" = "Insulina",
                         "pioglitazone" = "Pioglitazona"),
    Var2 = dplyr::recode(Var2,
                         "metformin" = "Metformina",
                         "glipizide" = "Glipizida",
                         "glyburide" = "Gliburida",
                         "insulin" = "Insulina",
                         "pioglitazone" = "Pioglitazona")
  )
ggplot(corr_df_traduzido, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Correlação de Uso Binário de Top 5 Medicamentos",
    x = "", 
    y = "",
    fill = "Valor"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------------------- Análise de Hipóteses -------------------------------
# ------------------------------- Análise de Hipóteses -------------------------------
## Hipótese 1 (Numérica): Mulheres vs Homens no Tempo de Internação
alpha <- 0.05

# 2) Visualização das distribuições (densidade)
plt_dens <- ggplot(diabetes_clean, aes(x = time_in_hospital, fill = gender)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidade: Tempo de Internação por Gênero", x = "Dias no Hospital") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(plt_dens)

# 3) Pressupostos
#   a) Normalidade (Shapiro) — subamostrando 5.000 casos para cada gênero
set.seed(123)  # para reprodutibilidade
sample_f_all <- diabetes_clean %>% filter(gender == "Female") %>% pull(time_in_hospital)
sample_m_all <- diabetes_clean %>% filter(gender == "Male")   %>% pull(time_in_hospital)

sample_f <- sample(sample_f_all, 5000)
sample_m <- sample(sample_m_all, 5000)

sh_f <- shapiro.test(sample_f)
sh_m <- shapiro.test(sample_m)
print(sh_f)
print(sh_m)

#   b) Homogeneidade de variância (Levene)
lev <- leveneTest(time_in_hospital ~ gender, data = diabetes_clean)
print(lev)

# 4) Teste Estatístico (Welch t-test)
t1 <- t.test(time_in_hospital ~ gender, data = diabetes_clean)
print(t1)

# 5) Tamanho de Efeito (Cohen's d)
d_cohen <- cohen.d(time_in_hospital ~ gender, data = diabetes_clean)
print(d_cohen)

# 6) Conclusão
cat(ifelse(t1$p.value < alpha,
           "Rejeita H0: diferença significativa.\n",
           "Falha em rejeitar H0.\n"))

## Hipótese 2 (Categórica): Associação entre Gênero e Readmissão
alpha <- 0.05

# 1) Formulação
#    H0: Gênero e Readmissão são independentes
#    H1: Há associação entre Gênero e Readmissão

# 2) Visualização (gráfico de barras percentuais)
ggplot(diabetes_clean, aes(x = gender, fill = readmitted)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proporção de Readmissão por Gênero",
    x     = "Gênero",
    y     = "Percentual",
    fill  = "Readmissão"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# 3) Pressupostos
#    a) Frequências esperadas >= 5 em cada célula da tabela de contingência
tbl <- table(diabetes_clean$gender, diabetes_clean$readmitted)
expected <- chisq.test(tbl, simulate.p.value = FALSE)$expected
print(expected)  # confira se todos >= 5

# 4) Teste Estatístico (Chi-squared)
chi <- chisq.test(tbl)
print(chi)

# 5) Tamanho de Efeito (Cramer's V)
cramer_v <- sqrt(chi$statistic / (sum(tbl) * (min(dim(tbl)) - 1)))
cat("Cramer's V =", round(cramer_v, 3), "\n")

# 6) Conclusão
if (chi$p.value < alpha) {
  cat("Rejeita H0: há associação entre Gênero e Readmissão.\n")
} else {
  cat("Falha em rejeitar H0: não há evidência de associação.\n")
}

# ------------------------------- Fim da Análise de Hipóteses -------------------------------
