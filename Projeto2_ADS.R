#Chamando as bibliotecas
library(ggthemes)
library(dplyr)
library(stringr)
library(tidyverse)
library(tidyr)
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
# --------------------------------------------------------------------------------
# HIPÓTESE 1: Diferença no tempo de internação entre Mulheres e Homens
# --------------------------------------------------------------------------------

# 1) Hipóteses
# H0: μ_female == μ_male
# H1: μ_female != μ_male

alpha <- 0.05

# 2) Visualização: Histograma + curva normal ajustada por gênero
ggplot(diabetes_clean, aes(x = time_in_hospital)) +
  geom_histogram(aes(y = ..density..), bins = 30,
                 fill = "lightgray", color = "black") +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(diabetes_clean %>% filter(gender == "Female") %>% pull(time_in_hospital), na.rm = TRUE),
      sd   = sd(diabetes_clean %>% filter(gender == "Female") %>% pull(time_in_hospital), na.rm = TRUE)
    ),
    color = "red", size = 1,
    inherit.aes = FALSE
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(diabetes_clean %>% filter(gender == "Male") %>% pull(time_in_hospital), na.rm = TRUE),
      sd   = sd(diabetes_clean %>% filter(gender == "Male") %>% pull(time_in_hospital), na.rm = TRUE)
    ),
    color = "blue", size = 1,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ gender) +
  labs(
    title = "Distribuição do Tempo de Internação com Curva Normal Ajustada",
    x = "Dias no Hospital",
    y = "Densidade"
  ) +
  theme_minimal()

# 3) Teste estatístico: Welch t-test
t1 <- t.test(time_in_hospital ~ gender, data = diabetes_clean, var.equal = FALSE)


# 4) Conclusão
print(t1)

if (t1$p.value < alpha) {
  message("Rejeita H0: diferença significativa entre médias (p = ",
          signif(t1$p.value, 3), ").")
} else {
  message("Falha em rejeitar H0: não houve diferença significativa (p = ",
          signif(t1$p.value, 3), ").")
}

# --------------------------------------------------------------------------------
# HIPÓTESE 2: Associação entre Gênero e Readmissão
# --------------------------------------------------------------------------------
# 1) Hipóteses
# H0: Gênero e Readmissão são independentes
# H1: Existe associação entre Gênero e Readmissão

# 2) Visualização: Curvas normais ajustadas por readmissão

stats <- diabetes_clean %>%
  group_by(readmitted) %>%
  summarize(
    mu = mean(time_in_hospital, na.rm = TRUE),
    sigma = sd(time_in_hospital, na.rm = TRUE)
  )

x_vals <- seq(
  from = min(diabetes_clean$time_in_hospital, na.rm = TRUE),
  to   = max(diabetes_clean$time_in_hospital, na.rm = TRUE),
  length.out = 500
)

curvas <- stats %>%
  crossing(x = x_vals) %>%
  mutate(y = dnorm(x, mean = mu, sd = sigma))

ggplot() +
  geom_density(aes(x = time_in_hospital, fill = readmitted),
               data = diabetes_clean, alpha = 0.3) +
  geom_line(aes(x = x, y = y, color = readmitted),
            data = curvas, size = 1) +
  labs(
    title = "Tempo de Internação com Curvas Normais por Status de Readmissão",
    x = "Dias no Hospital",
    y = "Densidade"
  ) +
  theme_minimal()

# 3) Teste estatístico: Qui-quadrado
tbl <- table(diabetes_clean$gender, diabetes_clean$readmitted)
chi <- chisq.test(tbl)


# 4) Conclusão
if (chi$p.value < alpha) {
  message("Rejeita H0: há associação entre Gênero e Readmissão (p = ",
          signif(chi$p.value, 3), ").")
} else {
  message("Falha em rejeitar H0: sem evidência de associação (p = ",
          signif(chi$p.value, 3), ").")
}

# ------------------------------- Fim da Análise de Hipóteses -------------------------------
