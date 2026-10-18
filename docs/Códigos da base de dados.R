# ------------------------------------------------------------------------------------ #
# PACOTES NECESSÁRIOS
require(openxlsx)            # Leitura de base de dados
require(dplyr)               # Manipulação de base de dados
require(gtsummary)           # Tabelas automáticas
require(gt)                  # Tabelas automáticas
require(rstatix)             # Coeficiente de Cramer
require(ggplot2)             # Gráficos
require(qqplotr)             # Gráficos qqplot
require(DescTools)           # Teste de Levene

# ------------------------------------------------------------------------------------ #
# MANIPULAÇÃO DA BASE DE DADOS

Dados = read.xlsx("Nutrição.xlsx")
Dados = Dados |>
  filter(!Genero %in% c("Prefiro não responder", "Outro"))
Dados <- Dados |>
  mutate(Raca_cor = case_when(
    Raca_cor %in% c("Amarela", "Indígena", "Outro", "Prefiro não declarar") ~ "Outros",
    TRUE ~ as.character(Raca_cor)
  ))
Dados = Dados |>
  filter(!EscolarIdade %in% c("Analfabeto", "Ensino Fundamental Completo",
                              "Não desejo informar"))

# ------------------------------------------------------------------------------------ #
# TABELAS RESUMO

DadosQuali = Dados |> select(Planejamento_Cardapio,
                              Genero, Raca_cor, Regiao,
                              Isolamento, Trabalha, Renda_familiar,
                              EscolarIdade, Covid, Consulta_Nutricionista,
                              Dificuldade_Financeira,cigarro,Ansiedade,
                              Depressao,Insonia,Enxaqueca)

DadosQuanti = Dados |> 
  select(Planejamento_Cardapio,Altura,Idade,Peso, IMC)

# ------------------------------------------------------------------------------------ #
# TABELA 1 COMPLETA

comparacoes_letras <- function(data, variable, by, ...) {
  letras <- data.frame(
    variavel = c("Idade", "Peso",
                 "Altura", "IMC"),
    comparacao = c(
      "Aumentou e não tenho o hábito,<br>Não alterou e não tenho o hábito",
      "Aumentou e diminuiu,<br>Diminuiu e não alterou",
      "---",
      "Aumentou e diminuiu,<br>Diminuiu e não alterou"
    ),
    stringsAsFactors = FALSE  
  )
  result <- letras$comparacao[letras$variavel == variable]
  tibble::tibble(`**Comparações Múltiplas**` = result)
}

tab1 = tbl_summary(data = DadosQuanti,
                   by = Planejamento_Cardapio,
                   statistic = all_continuous() ~ "{mean} ({sd})",
                   label = list(
                     Idade ~ "Idade<sup>W</sup>",
                     Peso ~ "Peso<sup>W</sup>",
                     Altura ~ "Altura<sup>A</sup>",
                     IMC ~"IMC<sup>W</sup>")
)|>
  add_p(
    test = everything() ~ "oneway.test",
    test.args = list(
      Altura ~ list(var.equal = T),   # Variâncias iguais (Anova tradicional)
      c(Peso,Idade,IMC) ~ list(var.equal = F)  # Variâncias diferentes (Anova de Welch)
    ), 
    pvalue_fun = label_style_pvalue(digits = 3)
  )|>
  bold_p(t = 0.05) |>
  modify_spanning_header(all_stat_cols() ~ "**Planejamento do Cardápio**") |>
  modify_header(label ~ "**Variáveis**") |>
  bold_labels() |>
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)")|>
  modify_footnote(everything() ~ NA)|>
  add_stat(
    fns = everything() ~ comparacoes_letras,
    location = everything() ~ "label"
  ) |>
  as_gt()  |> 
  fmt_markdown(columns = c(label, `**Comparações Múltiplas**`)) |>
  tab_options(
    table.font.size = "20px",
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )
# ------------------------------------------------------------------------------------ #
# TABELA 2 COMPLETA

cramer_fun <- function(data, variable, by, ...) {
  tab <- table(data[[variable]], data[[by]])
  v <- cramer_v(tab)
  tibble::tibble(`**Cramér**` = round(v, 3))
}

tab2 = tbl_summary(data=DadosQuali,
            by=Planejamento_Cardapio,
            percent = "row",
            statistic = all_continuous() ~ "{mean} ({sd})",
            label = list(
              Genero ~ "Gênero<sup>Q</sup>",
              Raca_cor ~ "Raça ou cor<sup>Q</sup>",
              Regiao ~ "Região<sup>Q</sup>",
              Isolamento ~ "Isolamento<sup>Q</sup>",
              Trabalha ~ "Trabalha<sup>Q</sup>",
              Renda_familiar ~ "Renda familiar<sup>Q</sup>",
              Escolaridade ~ "Escolaridade<sup>Q</sup>",
              Covid ~ "Covid<sup>Q</sup>",
              Consulta_Nutricionista ~ "Consulta ao nutricionista<sup>Q</sup>",
              Dificuldade_Financeira ~ "Dificuldade financeira<sup>Q</sup>",
              cigarro ~ "Cigarro<sup>Q</sup>",
              Ansiedade ~ "Ansiedade<sup>Q</sup>",
              Depressao ~ "Depressão<sup>Q</sup>",
              Insonia ~ "Insônia<sup>Q</sup>",
              Enxaqueca ~ "Enxaqueca<sup>Q</sup>"
            )) |> 
  add_p(pvalue_fun = label_style_pvalue(digits = 3)) |> 
  bold_p(t = 0.05) |> 
  add_stat(fns = everything() ~ cramer_fun) |> 
  modify_spanning_header(all_stat_cols() ~ "**Planejamento com o Cardápio**") |> 
  modify_header(label ~ "**Variáveis**") |> 
  bold_labels() |> 
  modify_header(all_stat_cols() ~ "**{level}**<br>{n} ({style_percent(p)}%)") |> 
  modify_bold(
    columns = stat_1,  # primeira coluna
    rows = (
      (variable == "Regiao" & label %in% c("Norte", "Sul"))|
        (variable == "Isolamento" & label %in% c("Não", "Sim")) |
        (variable == "Renda_familiar" & label  == "até R$ 1254,00") |
        (variable == "Escolaridade" & label %in% c("Ensino Médio Completo","Pós-graduação")) |
        (variable == "Consulta_Nutricionista" & label %in% c("Não", "Sim"))
    )) |> 
  modify_bold(
    columns = stat_2, # segunda coluna
    rows = (
      (variable == "Isolamento" & label %in% c("Não", "Sim")) | 
        (variable == "Renda_familiar" & label  == "até R$ 1254,00") |
        (variable == "Escolaridade" & label %in% c("Ensino Superior Completo", "Pós-graduação")) |
        (variable == "Dificuldade_Financeira" & label %in% c("Não", "Sim")) |
        (variable == "Ansiedade" & label %in% c("Não", "Sim")) |
        (variable == "Depressao" & label %in% c("Não", "Sim")) |
        (variable == "Insonia" & label %in% c("Não", "Sim"))
    )) |> 
  modify_bold(
    columns = stat_3,  # terceira coluna
    rows = (
      (variable == "Regiao" & label == "Norte") | 
        (variable == "Isolamento" & label %in% c("Não", "Sim")) |
        (variable == "Renda_familiar" & label == ("até R$ 1254,00")) |
        (variable == "Consulta_Nutricionista" & label %in% c("Não", "Sim")) |
        (variable == "Dificuldade_Financeira" & label %in% c("Não", "Sim")) |
        (variable == "Ansiedade" & label %in% c("Não","Sim")) |
        (variable == "Depressao" & label %in% c("Não", "Sim")) |
        (variable == "Insonia" & label %in% c("Não", "Sim")) |
        (variable == "Enxaqueca" & label %in% c("Não", "Sim"))
    ))|> 
  modify_bold(
    columns = stat_4,  # quarta coluna
    rows = (
      (variable == "Escolaridade" & label %in% c("Ensino Médio Completo", "Pós-graduação")) |
        (variable == "Depressao" & label %in% c("Não", "Sim"))
    )) |> 
  modify_table_body(
    ~ .x |>
      mutate(
        label = ifelse(
          variable == "Renda_familiar" &
            label == "entre R$ 1.255 - R$ 8.640",
          "entre 1255 a R$ 8640",
          label
        )
      )
  ) |> 
  modify_footnote(everything() ~ NA)|>
  as_gt() |>                   
  fmt_markdown(columns = label) |>
  tab_options(
    table.font.size = "20px",    
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )
  

# ------------------------------------------------------------------------------------ #
# JUNÇÃO DAS DUAS TABELAS

tbl_stack(
  list(tab1, tab2)
) |>
  modify_spanning_header(
    all_stat_cols() ~ "**Planejamento do Cardápio**"
  ) |>
  as_gt() |>
  fmt_markdown(columns = c(label, `**Comparações Múltiplas**`)) |>
  tab_options(
    table.font.size = "20px",
    heading.title.font.size = "26px",
    column_labels.font.size = "22px"
  )

# ------------------------------------------------------------------------------------ #
# CHECAR NORMALIdade
# De DadosQuanti
# Idade

ggplot(DadosQuanti, aes(x = Idade)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Planejamento_Cardapio) +
  xlab("Idade") +
  ylab("DensIdade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(DadosQuanti, aes(sample = Idade)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Planejamento_Cardapio,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(DadosQuanti$Idade, DadosQuanti$Planejamento_Cardapio, shapiro.test)
# Rejeita a hipótese de normalIdade em todos os aspectos

LeveneTest(formula = Idade~Planejamento_Cardapio,data = DadosQuanti)
# Há evidências de que as variâncias são diferentes

games_howell_test(Idade~Planejamento_Cardapio,
                  data=DadosQuanti)
# Aumentou e não tem o hábito
# Não alterou e não tem o hábito
# Foram significativamente diferentes

# IMC

ggplot(DadosQuanti, aes(x = IMC)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Planejamento_Cardapio) +
  xlab("IMC") +
  ylab("DensIMC") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(DadosQuanti, aes(sample = IMC)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Planejamento_Cardapio,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(DadosQuanti$IMC, DadosQuanti$Planejamento_Cardapio, shapiro.test)
# Rejeita a hipótese de normalIdade em todos os aspectos

LeveneTest(formula = IMC~Planejamento_Cardapio,data = DadosQuanti)
# Há evidências de que as variâncias são diferentes

games_howell_test(IMC~Planejamento_Cardapio,
                  data=DadosQuanti)
# Aumentou e Diminuiu
# Diminuiu e não alterou
# Foram significativamente diferentes

# Peso

ggplot(DadosQuanti, aes(x = Peso)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Planejamento_Cardapio) +
  xlab("Peso") +
  ylab("DensIdade") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(DadosQuanti, aes(sample = Peso)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Planejamento_Cardapio,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(DadosQuanti$Peso, DadosQuanti$Planejamento_Cardapio, shapiro.test)
# Rejeita a hipótese de normalIdade em todos os aspectos

LeveneTest(formula = Peso~Planejamento_Cardapio,data = DadosQuanti)
# Há evidências de que as variâncias são diferentes

games_howell_test(Peso~Planejamento_Cardapio,
                  data=DadosQuanti)
# Aumentou e Diminuiu
# Diminuiu e não alterou
# Foram significativamente diferentes

# Altura

ggplot(DadosQuanti, aes(x = Altura)) +
  geom_density(fill = "blue", alpha = 0.6) +
  facet_wrap(~ Planejamento_Cardapio) +
  xlab("Altura") +
  ylab("DensAltura") +
  theme_bw() +
  theme(text = element_text(size = 14))

ggplot(DadosQuanti, aes(sample = Altura)) +
  stat_qq_band(fill="lightblue") +
  stat_qq_line() +
  stat_qq_point() +
  facet_wrap(~Planejamento_Cardapio,scales = "free")+
  xlab("Quantis Teoricos")+
  ylab("Quantis Amostrais")+
  theme_bw() +
  theme(text = element_text(size = 14))

by(DadosQuanti$Altura, DadosQuanti$Planejamento_Cardapio, shapiro.test)
# Rejeita a hipótese de normalidade em quase todos os aspectos

LeveneTest(formula = Altura~Planejamento_Cardapio,data = DadosQuanti)
# Há evidências de que as variâncias são iguais

modelo = aov(Altura ~ Planejamento_Cardapio, data = DadosQuanti)
TukeyHSD(modelo)
# Nenhum teve nada significativo

# ------------------------------------------------------------------------------------- #
# INVESTIGANDO ASSOCIAÇÕES DA TAB2 

Genero = chisq.test(x=DadosQuali$Genero,y=DadosQuali$Planejamento_Cardapio)
Genero$stdres
# Aumentou e não alterou

Raca_cor = chisq.test(x=DadosQuali$Raca_cor,y=DadosQuali$Planejamento_Cardapio)
Raca_cor$stdres

Regiao = chisq.test(x=DadosQuali$Regiao,y=DadosQuali$Planejamento_Cardapio)
Regiao$stdres
# Aumentou e norte
# Aumentou e sul
# Não alterou e norte

Isolamento = chisq.test(x=DadosQuali$Isolamento,y=DadosQuali$Planejamento_Cardapio)
Isolamento$stdres
# Aumentou, diminuiu e não alterou

Trabalha = chisq.test(x=DadosQuali$Trabalha,y=DadosQuali$Planejamento_Cardapio)
Trabalha$stdres

Renda_familiar = chisq.test(x=DadosQuali$Renda_familiar,y=DadosQuali$Planejamento_Cardapio)
Renda_familiar$stdres
# Aumentou e 1254, diminuiu e 1254, não alterou e 1254

Escolaridade = chisq.test(x=DadosQuali$Escolaridade,y=DadosQuali$Planejamento_Cardapio)
Escolaridade$stdres
# Aumentou e EM Completo
# Diminuiu e ES Completo
# Pos graduação e aumentou e diminuiu
# Não tenho o hábito e EM Completo
# Não tenho o hábito e Pós graduação

Covid = chisq.test(x=DadosQuali$Covid,y=DadosQuali$Planejamento_Cardapio)
Covid$stdres

Consulta_Nutricionista = chisq.test(x=DadosQuali$Consulta_Nutricionista,y=DadosQuali$Planejamento_Cardapio)
Consulta_Nutricionista$stdres
# Aumentou
# não tenho o hábito

Dificuldade_Financeira = chisq.test(x=DadosQuali$Dificuldade_Financeira,y=DadosQuali$Planejamento_Cardapio)
Dificuldade_Financeira$stdres
# Diminuiu
# Não alterou

cigarro = chisq.test(x=DadosQuali$cigarro,y=DadosQuali$Planejamento_Cardapio)
cigarro$stdres

Ansiedade = chisq.test(x=DadosQuali$Ansiedade,y=DadosQuali$Planejamento_Cardapio)
Ansiedade$stdres
# Diminuiu
# Não alterou

Depressao = chisq.test(x=DadosQuali$Depressao,y=DadosQuali$Planejamento_Cardapio)
Depressao$stdres
# Diminuiu
# Não alterou
# Não tenho o hábito

Insonia = chisq.test(x=DadosQuali$Insonia,y=DadosQuali$Planejamento_Cardapio)
Insonia$stdres
# Diminuiu
# Não alterou

Enxaqueca = chisq.test(x=DadosQuali$Enxaqueca,y=DadosQuali$Planejamento_Cardapio)
Enxaqueca$stdres
# Não alterou
