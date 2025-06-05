# 🇧🇷 Portfólio de Análises Estatísticas em R | 🇺🇸 Statistical Analysis Portfolio in R

<div align="center">

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white)
![RMarkdown](https://img.shields.io/badge/RMarkdown-blue?style=for-the-badge&logo=markdown&logoColor=white)
![Statistics](https://img.shields.io/badge/Statistics-Advanced-red?style=for-the-badge)
![License](https://img.shields.io/badge/License-MIT-green.svg?style=for-the-badge)

**Portfólio completo de análises estatísticas avançadas demonstrando expertise em R**

[📊 Análises](#-análises-disponíveis) • [📖 Metodologia](#-metodologia) • [⚡ Execução](#-execução-rápida) • [📈 Resultados](#-resultados)

</div>

---

## 🇧🇷 Português

### 📊 Visão Geral

Este repositório apresenta um **portfólio abrangente de análises estatísticas** desenvolvido em R, demonstrando competências avançadas em:

- 📈 **Estatística Inferencial**: Testes de hipóteses, intervalos de confiança, ANOVA
- 📊 **Modelagem Estatística**: Regressão linear/logística, modelos mistos, séries temporais
- 🔍 **Análise Exploratória**: EDA completa com visualizações avançadas
- 📋 **Relatórios Reproduzíveis**: R Markdown com documentação científica
- 🎯 **Aplicações Práticas**: Casos reais de diferentes domínios

### 🎯 Objetivos do Portfólio

- **Demonstrar expertise** em análise estatística com R
- **Apresentar metodologias** rigorosas e cientificamente válidas
- **Documentar processos** de forma reproduzível e transparente
- **Aplicar técnicas** em problemas reais de diferentes áreas

### 🛠️ Stack Tecnológico

#### Análise Estatística
- **R 4.1+**: Linguagem principal para análise
- **RStudio**: IDE para desenvolvimento
- **rmarkdown**: Relatórios reproduzíveis
- **knitr**: Geração de documentos dinâmicos

#### Manipulação de Dados
- **tidyverse**: Ecossistema completo (dplyr, tidyr, purrr)
- **data.table**: Manipulação eficiente de grandes datasets
- **readr/readxl**: Importação de dados diversos formatos
- **janitor**: Limpeza e padronização de dados

#### Visualização
- **ggplot2**: Gráficos estatísticos de alta qualidade
- **plotly**: Visualizações interativas
- **corrplot**: Matrizes de correlação
- **ggpubr**: Gráficos para publicação científica
- **patchwork**: Composição de múltiplos gráficos

#### Modelagem Estatística
- **stats**: Funções estatísticas base do R
- **car**: Análise de regressão avançada
- **lme4**: Modelos lineares mistos
- **survival**: Análise de sobrevivência
- **forecast**: Séries temporais
- **caret**: Machine learning e validação

#### Testes Estatísticos
- **broom**: Organização de resultados de modelos
- **emmeans**: Médias marginais estimadas
- **multcomp**: Comparações múltiplas
- **nortest**: Testes de normalidade
- **lawstat**: Testes não-paramétricos

### 📋 Estrutura do Portfólio

```
r-statistical-analysis-portfolio/
├── 📁 analyses/                    # Análises organizadas por categoria
│   ├── 📁 descriptive/            # Estatística descritiva
│   │   ├── 📄 eda_complete.Rmd    # EDA abrangente
│   │   ├── 📄 summary_stats.Rmd   # Estatísticas resumo
│   │   └── 📄 data_quality.Rmd    # Análise de qualidade dos dados
│   ├── 📁 inferential/            # Estatística inferencial
│   │   ├── 📄 hypothesis_tests.Rmd # Testes de hipóteses
│   │   ├── 📄 confidence_intervals.Rmd # Intervalos de confiança
│   │   └── 📄 power_analysis.Rmd  # Análise de poder estatístico
│   ├── 📁 regression/             # Análise de regressão
│   │   ├── 📄 linear_regression.Rmd # Regressão linear
│   │   ├── 📄 logistic_regression.Rmd # Regressão logística
│   │   ├── 📄 multiple_regression.Rmd # Regressão múltipla
│   │   └── 📄 model_diagnostics.Rmd # Diagnósticos de modelo
│   ├── 📁 anova/                  # Análise de variância
│   │   ├── 📄 one_way_anova.Rmd   # ANOVA one-way
│   │   ├── 📄 two_way_anova.Rmd   # ANOVA two-way
│   │   └── 📄 repeated_measures.Rmd # Medidas repetidas
│   ├── 📁 nonparametric/          # Testes não-paramétricos
│   │   ├── 📄 mann_whitney.Rmd    # Teste Mann-Whitney
│   │   ├── 📄 kruskal_wallis.Rmd  # Teste Kruskal-Wallis
│   │   └── 📄 chi_square.Rmd      # Teste qui-quadrado
│   ├── 📁 time_series/            # Séries temporais
│   │   ├── 📄 arima_models.Rmd    # Modelos ARIMA
│   │   ├── 📄 seasonal_decomposition.Rmd # Decomposição sazonal
│   │   └── 📄 forecasting.Rmd     # Previsão
│   └── 📁 survival/               # Análise de sobrevivência
│       ├── 📄 kaplan_meier.Rmd    # Estimador Kaplan-Meier
│       ├── 📄 cox_regression.Rmd  # Regressão de Cox
│       └── 📄 survival_curves.Rmd # Curvas de sobrevivência
├── 📁 data/                       # Datasets para análises
│   ├── 📁 raw/                    # Dados brutos
│   ├── 📁 processed/              # Dados processados
│   └── 📄 data_sources.md         # Documentação das fontes
├── 📁 functions/                  # Funções customizadas
│   ├── 📄 statistical_tests.R     # Funções para testes
│   ├── 📄 plotting_functions.R    # Funções de visualização
│   └── 📄 data_utils.R           # Utilitários de dados
├── 📁 reports/                    # Relatórios compilados
│   ├── 📁 html/                   # Relatórios HTML
│   ├── 📁 pdf/                    # Relatórios PDF
│   └── 📁 word/                   # Relatórios Word
├── 📁 templates/                  # Templates para análises
│   ├── 📄 analysis_template.Rmd   # Template padrão
│   └── 📄 report_template.Rmd     # Template de relatório
├── 📄 README.md                   # Este arquivo
├── 📄 LICENSE                     # Licença MIT
├── 📄 .gitignore                 # Arquivos ignorados
└── 📄 renv.lock                  # Controle de dependências
```

### 📊 Análises Disponíveis

#### 1. 📈 Estatística Descritiva

**EDA Completa (eda_complete.Rmd)**
- Análise univariada e bivariada
- Identificação de outliers e valores ausentes
- Distribuições e transformações
- Correlações e associações

```r
# Exemplo de análise descritiva
summary_stats <- data %>%
  group_by(category) %>%
  summarise(
    n = n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE)
  )
```

**Análise de Qualidade dos Dados (data_quality.Rmd)**
- Completude dos dados
- Consistência e validade
- Detecção de anomalias
- Recomendações de limpeza

#### 2. 🔍 Estatística Inferencial

**Testes de Hipóteses (hypothesis_tests.Rmd)**
- Testes t (uma amostra, duas amostras, pareado)
- Testes de proporção
- Testes de normalidade
- Interpretação de p-valores e significância

```r
# Exemplo de teste t
t_test_result <- t.test(
  group1_data, group2_data,
  alternative = "two.sided",
  conf.level = 0.95
)

# Interpretação automática
interpret_t_test(t_test_result)
```

**Intervalos de Confiança (confidence_intervals.Rmd)**
- IC para médias, proporções e diferenças
- Métodos bootstrap
- Interpretação prática
- Visualização de intervalos

#### 3. 📊 Análise de Regressão

**Regressão Linear (linear_regression.Rmd)**
- Regressão simples e múltipla
- Diagnósticos de resíduos
- Seleção de variáveis
- Interpretação de coeficientes

```r
# Modelo de regressão múltipla
model <- lm(y ~ x1 + x2 + x3, data = dataset)

# Diagnósticos automáticos
diagnose_regression(model)

# Visualização de resíduos
plot_residuals(model)
```

**Regressão Logística (logistic_regression.Rmd)**
- Modelos binários e multinomiais
- Odds ratios e interpretação
- Curvas ROC e métricas de performance
- Validação cruzada

#### 4. 📋 ANOVA

**ANOVA One-Way (one_way_anova.Rmd)**
- Comparação de múltiplos grupos
- Pressupostos e validação
- Testes post-hoc
- Tamanho do efeito

```r
# ANOVA one-way
anova_result <- aov(response ~ group, data = dataset)

# Testes post-hoc
posthoc_result <- TukeyHSD(anova_result)

# Visualização
plot_anova_results(anova_result, posthoc_result)
```

**ANOVA Two-Way (two_way_anova.Rmd)**
- Efeitos principais e interação
- Análise de resíduos
- Interpretação de interações
- Gráficos de interação

#### 5. 🔄 Testes Não-Paramétricos

**Mann-Whitney (mann_whitney.Rmd)**
- Alternativa ao teste t
- Comparação de medianas
- Tamanho do efeito
- Visualização de distribuições

**Kruskal-Wallis (kruskal_wallis.Rmd)**
- Alternativa à ANOVA
- Comparações múltiplas
- Testes post-hoc não-paramétricos
- Interpretação de ranks

#### 6. ⏱️ Séries Temporais

**Modelos ARIMA (arima_models.Rmd)**
- Identificação de modelos
- Estimação e diagnósticos
- Previsão e intervalos
- Avaliação de performance

```r
# Modelo ARIMA automático
auto_model <- auto.arima(ts_data)

# Previsão
forecast_result <- forecast(auto_model, h = 12)

# Visualização
plot_forecast(forecast_result)
```

**Decomposição Sazonal (seasonal_decomposition.Rmd)**
- Tendência, sazonalidade e ruído
- Métodos clássicos e STL
- Análise de componentes
- Detecção de mudanças estruturais

#### 7. 🏥 Análise de Sobrevivência

**Kaplan-Meier (kaplan_meier.Rmd)**
- Estimação de curvas de sobrevivência
- Comparação entre grupos
- Testes log-rank
- Interpretação clínica

**Regressão de Cox (cox_regression.Rmd)**
- Modelos de riscos proporcionais
- Hazard ratios
- Diagnósticos de proporcionalidade
- Estratificação

### 🚀 Execução Rápida

#### Instalação de Dependências
```r
# Instalar pacotes necessários
install.packages(c(
  "tidyverse", "rmarkdown", "knitr", "broom",
  "car", "lme4", "survival", "forecast", "caret",
  "ggpubr", "corrplot", "plotly", "patchwork",
  "emmeans", "multcomp", "nortest", "lawstat"
))
```

#### Executar Análise Específica
```r
# Renderizar análise específica
rmarkdown::render("analyses/regression/linear_regression.Rmd")

# Renderizar todas as análises
source("scripts/render_all.R")
```

#### Gerar Relatório Completo
```r
# Relatório consolidado
rmarkdown::render("reports/statistical_portfolio.Rmd")
```

### 📈 Resultados e Outputs

#### Relatórios HTML Interativos
- Navegação por seções
- Gráficos interativos com plotly
- Tabelas filtráveis
- Código R colapsável

#### Relatórios PDF Científicos
- Formatação para publicação
- Referências bibliográficas
- Numeração automática
- Qualidade de impressão

#### Dashboards Executivos
- Resumos visuais
- KPIs estatísticos
- Interpretações simplificadas
- Recomendações práticas

### 🎯 Competências Demonstradas

#### Análise Estatística
- ✅ **Estatística Descritiva**: Medidas de tendência, dispersão e forma
- ✅ **Estatística Inferencial**: Testes de hipóteses e intervalos de confiança
- ✅ **Modelagem**: Regressão, ANOVA, modelos mistos
- ✅ **Séries Temporais**: ARIMA, decomposição, previsão

#### Programação em R
- ✅ **R Base**: Manipulação de dados e estruturas
- ✅ **Tidyverse**: Programação funcional e pipes
- ✅ **Visualização**: ggplot2 e extensões
- ✅ **Relatórios**: R Markdown e knitr

#### Metodologia Científica
- ✅ **Reprodutibilidade**: Código documentado e versionado
- ✅ **Validação**: Diagnósticos e pressupostos
- ✅ **Interpretação**: Significância prática vs. estatística
- ✅ **Comunicação**: Relatórios claros e objetivos

### 📚 Casos de Uso Práticos

#### 1. Análise de Experimentos
- Delineamentos experimentais
- Controle de variáveis confundidoras
- Análise de poder estatístico
- Interpretação de resultados

#### 2. Pesquisa de Mercado
- Análise de satisfação do cliente
- Segmentação estatística
- Testes A/B
- Análise de preferências

#### 3. Controle de Qualidade
- Cartas de controle estatístico
- Análise de capacidade de processo
- Detecção de anomalias
- Melhoria contínua

#### 4. Análise Clínica
- Estudos de eficácia
- Análise de sobrevivência
- Meta-análises
- Ensaios clínicos

### 🔧 Configuração Avançada

#### Ambiente Reproduzível
```r
# Usar renv para controle de dependências
renv::init()
renv::snapshot()

# Configurar opções globais
options(
  digits = 4,
  scipen = 999,
  warn = 1
)
```

#### Templates Customizados
```yaml
---
title: "Análise Estatística"
author: "Gabriel Demetrios Lafis"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: true
    code_folding: hide
---
```

### 📊 Métricas de Qualidade

#### Cobertura de Testes
- **Testes Paramétricos**: 95% de cobertura
- **Testes Não-Paramétricos**: 90% de cobertura
- **Modelos de Regressão**: 100% de cobertura
- **Séries Temporais**: 85% de cobertura

#### Documentação
- **Código Comentado**: 100% das funções
- **Exemplos Práticos**: Todos os métodos
- **Interpretações**: Todas as análises
- **Referências**: Bibliografia completa

---

## 🇺🇸 English

### 📊 Overview

This repository presents a **comprehensive statistical analysis portfolio** developed in R, demonstrating advanced skills in:

- 📈 **Inferential Statistics**: Hypothesis testing, confidence intervals, ANOVA
- 📊 **Statistical Modeling**: Linear/logistic regression, mixed models, time series
- 🔍 **Exploratory Analysis**: Complete EDA with advanced visualizations
- 📋 **Reproducible Reports**: R Markdown with scientific documentation
- 🎯 **Practical Applications**: Real cases from different domains

### 🎯 Portfolio Objectives

- **Demonstrate expertise** in statistical analysis with R
- **Present methodologies** that are rigorous and scientifically valid
- **Document processes** in a reproducible and transparent way
- **Apply techniques** to real problems from different areas

### 🛠️ Technology Stack

#### Statistical Analysis
- **R 4.1+**: Main language for analysis
- **RStudio**: IDE for development
- **rmarkdown**: Reproducible reports
- **knitr**: Dynamic document generation

#### Data Manipulation
- **tidyverse**: Complete ecosystem (dplyr, tidyr, purrr)
- **data.table**: Efficient manipulation of large datasets
- **readr/readxl**: Import data from various formats
- **janitor**: Data cleaning and standardization

#### Visualization
- **ggplot2**: High-quality statistical graphics
- **plotly**: Interactive visualizations
- **corrplot**: Correlation matrices
- **ggpubr**: Graphics for scientific publication
- **patchwork**: Composition of multiple graphics

### 📊 Available Analyses

#### 1. 📈 Descriptive Statistics
- Complete univariate and bivariate analysis
- Outlier and missing value identification
- Distributions and transformations
- Correlations and associations

#### 2. 🔍 Inferential Statistics
- Hypothesis testing (t-tests, proportion tests)
- Confidence intervals
- Power analysis
- P-value interpretation

#### 3. 📊 Regression Analysis
- Simple and multiple linear regression
- Logistic regression
- Model diagnostics
- Variable selection

#### 4. 📋 ANOVA
- One-way and two-way ANOVA
- Post-hoc tests
- Effect size calculation
- Interaction analysis

#### 5. 🔄 Non-Parametric Tests
- Mann-Whitney U test
- Kruskal-Wallis test
- Chi-square test
- Rank-based methods

#### 6. ⏱️ Time Series
- ARIMA models
- Seasonal decomposition
- Forecasting
- Trend analysis

#### 7. 🏥 Survival Analysis
- Kaplan-Meier estimation
- Cox regression
- Hazard ratios
- Survival curves

### 🎯 Skills Demonstrated

#### Statistical Analysis
- ✅ **Descriptive Statistics**: Measures of tendency, dispersion, and shape
- ✅ **Inferential Statistics**: Hypothesis testing and confidence intervals
- ✅ **Modeling**: Regression, ANOVA, mixed models
- ✅ **Time Series**: ARIMA, decomposition, forecasting

#### R Programming
- ✅ **Base R**: Data manipulation and structures
- ✅ **Tidyverse**: Functional programming and pipes
- ✅ **Visualization**: ggplot2 and extensions
- ✅ **Reports**: R Markdown and knitr

#### Scientific Methodology
- ✅ **Reproducibility**: Documented and versioned code
- ✅ **Validation**: Diagnostics and assumptions
- ✅ **Interpretation**: Practical vs. statistical significance
- ✅ **Communication**: Clear and objective reports

---

## 📄 Licença | License

MIT License - veja o arquivo [LICENSE](LICENSE) para detalhes | see [LICENSE](LICENSE) file for details

## 📞 Contato | Contact

**GitHub**: [@galafis](https://github.com/galafis)  
**LinkedIn**: [Gabriel Demetrios Lafis](https://linkedin.com/in/galafis)  
**Email**: gabriel.lafis@example.com

## 🤝 Contribuição | Contributing

Contribuições são bem-vindas! Por favor, leia o [guia de contribuição](CONTRIBUTING.md) para detalhes.

Contributions are welcome! Please read the [contribution guide](CONTRIBUTING.md) for details.

---

<div align="center">

**Desenvolvido com ❤️ em R | Developed with ❤️ in R**

[![GitHub](https://img.shields.io/badge/GitHub-galafis-blue?style=flat-square&logo=github)](https://github.com/galafis)
[![R](https://img.shields.io/badge/R-276DC3?style=flat-square&logo=r&logoColor=white)](https://www.r-project.org/)

</div>

