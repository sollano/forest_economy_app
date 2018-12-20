source("funs/vpl_tir.R",encoding = "UTF-8")

dados <- openxlsx::read.xlsx("dados.xlsx")

dados

vpl_tir(dados,"ano","custos","receitas",taxa_a_a=8.75)

vpl_tir(dados,"ano","custos","receitas",taxa_a_a=8.75,"vertical")

formattable(teste,)
# usuário digita o horizonte, tipo 0 a 7.
# Uma planilha é criada automaticamente, onde ele pode digitar os valores
# conforme ele digita, os valores de vpl e tir são calculados
