source("funs/vpl_tir.R",encoding = "UTF-8")

dados <- openxlsx::read.xlsx("dados.xlsx")
data_ex <- openxlsx::read.xlsx("dados.xlsx",2)

dados

vpl_tir(dados,"ano","custos","receitas",taxa_a_a=8.75)
vpl_tir(dados,"ano","custos","receitas",taxa_a_a=8.75, "simple")
vpl_tir(dados,"ano","custos","receitas",taxa_a_a=8.75, "full")
vpl_tir(dados2,"ano","custos","receitas",taxa_a_a=10, "full")



npv_irr(data_ex,"year","cost","revenue",rate=8.75)
npv_irr(data_ex,"year","cost","revenue",rate=8.75, "simple")
npv_irr(data_ex,"year","cost","revenue",rate=8.75, "full")

