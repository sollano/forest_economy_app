library(tidyverse)
library(formattable)
library(FinCal)

dados <- openxlsx::read.xlsx("dados.xlsx")

df <- dados
taxa_a_a <- 8.75
ano <- "ano"
custo <- "custos"
receita <- "receitas"


ano_name <- ano
ano_sym <- rlang::sym(ano)
custo_sym <- rlang::sym(custo)
receita_sym <- rlang::sym(receita)

df[is.na(df)] <- 0

tab1 <- df %>% 
  dplyr::mutate(
    !!ano_name := as.integer(!!ano_sym), # converter pra integer, pra diferenciar dos outros numeros, que sao  double
    saldo = !!receita_sym - !!custo_sym,
    VoCT = !!custo_sym/(1+taxa_a_a/100)^(!!ano_sym),
    VoRT = !!receita_sym/(1+taxa_a_a/100)^(!!ano_sym), 
    VnCT = !!custo_sym*(1+taxa_a_a/100)^(max(!!ano_sym,na.rm=TRUE) - !!ano_sym),
    VnRT = !!receita_sym*(1+taxa_a_a/100)^(max(!!ano_sym,na.rm=TRUE) - !!ano_sym)) %>% 
  dplyr::mutate_if(is.double,formattable::comma) # formatar todos os numeros que sao double
  
tab1

# Calcula tir
tir <- FinCal::irr2(as.numeric(tab1$saldo))


tab2 <- tab1 %>%
  summarise_all(funs(total=sum(.,na.rm=TRUE))) %>% 
  mutate(n=max(df[[ano_name]]),
         taxa.a.a = formattable::percent(taxa_a_a/100),
         !!paste(ano_name,"total",sep="_"):=NULL,
         VPL = VoRT_total - VoCT_total,
         VFL = VnRT_total - VnCT_total,
         BC  = VoRT_total / VoCT_total,
         VET = (VnRT_total - VnCT_total) / ((1+taxa_a_a/100)^n - 1 ),
         VPLA = ((VnRT_total - VnCT_total)*taxa_a_a/100) / ((1+taxa_a_a/100)^n - 1 ),
         TIR = formattable::percent(tir)  ) 
tab2  


suppressWarnings(
  
  tab2_mod <- tab2 %>% 
    dplyr::mutate_at(vars(taxa.a.a,TIR),~.*100) %>% 
    select(-dplyr::contains("total"), -n,-taxa.a.a) %>% 
    tidyr::gather("Variável", "Valor") %>% 
    dplyr::mutate_at(vars(Valor),formattable::comma)
  
)

tab2_mod


