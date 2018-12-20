vpl_tir <- function(df, ano, custo, receita, taxa_a_a,output="horizontal"){
  
  # ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se ano nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(ano) ){  
    stop("ano not set", call. = F) 
  }else if( !is.character(ano) ){
    stop("'ano' must be a character containing a variable name", call.=F)
  }else if(length(ano)!=1){
    stop("Length of 'ano' must be 1", call.=F)
  }else if(forestmangr::check_names(df, ano)==F){
    stop(forestmangr::check_names(df, ano, boolean=F), call.=F)
  }
  
  # se custo nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(custo) ){  
    stop("custo not set", call. = F) 
  }else if( !is.character(custo) ){
    stop("'custo' must be a character containing a variable name", call.=F)
  }else if(length(custo)!=1){
    stop("Length of 'custo' must be 1", call.=F)
  }else if(forestmangr::check_names(df, custo)==F){
    stop(forestmangr::check_names(df, custo, boolean=F), call.=F)
  }
  
  # se receita nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(receita) ){  
    stop("receita not set", call. = F) 
  }else if( !is.character(receita) ){
    stop("'receita' must be a character containing a variable name", call.=F)
  }else if(length(receita)!=1){
    stop("Length of 'receita' must be 1", call.=F)
  }else if(forestmangr::check_names(df, receita)==F){
    stop(forestmangr::check_names(df, receita, boolean=F), call.=F)
  }
  
  # Se taxa.a.a nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( taxa_a_a )){
    stop( "'taxa_a_a' must be numeric", call.=F)
  }else if(length(taxa_a_a)!=1){
    stop("length of 'taxa_a_a' must be 1", call.=F)
  }else if(! taxa_a_a > 0 | ! taxa_a_a <= 1000){
    stop("'taxa_a_a' must be a number between 0 and 1000", call.=F)
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('horizontal', 'vertical') ){ 
  stop("'output' must be equal to 'horizontal' or 'vertical' ", call. = F) 
  }
  
  # ####
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
           #taxa.a.a = formattable::percent(taxa_a_a/100),
           !!paste(ano_name,"total",sep="_"):=NULL,
           VPL = VoRT_total - VoCT_total,
           VFL = VnRT_total - VnCT_total,
           BC  = VoRT_total / VoCT_total,
           VET = (VnRT_total - VnCT_total) / ((1+taxa_a_a/100)^n - 1 ),
           VPLA = ((VnRT_total - VnCT_total)*taxa_a_a/100) / ((1+taxa_a_a/100)^n - 1 ),
           TIR = formattable::percent(tir)  ) 
  
  if(output=="horizontal"){
    
  return(dplyr::select(tab2, -dplyr::contains("total"), -n) )
    
  }else if(output=="vertical"){
    suppressWarnings(
      
      tab2_mod <- tab2 %>% 
        dplyr::mutate_at(vars(
          #taxa.a.a,
          TIR),~.*100) %>% 
        dplyr::select(-dplyr::contains("total"),
                      #-taxa.a.a,
                      -n) %>% 
        tidyr::gather("Variável", "Valor") %>% 
        dplyr::mutate_at(vars(Valor),formattable::comma)
      
    )
    return(tab2_mod)
    
    }
  
  
}

