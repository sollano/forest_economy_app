vpl_tir <- function(df, ano, custo, receita, taxa_a_a,output="full", big_mark=".",dec_mark=",", prefix="R$"){
  
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
  }else if(check_names(df, ano)==F){
    stop(check_names(df, ano, boolean=F), call.=F)
  }
  
  # se custo nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(custo) ){  
    stop("custo not set", call. = F) 
  }else if( !is.character(custo) ){
    stop("'custo' must be a character containing a variable name", call.=F)
  }else if(length(custo)!=1){
    stop("Length of 'custo' must be 1", call.=F)
  }else if(check_names(df, custo)==F){
    stop(check_names(df, custo, boolean=F), call.=F)
  }
  
  # se receita nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(receita) ){  
    stop("receita not set", call. = F) 
  }else if( !is.character(receita) ){
    stop("'receita' must be a character containing a variable name", call.=F)
  }else if(length(receita)!=1){
    stop("Length of 'receita' must be 1", call.=F)
  }else if(check_names(df, receita)==F){
    stop(check_names(df, receita, boolean=F), call.=F)
  }
  
  # Se taxa_a_a nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
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
  }else if(! output %in% c('simple', 'full') ){ 
  stop("'output' must be equal to 'simple' or 'full' ", call. = F) 
  }
  
  # Se big_mark nao for character,ou nao for de tamanho 1, parar
  if(!is.character( big_mark )){
    stop( "'big_mark' must be character", call.=F)
  }else if(length(big_mark)!=1){
    stop("Length of 'big_mark' must be 1", call.=F)
  }else if(! big_mark %in% c('.', ',', " ") ){ 
  stop("'big_mark' must be equal to '.', ',' or ' ' ", call. = F) 
  }
  
  # Se dec_mark nao for character,ou nao for de tamanho 1, parar
  if(!is.character( dec_mark )){
    stop( "'dec_mark' must be character", call.=F)
  }else if(length(dec_mark)!=1){
    stop("Length of 'dec_mark' must be 1", call.=F)
  }else if(! dec_mark %in% c('.', ',') ){ 
  stop("'dec_mark' must be equal to '.' or ',' ", call. = F) 
  }
  
  # Se prefix nao for character,ou nao for de tamanho 1, parar
  if(!is.character( prefix )){
    stop( "'prefix' must be character", call.=F)
  }else if(length(prefix)!=1){
    stop("Length of 'prefix' must be 1", call.=F)
  }else if(! prefix %in% c('R$', '$') ){ 
  stop("'prefix' must be equal to 'R$' or '$' ", call. = F) 
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
      saldo = (!!receita_sym) - (!!custo_sym),
      VoCT = (!!custo_sym)/(1+taxa_a_a/100)^(!!ano_sym),
      VoRT = (!!receita_sym)/(1+taxa_a_a/100)^(!!ano_sym), 
      VnCT = (!!custo_sym)*(1+taxa_a_a/100)^(max(!!ano_sym,na.rm=TRUE) - (!!ano_sym) ),
      VnRT = (!!receita_sym)*(1+taxa_a_a/100)^(max(!!ano_sym,na.rm=TRUE) - (!!ano_sym) )) %>% 
    dplyr::mutate_if(is.double,formattable::comma,big.mark=big_mark,decimal.mark=dec_mark) # formatar todos os numeros que sao double
  
  tab1
  
  # Calcula tir
  tir <- FinCal::irr2(as.numeric(tab1$saldo))
  
  
  tab2 <- tab1 %>%
    dplyr::summarise_all(dplyr::funs(total=sum(.,na.rm=TRUE))) %>% 
    dplyr::mutate(n=max(df[[ano_name]]),
           #taxa_a_a = formattable::percent(taxa_a_a/100),
           !!paste(ano_name,"total",sep="_"):=NULL,
           VPL = VoRT_total - VoCT_total, # Valor Presente Liquido
           VFL = VnRT_total - VnCT_total, # Valor Futuro Liquido
           BC  = VoRT_total / VoCT_total, # Razao Beneficio custo
           VET = (VnRT_total - VnCT_total) / ((1+taxa_a_a/100)^n - 1 ),
           VPLA = ((VnRT_total - VnCT_total)*taxa_a_a/100) / ((1+taxa_a_a/100)^n - 1 ), # Valor Presente Liquido Anualizado
           TIR = formattable::percent(tir,decimal.mark=dec_mark)  ) # Taxa Interna de Retorno
  
  if(output=="simple"){
    
  return(dplyr::select(tab2, -dplyr::contains("total"), -n) )
    
  }else if(output=="full"){
    suppressWarnings(
      
      tab2_mod <- tab2 %>% 
        dplyr::mutate_at(
          dplyr::vars(
          #taxa_a_a,
          TIR),~.*100) %>% 
        dplyr::select(-dplyr::contains("total"),
                      #-taxa_a_a,
                      -n) %>% 
        tidyr::gather("Variável", "Valor") %>% 
        dplyr::mutate_at(
          dplyr::vars(Valor),
          formattable::comma,big.mark=big_mark,decimal.mark=dec_mark ) %>% 
        dplyr::mutate(
          `Variável` = forcats::fct_recode(`Variável`, 
                          "Valor Presente Liquido (VPL)" = "VPL",
                          "Valor Futuro Liquido (VFL)" = "VFL",
                          "Razão Benefício Custo (BC)" = "BC",
                          "Valor Esperado da Terra (VET)" = "VET",
                          "Valor Presente Liquido Anualizado (VPLA)" = "VPLA",
                          "Taxa Interna de Retorno (TIR)" = "TIR") )
      
    )
    
    # sensibilidade ####
    plot_data <- data.frame(x_axis = seq(0.01, round(taxa_a_a/100*10)/10 +0.2, 0.01) ) %>%
      dplyr::mutate(
        VPR = purrr::map_dbl(x_axis, ~sum(df[[receita]] / (1 + .x)^df[[ano]],na.rm=T)),
        VPC = purrr::map_dbl(x_axis, ~sum(df[[custo]] / (1 + .x)^df[[ano]],na.rm=T)),
        VFR = purrr::map_dbl(x_axis, ~sum(df[[receita]] * (1 + .x)^df[[ano]],na.rm=T)),
        VFC = purrr::map_dbl(x_axis, ~sum(df[[custo]] * (1 + .x)^df[[ano]],na.rm=T)),
        VPL = VPR - VPC,
        VET = (VFR - VFC)/((1+x_axis)^(length(df[[ano]])-1)-1) ) %>% 
      dplyr::select(x_axis,VPL, VET) %>% 
      tidyr::gather("facet_var", "y_axis", VPL, VET,factor_key = TRUE) %>% 
      dplyr::group_by(facet_var) %>% 
      dplyr::mutate(red_y_axis = ifelse(y_axis<0, y_axis, NA ),
                    red_y2_axis = any(y_axis < 0),
                    red_y2_axis = ifelse(red_y2_axis, 0, NA )
                    
      ) %>% as.data.frame()
    
    
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x_axis, y_axis) ) + 
      ggplot2::geom_line(size=1)+
      ggplot2::geom_line(ggplot2::aes(y=red_y_axis), color="red",size=1,na.rm=T )+
      ggplot2::geom_line(ggplot2::aes(y=red_y2_axis), color="red",size=1,na.rm=T )+
      ggplot2::facet_wrap(~facet_var, scales="free_y",ncol=1,strip.position = "right") +
      ggplot2::scale_x_continuous(labels=scales::percent) +
      ggplot2::scale_y_continuous(labels=scales::dollar_format(big.mark=big_mark,decimal.mark = dec_mark,prefix=paste(prefix," ",sep="")))+
      ggplot2::labs(x="Taxa",y=NULL) +
      ggthemes::theme_igray(base_family = "serif") +
      ggplot2::theme(
        axis.title       = ggplot2::element_text(size = 22), 
        axis.text        = ggplot2::element_text(size = 20),
        plot.title       = ggplot2::element_text(size = 22, face = "bold"),
        legend.title     = ggplot2::element_text(size = 20),
        legend.text      = ggplot2::element_text(size = 18),
        plot.caption     = ggplot2::element_text(size = 20),
        strip.text.y     = ggplot2::element_text(size = 18, face = "bold",angle=0),
        strip.background = ggplot2::element_blank() ) 
    # ####
    
    return(list(Sensibilidade=plot,Resultado=tab2_mod))
    
    }
  
  
}

