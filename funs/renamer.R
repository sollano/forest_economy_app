renamer <- function(df, arvore,parcelas,especies,cap,dap,ht,
                    vcc,vsc,area.parcela,area.total,
                    est.vertical,est.interna,estrato,
                    hd, idade, obs, di, hi, e_casca, comp_secao,
                    ano,custo,receita,taxa.a.a){
  
  
  # ####
  
  dfmod <- data.frame()
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }else{
    df <- as.data.frame(df)
    dfmod <- data.frame(XX=1:nrow(df))
  }
  
  # se arvore nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(arvore) || is.null(arvore) || is.na(arvore) || arvore=="" ){  
    
  }else if( !is.character(arvore) ){
  }else if(length(arvore)!=1){
    stop("Length of 'arvore' must be 1", call.=F)
  }else if(check_names(df, arvore)==F){
    
  }else{
    dfmod[["arvore"]] <- df[[arvore]]
  }
  
  # se parcelas nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  # se parcelas nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(parcelas) || is.null(parcelas) || is.na(parcelas) || parcelas=="" ){  
  }else if( !is.character(parcelas) ){
  }else if(length(parcelas)!=1){
    stop("Length of 'parcelas' must be 1", call.=F)
  }else if(check_names(df, parcelas)==F){
    
  }else{
    dfmod[["parcelas"]] <- df[[parcelas]]
  }
  
  # se especies nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(especies) || is.null(especies) || is.na(especies) || especies=="" ){  
    
  }else if( !is.character(especies) ){
  }else if(length(especies)!=1){
    stop("Length of 'especies' must be 1", call.=F)
  }else if(check_names(df, especies)==F){
    
  }else{
    dfmod[["especies"]] <- df[[especies]]
  }
  
  # se cap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(cap) || is.null(cap) || is.na(cap) || cap=="" ){  
    
  }else if( !is.character(cap) ){
  }else if(length(cap)!=1){
    stop("Length of 'cap' must be 1", call.=F)
  }else if(check_names(df, cap)==F){
    
  }else{
    dfmod[["cap"]] <- df[[cap]]
  }
  
  # se dap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dap) || is.null(dap) || is.na(dap) || dap=="" ){  
    
  }else if( !is.character(dap) ){
  }else if(length(dap)!=1){
    stop("Length of 'dap' must be 1", call.=F)
  }else if(check_names(df, dap)==F){
    
  }else{
    dfmod[["dap"]] <- df[[dap]]
  }
  
  # se ht nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(ht) || is.null(ht) || is.na(ht) || ht=="" ){  
    
  }else if( !is.character(ht) ){
  }else if(length(ht)!=1){
    
  }else if(check_names(df, ht)==F){
    
  }else{
    dfmod[["ht"]] <- df[[ht]]
  }
  
  # se vcc nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(vcc) || is.null(vcc) || is.na(vcc) || vcc=="" ){  
    
  }else if( !is.character(vcc) ){
  }else if(length(vcc)!=1){
    stop("Length of 'vcc' must be 1", call.=F)
  }else if(check_names(df, vcc)==F){
    
  }else{
    dfmod[["vcc"]] <- df[[vcc]]
  }
  
  # se vsc nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(vsc) || is.null(vsc) || is.na(vsc) || vsc=="" ){  
    
  }else if( !is.character(vsc) ){
    
  }else if(length(vsc)!=1){
    stop("Length of 'vsc' must be 1", call.=F)
  }else if(check_names(df, vsc)==F){
    
  }else{
    dfmod[["vsc"]] <- df[[vsc]]
  }
  
  # se area.parcela nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(area.parcela) || is.null(area.parcela) || is.na(area.parcela) || area.parcela=="" ){  
    
  }else if( is.null(area.parcela) || is.na(area.parcela) || area.parcela == "" ){
  
  }else if(is.numeric(area.parcela) & length(area.parcela)==1){
    
  dfmod[["area.parcela"]] <- area.parcela
    
  }else if(!is.character(area.parcela)){

  }else if(length(area.parcela)!=1){
    stop("Length of 'area.parcela' must be 1", call.=F)
  }else if(check_names(df, area.parcela)==F){
    
  }else{
    dfmod[["area.parcela"]] <- df[[area.parcela]]
  }
  
  # se area.total nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(area.total) || is.null(area.total) || is.na(area.total) || area.total=="" ){  
  }else if( is.null(area.total) || is.na(area.total) || area.total == "" ){
  }else if(is.numeric(area.total) & length(area.total)==1){

  dfmod[["area.total"]] <- area.total
  
  }else if(!is.character(area.total)){
  }else if(length(area.total)!=1){
    stop("Length of 'area.total' must be 1", call.=F)
  }else if(check_names(df, area.total)==F){
    
  }else{
    dfmod[["area.total"]] <- df[[area.total]]
  }
  
  # se est.vertical nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(est.vertical) || is.null(est.vertical) || is.na(est.vertical) || est.vertical=="" ){  
  }else if( !is.character(est.vertical) ){
  }else if(length(est.vertical)!=1){
    stop("Length of 'est.vertical' must be 1", call.=F)
  }else if(check_names(df, est.vertical)==F){
    
  }else{
    dfmod[["est.vertical"]] <- df[[est.vertical]]
  }
  
  # se est.interna nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(est.interna) || is.null(est.interna) || is.na(est.interna) || est.interna=="" ){  
  }else if( !is.character(est.interna) ){
  }else if(length(est.interna)!=1){
    stop("Length of 'est.interna' must be 1", call.=F)
  }else if(check_names(df, est.interna)==F){
    
  }else{
    dfmod[["est.interna"]] <- df[[est.interna]]
  }
  
  # se estrato nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(estrato) || is.null(estrato) || is.na(estrato) || estrato=="" ){
    
  }else if( !is.character(estrato) ){
  }else if(check_names(df, estrato)==F){
    
  }else{
    
    # Se tiver um estrato so, o nome vai ser estrato,
    # se tiver mais de um, vai ser estrato1, esterato2, etc
    if(length(estrato)==1){
      lista_var_novas <- "estrato"
    }else{lista_var_novas <- paste("estrato", 1:length(estrato),sep="")}
    
    # funcao para adicionar varios estratos
    ft <- function(var1,var2,df1,df2){df1[[var1]] <- df2[[var2]]; return(df1[var1]) }
    
    # cria as colunas separadas com os nomes de 1 a n.
    # depois junta com o dado original utilizando bind_cols
    dfmod <- as.data.frame(
      dplyr::bind_cols(
        purrr::map2(.x = lista_var_novas, .y = estrato, ~ft(.x,.y, dfmod, df ) ),
        dfmod
      )
    )
    
  }
  
  # se hd nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(hd) || is.null(hd) || is.na(hd) || hd == "" ){
  }else if(!is.character(hd)){
    stop("'hd' must be a character containing a variable name", call.=F)
  }else if(length(hd)!=1){
    
  }else if(check_names(df, hd)==F){
    
  }else{
    dfmod[["hd"]] <- df[[hd]]
  }
  
  # se idade nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(idade) || is.null(idade) || is.na(idade) || idade == "" ){
  }else if(!is.character(idade)){
    stop("'idade' must be a character containing a variable name", call.=F)
  }else if(length(idade)!=1){
    stop("Length of 'idade' must be 1", call.=F)
  }else if(check_names(df, idade)==F){
    
  }else{
    dfmod[["idade"]] <- df[[idade]]
  }
  
  # se di nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(di) || is.null(di) || is.na(di) || di == "" ){

  }else if(!is.character(di)){
    stop("'di' must be a character containing a variable name", call.=F)
  }else if(length(di)!=1){
    stop("Length of 'di' must be 1", call.=F)
  }else if(check_names(df, di)==F){
    
  }else{
    dfmod[["di"]] <- df[[di]]
  }
  
  # se hi nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(hi) || is.null(hi) || is.na(hi) || hi == "" ){
    
  }else if(!is.character(hi)){
    stop("'hi' must be a character containing a variable name", call.=F)
  }else if(length(hi)!=1){
    stop("Length of 'hi' must be 1", call.=F)
  }else if(check_names(df, hi)==F){
    
  }else{
    dfmod[["hi"]] <- df[[hi]]
  }
  
  # se e_casca nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(e_casca) || is.null(e_casca) || is.na(e_casca) || e_casca == "" ){
    
  }else if(!is.character(e_casca)){
    stop("'e_casca' must be a character containing a variable name", call.=F)
  }else if(length(e_casca)!=1){
    stop("Length of 'e_casca' must be 1", call.=F)
  }else if(check_names(df, e_casca)==F){
    
  }else{
    dfmod[["e_casca"]] <- df[[e_casca]]
  }
  
  # se comp_secao nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(comp_secao) || is.null(comp_secao) || is.na(comp_secao) || comp_secao == "" ){
    
  }else if(!is.character(comp_secao)){
    stop("'comp_secao' must be a character containing a variable name", call.=F)
  }else if(length(comp_secao)!=1){
    stop("Length of 'comp_secao' must be 1", call.=F)
  }else if(check_names(df, comp_secao)==F){
    
  }else{
    dfmod[["comp_secao"]] <- df[[comp_secao]]
  }
  
  # se ano nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(ano) || is.null(ano) || is.na(ano) || ano=="" ){  
    
  }else if( !is.character(ano) ){
  }else if(length(ano)!=1){
    stop("Length of 'ano' must be 1", call.=F)
  }else if(check_names(df, ano)==F){
    
  }else{
    dfmod[["ano"]] <- df[[ano]]
  }
  
  # se custo nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(custo) || is.null(custo) || is.na(custo) || custo=="" ){  
    
  }else if( !is.character(custo) ){
  }else if(length(custo)!=1){
    stop("Length of 'custo' must be 1", call.=F)
  }else if(check_names(df, custo)==F){
    
  }else{
    dfmod[["custo"]] <- df[[custo]]
  }
  
  # se receita nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(receita) || is.null(receita) || is.na(receita) || receita=="" ){  
    
  }else if( !is.character(receita) ){
  }else if(length(receita)!=1){
    stop("Length of 'receita' must be 1", call.=F)
  }else if(check_names(df, receita)==F){
    
  }else{
    dfmod[["receita"]] <- df[[receita]]
  }
  
  # se taxa.a.a nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(taxa.a.a) || is.null(taxa.a.a) || is.na(taxa.a.a) || taxa.a.a=="" ){  
    
  }else if(is.numeric(taxa.a.a) & length(taxa.a.a)==1){
    
    dfmod[["taxa.a.a"]] <- taxa.a.a
    
  }else{
    
  }
  
  # ####
  dfmod$XX <- NULL
  return(dfmod)
  
}
