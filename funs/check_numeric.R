# Funcao para testar se uma variavel e numerica
# Sera utilizada dentro da funcao validate

check_numeric <- function(input, df, code){
  
  if(is.null(input) ){
    
    
  }else if(is.na(input)){
    
    
  }else if(input == ""){
    
  }else if(!is.null(input) && !is.numeric(df[[input]]) ){
    
    paste(code, "column must be numeric")
    
  }
  
}
