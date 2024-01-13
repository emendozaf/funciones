#' guardadf
#' Gaarda los dataframes listados  en la carpeta designada
#' @param lista lista de dataframes.
#'        Allowed values: "objeto1" o c("objeto1", "objeto2")
#' @param param2 Description of the second parameter.
#'        Allowed values: Value A, Value B, Value C.
#'@author: emendoza

guardadf <- function(dataframes,folder="", home="Yes"){
    if(home=="No"){
        path=folder
    } else{
        path=paste0(folder,"/")
    }
    for (df in dataframes) {
        if(exists(df)){
            save(list=df,file = paste0(path,df,".Rda"))
            write.table(get(df), paste0(path,df,".csv"),row.names = F, sep = "|")
        } else{
            cat(paste("Object",df, "does not exist in the environment and cannot be saved.\n"))
        }
    }
}