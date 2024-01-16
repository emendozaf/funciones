guardadf <- function(dataframes,folder="", home="Yes"){
    #' guardadf
    #' Gaarda los dataframes listados  en la carpeta designada
    #' @param lista lista de dataframes en string.
    #'        Allowed values: "objeto1" o c("objeto1", "objeto2")
    #' @param folder string con la ruta donde se desea guardar. 
    #' @param home "description"Yes" implica que 'folder' son subcarpetas de el working directory. Si es igual a no, 'folder' se considerará como la ruta completa.
    #'@author: emendoza
    if(home=="No"){
        path <- folder
    } else{
        path <- if(folder == "") "" else paste0(folder, "/")
    }
    path <- gsub("//", "/", path)
    
    if (dir.exists(paste0(getwd(),"/",path))) {
        cat(paste0("Objects will be saved to '",if(folder == "") "working directory" else path,"' \n"))
    } else {
        stop("Directory does not exist. No object was saved")
    }
    
    for (df in dataframes) {
        if(exists(df)){
            saveRDS(get(df),file = paste0(path,df,".Rds"))
            write.table(get(df), paste0(path,df,".csv"),row.names = F, sep = "|")
            cat(paste0("File '",df,"' saved\n"))
        } else{
            cat(paste0("Object '",df, "' does not exist in the environment and cannot be saved.\n"))
        }
    }
}
