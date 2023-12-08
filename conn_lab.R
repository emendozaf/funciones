######################################################
###
### Crea la conexión para subir bases de datos al Data Lake
###
#######################################################

# conn_lab permite conectarse al data lake para subir archivos
conn_lab <- dbConnect(RPostgres::Postgres(),
                      dbname = "DL_LABORATORIO",
                      host = "MLM4156",
                      port = 6432,
                      user = "usr_laboratorio",
                      password = "l4bor@torio2023")