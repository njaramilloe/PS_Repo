#' *****************************************************************************
#' **********************REGULAR EXPRESSION ANALISIS****************************
#' *****************************************************************************


#----------------------prepare the environment----------------------------------
#clean global environment
rm(list = ls())

#load packages
require("pacman")

p_load(stringi, #manipulate string/text data in the cleaning process
       tokenizers, #convert natural language text into tokens
       stopwords, #low value commonly used words cleaning
       SnowballC, #apply stemming to each word reducing them to their root forms
       wordcloud, #takes words frequencies as input to create a cloud visualization
       tidyverse, #data manipulation and visualization
       tm, #text mining and natural language processing task
       rio, #import/export data file formats
       skimr, #summary data
       stargazer, #generate publication-quality tables
       RColorBrewer #color palettes for thematic maps
)


# set working directory
path_script <- rstudioapi::getActiveDocumentContext()$path
path_folder <- dirname(path_script)
setwd(path_folder)
setwd("../stores")

#----------------------loading data---------------------------------------------
# load training data
train <- read.csv("train.csv")

# load test data
test <- read.csv("test.csv")

# generate new variable that identifies the sample
test<-test %>% mutate(sample="test")
train<-train %>% mutate(sample="train")

# bind together both databases
total_table<-rbind(test,train)
table(total_table$sample) #test 10286 | train 38644


#----------------------data cleaning process------------------------------------

head(total_table$title)
head(total_table$description)

#' converts non-ASCII-range punctuation, symbols, and latin letters 
#' in an ASCII-range equivalent
total_table$title <- stri_trans_general(str = total_table$title, id = "Latin-ASCII")
total_table$description <- stri_trans_general(str = total_table$description, id = "Latin-ASCII")

# remove not alphanumeric text or spaces
total_table$title <- gsub('[^A-Za-z0-9 ]+', ' ', total_table$title)
total_table$description  <- gsub('[^A-Za-z0-9 ]+', ' ', total_table$description )

# be sure are lowercase letters
total_table$title <- tolower(total_table$title)
total_table$description <- tolower(total_table$description)

# remove multiple consecutive whitespace characters
total_table$title <- gsub('\\s+', ' ', total_table$title)
total_table$description <- gsub('\\s+', ' ', total_table$description)

# erase numbers from the title
total_table$title <- gsub("\\d+", "", total_table$title)
#total_table$description <- gsub("\\d+", "", total_table$description)

#  remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
total_table$title <- trimws(total_table$title)
total_table$description <- trimws(total_table$description)

#-----------------------erase meaningless words---------------------------------
#' use a list of stopwords from two libraries: tm and stopwords
#' this is to make a bigger list of words

#' tm library: 
#' create a vector of stopwords in spanish
tm_stopwords <- tm::stopwords("es")

#' stopwords library
#' create a list to store the stopwords from sources that supports spanish
st_stopwords1 <- stopwords::stopwords(language = "es", source = "snowball")
st_stopwords2 <- stopwords::stopwords(language = "es", source = "nltk")
st_stopwords3 <- stopwords::stopwords(language = "es", source = "stopwords-iso")

# join stopwords coming from different libraries and sources
stopwords <- list(tm_stopwords, st_stopwords1, st_stopwords2, st_stopwords3)
stopwords <- Reduce(union, stopwords)

# erase stopwords from title and description variable
total_table$title <- lapply(total_table$title, 
                            function(title) removeWords(title, stopwords))

total_table$description <- lapply(total_table$description, 
                                  function(description) removeWords(description, 
                                                                    stopwords))

# remove multiple consecutive whitespace characters
total_table$title <- gsub('\\s+', ' ', total_table$title)
total_table$description <- gsub('\\s+', ' ', total_table$description)

#  remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
total_table$title <- trimws(total_table$title)
total_table$description <- trimws(total_table$description)

#---------------------------tokenization----------------------------------------
#' tokenize both title and description splitting into individual words
#' each observation has a list of tokens
total_table$tokens_title <- tokenize_words(total_table$title)
total_table$tokens_descr <- tokenize_words(total_table$description)


#-----------------------stemming words------------------------------------------
## reduce to its roots each tokenized word
#total_table$tokens_title <- wordStem(total_table$tokens_title, "spanish")
#total_table$tokens_descr  <- wordStem(total_table$tokens_descr, "spanish")
#total_table$tokens_title[1:15]
#total_table$description[1:10]

#-----------------------words frequencies---------------------------------------
## make a table with word frequencies


frec_title <- total_table$title %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

frec_descr <- total_table$tokens_descr %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

#-----------------------delete repeated words-----------------------------------
## delete word without added value for the analysis

repeated_words <- c("bogota", "apartamento", "casa", "apartaestudio", "cod",
                    "vrei", "rah", "co", "vende", "fr", "vbcyf", "vbks", "mts",
                    "hermoso", "vcje", "estrenar", "vbire","vinh", "hahfa", 
                    "vpad", "mls", "rcj", "mm", "vmisx", "vcyf", "vpre", "vgcs",
                    "vbluqx", "duplex", "gangazo", "rcj", "zs", "vpad", "vgigx",
                    "vire", "calle", "amb", "dor", "vaebhx", "vcond", "cb", 
                    "club house", "vixo", "vx", "vjil", "vdur", "apto", "vxzu", 
                    "vkwcx", "visax", "habitaciones", "vcyc", "ap", "usaqun",
                    "vrcr", "vpos", "vgesx", "vcbr", "vosp", "vgue", "vgesx", 
                    "vabt", "eta", "vrcr", "vonex", "vedu", "vmpi", "lv", 
                    "usaquen", "local", "condominio", "excelente", "ubicación",
                    "penthouse", "ct", "cx", "art", "aparta estudio", "balcon", 
                    "chimenea", "tk", "autopista", "remodelado", "vbinp", 
                    "comercial", "piso", "hermosa", "linda", "lindo", "moderno", 
                    "rentable", "vbcbr", "mf", "wasix", "vluq", "vbimc", "crm", 
                    "inversionista", "ca", "banos", "paequeos", "xxmm", "ac", 
                    "inmuebles", "ov", "vgcsx", "vkwrx", "vunp", "av", "vitek", 
                    "vbrex", "departamento", "espectacular", "esquinero", 
                    "oportunidad", "exterior", "inversion", "vista montana", 
                    "parqueadero", "doble", "residencial", "vasv", "propiedad", 
                    "proyecto", "vrlf", "rentando", "vbidm", "mj", "bta", "cc", 
                    "vfor", "vcdi", "venpermuta", "productiva", "ubicada", 
                    "exclusiva", "zona", "acogedor", "alcobas", "amplia", "dc",
                    "amplio", "aparatamento", "districucion", "ubicacion", 
                    "app ", "aptv", "square", "loc", "suba", "terraza", "vsei", 
                    "vrcrx", "mhab", "ban", "parq", "depos", "aat", "vryg", 
                    "vcrs", "vrymx", "vass", "rq", "economico", "espacioso", 
                    "agrupacion", "vivivienda", "ganga", "grande", "frente",  
                    "dejes", "pasar", "exclusivo sector", "iluminado", "listn", 
                    "lote", "magnifico", "acabados", "niveles", "vird", "tipo", 
                    "ofrece", "apta", "estudio", "perfecto", "ph", "pisos",  
                    "mvmkl", "quintas", "re", "recien", "modelado", "renta",  
                    "proyectos", "id", "metros", "mh", "mt", "vgra", "vforx", 
                    "super", "vias", "acceso", "triplex", "unidad", "vendemos", 
                    "alcb", "venpermuto", "para", "ultima", "comodisima", "to", 
                    "modenos", "ideal", "compartir", "momentos", "familia",  
                    "vain", "pa", "loft", "alquiler", "altillo", "hbg", 
                    "industrial", "amplitud", "confort", "sx", "dt", "estudi",  
                    "aprovecha", "arrienda", "cbs", "sala", "tv", "vh", "vmak",  
                    "bedroom", "count", "xx", "garajes", "cubiertos", "garaje", 
                    "vbimp", "bomberos", "vojl", "divino", "bonita", "engativa", 
                    "cll", "callejn", "vbnc", "xxmlls", "yg", "vbre", "cas",  
                    "comerciales", "apartamentos", "carrera", "botanika", "edf",  
                    "depo", "ascensor", "pisohab", "vest", "vint", "vuag", "xf", 
                    "alcoba", "constructores", "privada", "bello", "gp", "dep", 
                    "mg", "xdt", "sg", "vm", "clubhouse", "comodo", "cra",   
                    "localizada", "ubicado", "comedor", "construidos", "house",  
                    "vrvi", "edificio", "entrega", "esquinera", "estrategica", 
                    "estrrenar", "estupendo", "espacios", "nivel", "principal", 
                    "vestie", "nales", "cuadra", "luz", "natural", "ve", "vbgu", 
                    "fontibon", "goza", "increible", "conjunto", "cerrado",  
                    "gara", "entreda", "bifamiliar", "proxima", "avdas",  
                    "inteligente", "interes", "cultural", "invierta",  
                    "aparta", "elefantes", "pasos", "mtrs", "via", "vehicular", 
                    "codigo", "lujo", "vgbxx", "maravilloso", "sec",  
                    "mixto", "vshi", "muebles", "luminoso", "multifamiliar", 
                    "candelariamiliar", "multifavende", "niteroi", "pen",  
                    "noveno", "obra", "gris", "oferta", "ofrecemos", "opcion", 
                    "aticos", "permuta", "permuto", "plan", "planos", "pleno", 
                    "punto", "bodega", "casas", "oficina", "amoblado", "cavv",
                    "bd", "locales", "ja", "csc", "hotelero", "inmueble",
                    "confortable", "ubicadisima", "mc donalls", "exclusivo",
                    "barrio", "hab", "ar", "acogedora", "bano", "bf", "alc",
                    "invesion", "xa", "vsii", "wok", "xxm", "gar", "res", 
                    "admirelo", "excelentes", "exelente", "iluminacion", 
                    "pent", "vmkl", "habitacion", "pra", "vkwc", "vkwr", 
                    "totalmente", "empleados", "rentado", "rentando", "vryg", 
                    "xxm", "visin", "piso", "arr", "hab", "vista", "barrio", 
                    "bifamiliar", "arr", "iluminado", "dp", "gj", "bn", "hb", 
                    "rem", "ind", "metros", "vta", "ic", "wr", "xa", "xm", 
                    "xp", "xx", "xxxm", "yg", "suba", "vbrex", "zjq", "zmh", 
                    "brp", "zpv", "avenidas", "habs", "casalote", "economica", 
                    "vhab", "vimp", "icr", "vcmp", "panoramica", "parqueaderos", 
                    "tza", "est", "hb", "cl", "club hause", "completamente", 
                    "ext", "vivienda", "viviendas", "oficinas", "cva", "gjs",
                    "gaia", "estrato", "er", "cva", "empresa", "aparramento", 
                    "aparamento", "aparataestudio", "aparatmento", "apartaemnto",
                    "apartaestudios", "apartahotel", "apartamanet", "pbg", 
                    "apartamaneto", "apartamanto", "apartamemto", "apartamenrto",
                    "apartamenti", "apartamentoen", "apartameto", "apartamneto",
                    "apartanento", "apartarmento", "apartartamento", 
                    "apartestudio", "subsidio", "vis", "apartmento", "aparto",
                    "apartoestudio", "apataestudio", "apatamento", 
                    "apatartamento", "apetecido", "appaptv", "nr", "aprtamento",
                    "apryamento", "apt", "aptaestudio", "aptaestudios", 
                    "aptestudio", "aptoe", "aptoestudio", "aptos", "ara", 
                    "apartamentop", "cuadras", "localidad", "diseno", "exito", 
                    "patrimonio", "cerrada", "reservado", "desarrollar", 
                    "elegante", "encantador", "fabuloso", "familiar", 
                    "unifamiliar", "inversionistas", "vbsei", "mx", "edif",
                    "oprtunidad", "constructor", "pisomhab", "vbaav", 
                    "remodelada", "remodelar", "rentabilisima", "reserva", "ed", 
                    "habit", "area", "vbrei", "vbaavx", "vgar", "sevende", 
                    "hbp", "vender", "contigua", "iguales", "caracteristicas", 
                    "fondo", "vic", "vzio", "vkas" , "fa", "actividad", 
                    "adecuada", "residencia", "estudiantil", "repuestos", 
                    "agradable", "clle", "xmrs", "unico", "hbp", "ppal", "pro", 
                    "recibo", "menor", "construido", "cr", "salacomedor", 
                    "viagx", "alquila", "alquilo", "fase", "iluminados", 
                    "generosos", "wasi", "ampliada", "ampli", "amplios", "an", 
                    "recursivo", "movilidad", "simicrm", "sistema", "veic", 
                    "vark", "areas", "arquitectonico", "vbaavx", "arrendado", 
                    "arrendo", "artek", "as", "automatizado", "mhabs", 
                    "auto norte", "autop", "autorpista", "ave", "avenida", 
                    "vbsei", "vzio", "barato", "menor", "recibo", "camioneta" ,
                    "proponga", "estd", "indep", "vsea", "barrios reconocidos",
                    "propongan", "basement", "basico", "kre", "mc", "vrpr", 
                    "vtli", "bathroom", "ad", "vr", "deposito", "inmejorable",
                    "lavanderia", "mu", "parqueo", "parqueos", "servicio", 
                    "resort", "vaav", "vacio", "vkia", "vugo", "inigualable", 
                    "oia", "cluab", "hbs", "promocion", "vaind", "bg", 
                    "bioclimatica", "bis", "vtli", "reserva", "ygo", "busca", 
                    "especial", "building", "age", "esplendido", "pqs", "bog",
                    "alta calidad", "urbanistica", "hbgcbs", "varkx", "vald",
                    "vmas", "vmub", "vreyx", "turismo", "cw", "aaa", "habt",
                    "servicios", "vabtx", "aceptamos", "mensuales", "veic", 
                    "casabodega", "muelle", "casacon", "cascada", 
                    "balcones amplios", "recibo", "voro", " agenda cita", 
                    "alta izacion", "alta seguridad", "cocina", "abierta", 
                    "terceria", "impecable", "vayh", "mbanos", "mill", 
                    "alrededores", "msl", "na", "novena", "carulla", "pal", 
                    "piscina", "renoba", "renodelado", "vjvs", 
                    "ultimas unidades", "ubicadoen", "ubicadisimo", "vyuxjx", 
                    "vgig", "vboyx", "rm", "rentar", "remodelacion", "parcial",
                    "pm", "peq", "panoramico", "fabulosa", "fantastico", 
                    "externa", "excepcional", "estrene", "estrena", "estrategico",
                    "estilo", "vaavx", "vqub", "academia academia", "usos", 
                    "vbinh", "vbprv", "vbyp", "vjvs", "vmur", "vsbi", "vrhc", 
                    "locusaquen","vrhc", "centenariocasa", "vaavx", "mlsx", 
                    "vbcmp", "vbyp", "ambientes airbnb", "call", "vbsumx", "ta",
                    "amp", "ava", "cedo deuda", "pr", "vbbgg", "acepto", "estilo",
                    "ilumninado", "clasico", "exclente", "social", "inde", 
                    "pido", "exclusivos", "hotel dann", "mtr", "vinp", "visa", 
                    "rentas", "cortas", "vqub", "vprex", "remod", "remodelacion",
                    "superoportunidad", "sitio", "tradicional", "ub", "vaavx",
                    "desarrollo", "urbano", "sauna", "aeropuerto", "viaa", 
                    "vmat", "previa", "clasica", "cn", "vbiei", "completo", 
                    "complejo", "consultorios medicos", "consta", "construir",
                    "corazon universitario negocios", "completisimo", "cs",  
                    "prev", "csa", "cub", "cubierto", "deplex", 
                    "detalles clientes relacionados clientes posibles documentos", 
                    "comentarios historial", "directa", "descuento", "directo", 
                    "disenada colegio pre escolar", "distribucion", "dotado", 
                    "dueplex", "duples", "dvino", "edificacion", "edifico", "sj", 
                    "ejecutivo", "triple", "constructura", "embargada", "endo", 
                    "alcobadesercio", "negocio acreditado", "espera negocio", 
                    "estilo ingles", "expectacular", "fantastica", "0", 
                    "engativ", "fontibn", "fontibon", "formidable", "frentes", 
                    "etp dorado industr", "residen dorado engat", "grandioso", 
                    "const", "grill", "carros", "gv", "habi", "habitacional", 
                    "habitat", "conj santana", "bofgota colombia", "hermosas", 
                    "hermosos", "hh", "ia", "ilumindo", "increibles", "inde", 
                    "independiente", "independientes", "individual", "interior", 
                    "activa", "pausa", "salida", "soleada", "jacuzzi", "walk in", 
                    "mz", "klim", "estudioio", "lido", "lindisima", "marmol", 
                    "lindisimo", "lino", "lm", "cel", "ba", "lujosa", "lujoso", 
                    "apartastudio", "goa", "lounge", "manobogv", "mph", 
                    "negociabl", "mmill", "neg", "acondicionada", "stc", 
                    "inmobiliaria", "mlsx", "clla", "motivo", "viaje", 
                    "consentido", "vmjy", "disenar", "gusto", "oportinidad", 
                    "nal", "interna", "negocio", "vcondx", "wa", "ubica", 
                    "aeropuerto dorado ciudad", "locengativa", "ig", "ntr", 
                    "ob", "obtener", "ocasion", "ofrezco", "om", "negociacion", 
                    "opciones", "vyas", "panoramico", "pap", "ta", "exterio", 
                    "vgig", "penthouses", "tierra caliente", "permutemos", 
                    "pethouse", "plantas", "cubierta", "aeropuerto come", "pm", 
                    "pq", "pr", "predio", "principales", "comercio", "privado", 
                    "privilegiada", "facilidades", "educacion", "economia naranja", 
                    "actividades", "usos", "promesa", "qvi", "recibe", "escuchamos", 
                    "ofertas", "xxx", "completa", "rentabel", "rentabilidad", 
                    "rento", "mterraza", "wc", "mterr", "club hou", "reservas", 
                    "resid", "restaurantes", "rf", "buenisimo", "rcc", "rmodelar", 
                    "abajo", "circunv", "circunvalar", "ava", "cir", "diag", 
                    "lumini", "mejores", "rv", "sa", "mlc", "adarves", 
                    "perfectas", "condiciones", "escucha", "reconocido", "vbyp", 
                    "cv", "apropiada", "geriatrico", "hogar", "apartamentro", 
                    "domotica", "especiales", "mtr", "vcmc", "cjs", "mtshabs", 
                    "premium", "amp", "aplica", "excelent", "habitacione", 
                    "mlls", "extrenar", "magnifica", "maravillosa", "vici", 
                    "vicibigv", "vrci", "opc", "peq", "rentar", "rm", "vicibogv", 
                    "ape", "bogta", "cuart", "impresionante", "naturalmente", 
                    "sc", "ronda", "virtual", "princi", "izacion", "sen", 
                    "privilegiado", "vbrci", "wow", "ygqq", "seleccionar", 
                    "acfm", "situado", "multiples", "sonado", "akcb", "spto", 
                    "anos", "dupkex", "serv", "subastamos", "suit", "suite", 
                    "terminada", "apartments", "vpai", "tradicional", 
                    "tranquilo", "apacible", "ubicadisimo", "ubicadoen", 
                    "ultimas unidades", "unir", "usada", "prospecto", "recibir", 
                    "hotelera", "utilizacion", "vac", "vebdo", "veendo", "ven", 
                    "vendido", "vene", "vente", "vevta", "garage", "vbiex", 
                    "virtuoso", "vjvs", "vneta", "zapatos", "aapartamento") 
                    
total_table$title <- lapply(total_table$title, 
                            function(title) removeWords(title, repeated_words))

# remove multiple consecutive whitespace characters
total_table$title <- gsub('\\s+', ' ', total_table$title)

# remove leading and trailing whitespace (spaces, tabs, newlines, etc.)
total_table$title <- trimws(total_table$title)

#-----------------------wordcloud graphic---------------------------------------
## Loading required package: RColorBrewer
frec_title <- total_table$title %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

p_load(xlsx)
write.xlsx(frec_title, file = "frec_results.xlsx")  

frec_descr <- total_table$tokens_descr %>%
  table() %>%
  data.frame() %>%
  rename("Word" = ".") %>%
  arrange(desc(Freq))

set.seed(666) 
png(filename = "wordcloud_title.png", width = 1000, height = 1000)
wordcloud(words = frec_title$Word, freq = frec_title$Freq, scale=c(16,2),
          min.freq = 1, max.words = 200, random.order=  FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))
dev.off()
getwd()
