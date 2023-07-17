

attributes_words <- c( "arquitectonico",  "automatizado",  "clasica",  "clasico",
                      "cluab",  "club hause",  "club hou",  "club house",  
                      "deplex",  "dupkex",  "duples",  "duplex",  "estrato",  
                      "exterio",  "exterior",  "interior",  "interna",  
                      "penthouse",  "penthouses",  "reserva",  "reservado",  
                      "reservas", "dueplex", "externa", "mixto", "pethouse",  
                      "abierta",  "acabados",  "alcoba",  "amoblado",  
                      "ascensor",  "balcon",  "banos",  "basement",  
                      "bathroom",  "bifamiliar",  "bodega",  "cerrada",  
                      "chimenea",  "cocina",  "comedor",  "cubierta",  
                      "cubierto",  "deposito",  "disenar",  "diseno",  
                      "dotado",  "espacios",  "estudi",  "familiar",  
                      "habitacion",  "habitacional",  "barato",  "buenisimo", 
                      "descuento",  "directa",  "directo",  "estrategico",  
                      "estrena",  "estrenar",  "estrene",  "estrrenar",  
                      "extrenar",  "ganga",  "gangazo",  "inigualable",  
                      "inmejorable",  "inversion",  "inversionista",  
                      "inversionistas",  "izacion",  "negocio",  "oferta",  
                      "oportinidad",  "oportunidad",  "permuta",  "permutemos",  
                      "permuto",  "acogedor",  "acogedora",  "ampli",  
                      "ampliada",  "amplio",  "amplios",  "amplitud",  
                      "apacible",  "comodo", "confort",  "espectacular",  
                      "estupendo",  "excelent",  "excelentes",  "excepcional",  
                      "exclente",  "exclusivo",  "exclusivos",  "exelente",  
                      "expectacular",  "fabulosa",  "fabuloso",  "fantastica",  
                      "fantastico",  "grandioso",  "hermosa",  "comerciales",  
                      "exclusivo sector",  "ubicación",  "ubicada",  
                      "ubicadisima",  "ubicadisimo",  "ubicadoen", "localizada",  
                      "aapartamento",  "aparataestudio",  "aparatamento",  
                      "aparatmento",  "aparramento",  "apartaemnto",  
                      "apartaestudio",  "apartahotel",  "apartamanet",  
                      "apartamanto",  "apartamemto",  "apartamenrto",  
                      "apartamento",  "apartamentoen",  "apartamentos",  
                      "apartamentro",  "apartameto",  "apartamneto",  
                      "apartarmento",  "apartartamento",  "apartmento",  
                      "apartments",  "aparto",  "apataestudio",  "apatamento",  
                      "aprtamento", "apartasuite", "apartaento", "apartasuite"
)

attribute_design <- c("arquitectonico", "automatizado", "clasica", "clasico", "cluab", 
                      "clubhause", "clubhou", "clubhouse", "deplex", "dupkex", 
                      "duples", "duplex", "estrato", "exterio", "exterior", 
                      "interior", "interna", "penthouse", "penthouses", "reserva", 
                      "reservado", "reservas", "dueplex", "externa", "mixto", 
                      "pethouse", "familiar", "independientes", "loft", "multifamiliar", 
                      "independiente", "unifamiliar", "bifamiliar"
)


attribute_space	<- c("abierta", "acabados", "alcoba", "amoblado", "ascensor", 
                         "balcon", "banos", "basement", "bathroom", "bodega", 
                         "cerrada", "chimenea", "cocina", "comedor", "cubierta", 
                         "cubierto", "deposito", "estudi", "habitacion", "habitacione", 
                         "habitaciones", "habs", "habt", "individual", "jacuzzi", 
                         "marmol", "panoramica", "panoramico", "parqueadero", "parqueaderos", 
                         "parqueo", "parqueos", "piso", "pisomhab", "salacomedor", 
                         "sauna", "walkin", "aticos", "balconesamplios", "domotica", 
                         "lavanderia", "piscina" 
)


attribute_business <- c("barato", "buenisimo", "descuento", "directa", "directo", 
                        "estrategico", "estrena", "estrenar", "estrene", "estrrenar", 
                        "extrenar", "ganga", "gangazo", "inigualable", "inmejorable", 
                        "inversion", "inversionista", "inversionistas", "izacion", "negocio", 
                        "oferta", "oportinidad", "oportunidad", "permuta", "permutemos", 
                        "permuto", "planos", "promocion", "proyecto", "proyectos", 
                        "rentabel", "rentabilidad", "rentabilisima", "rentable", "rentando", 
                        "subastamos", "venpermuta", "venpermuto", "invesion", "negociabl", 
                        "opciones", "oprtunidad", "remodelada", "inmobiliaria"
)


attribute_qualitative <- c("acogedor", "acogedora", "ampli", "ampliada", "amplio", 
                           "amplios", "amplitud", "apacible", "comodo", "confort", 
                           "espectacular", "estupendo", "excelent", "excelentes", "excepcional", 
                           "exclente", "exclusivo", "exclusivos", "exelente", "expectacular", 
                           "fabulosa", "fabuloso", "fantastica", "fantastico", "grandioso", 
                           "hermosa", "hermoso", "iluminacion", "iluminado", "iluminados", 
                           "ilumindo", "impecable", "impresionante", "increible", "increibles", 
                           "lindisima", "magnifico", "maravillosa", "maravilloso", "modenos", 
                           "moderno", "remodelacion", "remodelar", "renodelado", "tradicional", 
                           "urbanistica", "adecuada", "confortable", "elegante", "hermosos", 
                           "lindisimo", "premium", "privilegiada", "privilegiado", "tranquilo", 
                           "lujo", "lujosa", "lujoso", "luminoso", "luz", 
                           "ilumninado", "lumini", "disenar", "diseno", "estiloingles", 
                           "estilo", "comodisima" 
)


attribute_location <- c("comerciales", "exclusivosector", "ubicación", "ubicada", "ubicadisima", 
                        "ubicadisimo", "ubicadoen", "localizada"
)


attribute_kind_property	<- c("aapartamento", "aparataestudio", "aparatamento", "aparatmento", "aparramento", 
                             "apartaemnto", "apartaestudio", "apartahotel", "apartamanet", "apartamanto", 
                             "apartamemto", "apartamenrto", "apartamento", "apartamentoen", "apartamentos", 
                             "apartamentro", "apartameto", "apartamneto", "apartarmento", "apartartamento", 
                             "apartmento", "apartments", "aparto", "apataestudio", "apatamento", 
                             "aprtamento", "apt", "aptaestudio", "aptaestudios", "aptoe", 
                             "aptoestudio", "aptos", "casa", "casalote", "casas", 
                             "departamento", "estudioio", "hogar", "residencia", "vivienda", 
                             "viviendas", "aparamento", "apartaestudios", "apartamaneto", "apartamenti", 
                             "apartamentop", "apartanento", "apartastudio", "apartestudio", "apartoestudio", 
                             "apatartamento", "ape", "apryamento", "aptestudio", "casabodega", 
                             "quintas", "quinta", "apartasuite" , "apartaento", "apto"
)




