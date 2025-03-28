#include <string.h>

#include <libxml/xmlmemory.h>
#include <libxml/encoding.h>
#include <libxml/parser.h>

#include "str.h"
#include "rmn/Dict.h"
#include "rmn/Meta.h"
#include <float.h>

char *TSHORT[]      = { "Description courte ","Short Description  " };
char *TLONG[]       = { "Description longue ","Long  Description  " };
char *TUNITES[]     = { "Unités             ","Units              " };
char *TDATE[]       = { "Date du status     ","Date of state      " };
char *TORIGIN[]     = { "Origine            ","Origin             " };
char *TSTATE[]      = { "Status             ","State              " };
char *TTYPE[]       = { "Représentation     ","Representation     " };
char *TMAG[]        = { "Ordre de grandeur  ","Magnitude          " };
char *TPREC[]       = { "Précision requise  ","Required precision " };
char *TPACK[]       = { "Compaction optimale","Optimal compaction " };
char *TRANGE[]      = { "Amplitude          ","Range              " };

char *TINT[]        = { "Variable entière"  ,"Integer Variable" };
char *TREAL[]       = { "Variable réelle"   ,"Real Variable"    };
char *TLOGIC[]      = { "Variable logique"  ,"Logical Variable" };
char *TCODE[]       = { "Variable codée"    ,"Coded Variable"   };
char *TVAL[]        = { "Valeur"            ,"Value"            };

char *TOBSOLETE[]   = { "Obsolète"   ,"Obsolete"   };
char *TDEPRECATED[] = { "Désuète"    ,"Deprecated" };
char *TFUTURE[]     = { "Futur"      ,"Future"     };
char *TCURRENT[]    = { "Courante"   ,"Current"    };
char *TINCOMPLETE[] = { "Incomplète" ,"Incomplete" };

char *TCENTILE[]    = { "e centile"                   ,"th percentile" };
char *TMIN[]        = { "(minimum)"                   ,"(minimum)" };
char *TMAX[]        = { "(maximum)"                   ,"(maximum)" };
char *TSSTD[]       = { "(écart-type (population))"   ,"(standard deviation (population))" };
char *TPSTD[]       = { "(écart-type (échantillon))"  ,"(standard deviation (sample))" };
char *TMEAN[]       = { "(moyenne)"                   ,"(mean)" };
char *TEFI[]        = { "(index de prévision extrème)","(extreme forecast index)" };
char *TBETWEEN[]    = { "entre","between" };
char *TAND[]        = { "et","and" };
char *TDAY[]        = { "le jour","day" };
char *THOUR[]       = { "l'heure","hour" };

typedef struct {
   char          *Name,*Date,*Version,String[64];     // Dictionary metadata
   TList         *Vars;                               // List of dictionary variables
   TList         *Types;                              // List of dictionary types
} TDict;

typedef struct {
   char          *Modifier;                           // Variable modifier (ETIKET)
   int            Mode;                               // Search mode (DICT_EXACT,DICT_GLOB)
   int            State;                              // Search state (DICT_OBSOLETE,DICT_CURRENT,DICT_FUTURE,DICT_INCOMPLETE)
   char          *Origin;                             // Search origin
   char          *ETIKET;                             // Search etiket
   int            IP1,IP2,IP3;                        // IP to look for
   int            AltIP1,AltIP2,AltIP3;               // Alternate IP to look for (OLD/NEW)
} TDictSearch;

static          TDict       Dict;                     // Global dictionary
static __thread TDictSearch DictSearch;               // Per thread search params

static int Dict_ParseVar(xmlDocPtr Doc,xmlNsPtr NS,xmlNodePtr Node,TDict_Encoding Encoding);
static int Dict_ParseType(xmlDocPtr Doc,xmlNsPtr NS,xmlNodePtr Node,TDict_Encoding Encoding);

static void strncpy0(char *restrict Dest,char *restrict Src,size_t Num) {
   strncpy(Dest,Src,Num-1);
   Dest[Num-1]='\0';
}

int Dict_Load(TDict_Encoding Encoding) {
   
   char *c=NULL;
   char  path[APP_BUFMAX];

   if (!Dict.Version) {
      if (!(c=getenv("CMCCONST"))) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Environment variable CMCCONST not defined, source the CMOI base domain\n",__func__);
         return(FALSE);
      }

      snprintf(path,APP_BUFMAX, "%s%s",c,"/opdict/ops.variable_dictionary.xml");
      if (!Dict_Parse(path,Encoding)) {
         Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Unable to read dictionnary file %s\n",__func__,path);
         return(FALSE);
      }
      Lib_Log(APP_LIBMETA,APP_INFO,"Loaded %s\n",Dict_Version());
   }
   return(TRUE);
}

static void Dict_Encoding(char *string,TDict_Encoding Encoding) {

   char tmpString[DICT_MAXLEN];
   int  tmplenout, tmplenin;
   int  i,i2;

   tmplenin = strlen(string);
   tmplenout = tmplenin;
   memset(tmpString,'\0',DICT_MAXLEN);

   switch (Encoding) {

      case DICT_ASCII:
         i= i2=0;
         while (i<tmplenout && i2<DICT_MAXLEN-1) {

            switch ((unsigned char)string[i]) {
               case 0xE2:
                  i++;
                  i++;
                  switch ((unsigned char) string[i]) {
                     case 0xBB:
                        tmpString[i2] = '-';
                        break;
                  }
                  break;
                 
               case 0xC2:
                  i++;
                  switch ((unsigned char) string[i]) {
                     case 0x2D:
                        tmpString[i2] = '-';
                        break;
                        
                     case 0xB0:
                        tmpString[i2] = ' ';
                        break;

                     case 0xB2:
                        tmpString[i2] = '2';
                        break;

                     case 0xB3:
                        tmpString[i2] = '3';
                        break;

                     case 0xB5:
                        tmpString[i2] = 'u';
                        break;
                        
                     case 0xB9:
                        tmpString[i2] = '1';
                        break;
                  }
                  break;

               case 0xC3:
                  i++;
                  
                  switch ((unsigned char) string[i]) {
                     case 0xB5:
                        tmpString[i2] = 'u';
                        break;

                     case 0x80:
                     case 0x81:
                     case 0x82:
                        tmpString[i2] = 'A';
                        break;

                     case 0x88:
                     case 0x89:
                     case 0x90:
                        tmpString[i2] = 'E';
                        break;

                     case 0xA0:
                     case 0xA1:
                     case 0xA2:
                        tmpString[i2] = 'a';
                        break;

                     case 0xA7:
                        tmpString[i2] = 'c';
                        break;

                     case 0xA8:
                     case 0xA9:
                     case 0xAA:
                     case 0xAB:
                        tmpString[i2] = 'e';
                        break;

                     case 0xEE:
                     case 0xAF:
                        tmpString[i2] = 'i';
                        break;

                     case 0xB4:
                     case 0xB6:
                        tmpString[i2] = 'o';
                        break;

                     case 0xB9:
                     case 0xBA:
                     case 0xBB:
                        tmpString[i2] = 'u';
                        break;

                     default:
                        break;
                  }
                  break;

               default:
                  tmpString[i2] = string[i];
                  break;
            }
            i++;
            i2++;
         }
         strcpy(string,tmpString);
         break;

      case DICT_ISO8859_1:
         tmplenout=tmplenin;
         UTF8Toisolat1((unsigned char *)tmpString,&tmplenout,(const unsigned char *)string,&tmplenin);
         strcpy(string,tmpString);
         break;

      case DICT_UTF8:
         break;
    }
}

char* Dict_Version(void) {
   return(Dict.String);
}

static int Dict_ParseText(char *Dest,xmlDocPtr Doc,xmlNodePtr Node,TDict_Encoding Encoding,int Len,int NoLangDefault) {
   xmlChar *str=NULL,*lang=NULL;
   int code=0;

   if( !xmlIsBlankNode(Node) && (str=xmlNodeListGetString(Doc,Node->children,1)) ) {
      if( (lang=xmlGetProp(Node,(const xmlChar *)"lang")) ) {
         int i=-1;
         if (     !strcmp((char*)lang,"fr")) { i=0; }
         else if( !strcmp((char*)lang,"en")) { i=1; }
         xmlFree(lang);

         if( i != -1 ) {
            i *= Len;
            strncpy0(Dest+i,(char *)str,Len);
            Dict_Encoding(Dest+i,Encoding);
            code=1;
         }
      } else if( NoLangDefault ) {
         strncpy0(Dest,(char *)str,Len);
         Dict_Encoding(Dest,Encoding);
         strncpy0(Dest+Len,Dest,Len);
         code=1;
      }

      xmlFree(str);
   }

   return code;
}

inline static void IPDecode(int IP,float *Level,int *Kind) {

   if( IP>0 ) {
      int flag=0,mode=-1;
      char fmt;

      // Convert to real level/value
      f77name(convip_plus)(&IP,Level,Kind,&mode,&fmt,&flag,1);
   } else {
      *Level = -1.0f;
      *Kind = -1;
   }
}

inline static int IPEncode(float Level,int Kind,int New) {
   int ip=-1;

   int flag=0,mode=New?2:3;
   char fmt;

   // Encode to IP
   f77name(convip_plus)(&ip,&Level,&Kind,&mode,&fmt,&flag,1);

   return ip;
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_SetSearch>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Define search params
 *
 * Parametres  :
 *  <SearchMode>   :
 *  <SearchState>  :
 *  <SearchOrigin> :
 *  <SearchIP1>    :
 *  <SearchIP2>    :
 *  <SearchIP3>    :
 *
 * Retour:
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
void Dict_SetSearch(int SearchMode,int SearchState,char *SearchOrigin,int SearchIP1,int SearchIP2,int SearchIP3,char *SearchETIKET) {

   int    type;
   float  level=0.0;

   DictSearch.State=SearchState;
   DictSearch.Mode=SearchMode;
   DictSearch.Origin=SearchOrigin;
   DictSearch.IP1=SearchIP1;
   DictSearch.IP2=SearchIP2;
   DictSearch.IP3=SearchIP3;
   DictSearch.ETIKET=SearchETIKET;

   if (DictSearch.IP1>0) {
      // Convert to real level/value
      IPDecode(DictSearch.IP1,&level,&type);
      // Get alternate representation (OLD/NEW)
      DictSearch.AltIP1 = IPEncode(level,type,DictSearch.IP1<=32767);
   }
}

void Dict_SetModifier(char *Modifier) {

   if (DictSearch.Modifier)
      free(DictSearch.Modifier);

   DictSearch.Modifier=NULL;

   if (Modifier)
      DictSearch.Modifier=strdup(Modifier);
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_Parse>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Read and parse an XML dictionary file.
 *
 * Parametres  :
 *  <Filename> : XML dictionary file
 *  <Encoding> : Encoding mode (DICT_ASCII,DICT_UTF8,DICT_ISO8859_1)
 *
 * Retour:
 *
 * Remarques :
 *    - Based heavily on r.dict code
 *----------------------------------------------------------------------------
*/
int Dict_Parse(char *Filename,TDict_Encoding Encoding) {

   xmlDocPtr    doc  = NULL;
   xmlNsPtr     ns   = NULL;
   xmlNodePtr   node = NULL;
   xmlChar     *tmpc = NULL;

   xmlDoValidityCheckingDefaultValue=1;
   LIBXML_TEST_VERSION
//   xmlKeepBlanksDefault(0);

   // Build the XML tree from the file
   if (!(doc=xmlParseFile(Filename))) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid dictionary file: %s\n",__func__,Filename);
      return(0);
   }

   // Check the document is of the right kind
   if (!(node = xmlDocGetRootElement(doc))) {
      Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Empty document\n",__func__);
      xmlFreeDoc(doc);
      return(0);
   }

   Dict.Name=NULL;

   tmpc=xmlGetProp(node,(const xmlChar *)"date");
   Dict.Date=strdup((const char *)tmpc);
   xmlFree(tmpc);

   tmpc=xmlGetProp(node,(const xmlChar *)"version_number");
   Dict.Version=strdup((const char *)tmpc);
   xmlFree(tmpc);

   if( (tmpc=xmlGetProp(node,(const xmlChar *)"name")) ) {
      Dict.Name=strdup((const char *)tmpc);
      xmlFree(tmpc);
      sprintf(Dict.String,"%s %s %s version %s",node->name,Dict.Name,Dict.Date,Dict.Version);
   } else {
      sprintf(Dict.String,"%s %s version %s",node->name,Dict.Date,Dict.Version);
   }

  // Parse the DTD
//    strcpy(dtdfile,"");
//    strcat(dtdfile,getenv("AFSISIO"));
//    strcat(dtdfile,"/datafiles/constants/dict-2.0.dtd");
//
//    if (!(dtd=xmlParseDTD(NULL,dtdfile))) {
//       Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Could not parse DTD %s\n",__func__,dtdfile);
//       return (1);
//    }
//
//    // Set up xmlValidCtxt for error reporting when validating
//    ctxt.userData = stderr;
//    ctxt.error    = (xmlValidityErrorFunc) fprintf;   // register error function
//    ctxt.warning  = (xmlValidityWarningFunc) fprintf; // register warning function
//
//    if (!xmlValidateDtd(&ctxt,doc,dtd)) {
//       Lib_Log(APP_LIBMETA,APP_ERROR,"%s: DTD validation error\n",__func__);
//       return (0);
//    }

   node=node->children;

   // Now, walk the tree
   char ok = 1;
   while (node) {

      if (!strcmp((const char *)node->name,"metvar")) {
         if (!(ok=Dict_ParseVar(doc,ns,node,Encoding))) break;
      }

      if (!strcmp((const char *)node->name,"typvar")) {
         if (!(ok=Dict_ParseType(doc,ns,node,Encoding))) break;
      }

      node=node->next;
   }

//    xmlFreeDtd(dtd);
    xmlFreeDoc(doc);

    return(ok);
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_ParseVar>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Read and parse an XML node nofr NOMVAR info.
 *
 * Parametres  :
 *  <Doc>      : XML document
 *  <NS>       :
 *  <Node>     : XML node to parse
 *  <Encoding> : Encoding mode (DICT_ASCII,DICT_UTF8,DICT_ISO8859_1)
 *
 * Retour:
 *
 * Remarques :
 *    - Based heavily on r.dict code
 *----------------------------------------------------------------------------
*/
static int Dict_ParseVar(xmlDocPtr Doc,xmlNsPtr NS,xmlNodePtr Node,TDict_Encoding Encoding) {

   TDictVar  *metvar;
   xmlNodePtr trotteur,trotteur1;
   xmlChar   *tmpc;
   int        i,y,m,d;

   metvar=(TDictVar*)calloc(1,sizeof(TDictVar));
   metvar->IP1=metvar->IP2=metvar->IP3=metvar->Pack=-1;
   metvar->Min=metvar->Max=metvar->Magnitude=metvar->Precision=DICT_NOTSET;
   metvar->Level=FLT_MAX;
   metvar->Kind=-1;

   if ((tmpc=xmlGetProp(Node, (const xmlChar *)"origin"))) {
      strncpy0(metvar->Origin,(char *)tmpc,32);
      xmlFree(tmpc);
   }

   if ((tmpc=xmlGetProp(Node,(const xmlChar *)"pack"))) {
      metvar->Pack=atoi((const char*)tmpc);
      xmlFree(tmpc);
   }

   if ((tmpc=xmlGetProp(Node,(const xmlChar *)"usage"))) {
      if (!strcmp((const char*)tmpc,"obsolete")) {
         metvar->Nature|=DICT_OBSOLETE;
      } else if (!strcmp((const char*)tmpc,"deprecated")) {
         metvar->Nature|=DICT_DEPRECATED;
      } else if (!strcmp((const char*)tmpc,"current")) {
         metvar->Nature|=DICT_CURRENT;
      } else if (!strcmp((const char*)tmpc,"future")) {
         metvar->Nature|=DICT_FUTURE;
      } else if (!strcmp((const char*)tmpc,"incomplete")) {
         metvar->Nature|=DICT_INCOMPLETE;
      }
      xmlFree(tmpc);
   }

   if ((tmpc=xmlGetProp(Node,(const xmlChar *)"date"))) {
      sscanf((const char *)tmpc,"%u-%u-%u",(unsigned int *)&y,(unsigned int *)&m,(unsigned int *)&d);
      metvar->Date=Meta_DateTime2Seconds(y,m,d,0,0,0,TRUE);
      xmlFree(tmpc);
   }

   Node=Node->children;
   while (Node) {

      if (!strcmp((char*)Node->name,"nomvar")) {
         if( !(tmpc=xmlNodeListGetString(Doc,Node->children,1)) ) {
            Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Empty variable definition\n",__func__);
            return(0);
         }
         strncpy0(metvar->Name,(char *)tmpc,5);
         xmlFree(tmpc);
         if ((tmpc=xmlGetProp(Node,(const xmlChar *)"ip1")) != NULL) {
            metvar->IP1=atoi((const char *)tmpc);
            IPDecode(metvar->IP1,&metvar->Level,&metvar->Kind);
            xmlFree(tmpc);
         }
         if ((tmpc=xmlGetProp(Node,(const xmlChar *)"ip2")) != NULL) {
            metvar->IP2=atoi((const char *)tmpc);
            xmlFree(tmpc);
         }
         if ((tmpc=xmlGetProp(Node,(const xmlChar *)"ip3")) != NULL) {
            metvar->IP3=atoi((const char *)tmpc);
            xmlFree(tmpc);
         }
         if ((tmpc=xmlGetProp(Node,(const xmlChar *)"etiket")) != NULL) {
            strncpy0(metvar->ETIKET,(char *)tmpc,13);
            xmlFree(tmpc);
         }
         if ((tmpc=xmlGetProp(Node,(const xmlChar *)"level")) != NULL) {
            metvar->Level=atof((const char *)tmpc);
            xmlFree(tmpc);
         }
         if ((tmpc=xmlGetProp(Node,(const xmlChar *)"kind")) != NULL) {
            metvar->Kind=atoi((const char *)tmpc);
            xmlFree(tmpc);
         }
      } else

      if (!strcmp((char*)Node->name,"description")) {
         trotteur1=Node->children;
         while (trotteur1) {
            if ( !strcmp((char*)trotteur1->name,"short") ) {
               Dict_ParseText((char*)metvar->Short,Doc,trotteur1,Encoding,256,0);
            } else if ( !strcmp((char*)trotteur1->name,"long") ) {
               Dict_ParseText((char*)metvar->Long,Doc,trotteur1,Encoding,DICT_MAXLEN,1);
            }
            trotteur1=trotteur1->next;
         }
      } else

      if (!strcmp((char*)Node->name,"measure")) {
         trotteur=Node->children->next;

         // Integer
         if (!strcmp((char*)trotteur->name,"integer")) {
            metvar->Nature|=DICT_INTEGER;

            trotteur1=trotteur->children;
            while (trotteur1) {
               if( !xmlIsBlankNode(trotteur1) && (tmpc=xmlNodeListGetString(Doc,trotteur1->children,1)) ) {
                  if(      !strcmp((char*)trotteur1->name,"units")      ) { strncpy0(metvar->Units,(char *)tmpc,32); Dict_Encoding(metvar->Units,Encoding); }
                  else if( !strcmp((char*)trotteur1->name,"magnitude")  ) { metvar->Magnitude=atof((const char *)tmpc); }
                  else if( !strcmp((char*)trotteur1->name,"min")        ) { metvar->Min=atof((const char *)tmpc); }
                  else if( !strcmp((char*)trotteur1->name,"max")        ) { metvar->Max=atof((const char *)tmpc); }

                  xmlFree(tmpc);
               }

               trotteur1=trotteur1->next;
            }
         } else

         // Real
         if (!strcmp((char*)trotteur->name,"real")) {
            metvar->Nature|=DICT_REAL;

            trotteur1=trotteur->children;
            while (trotteur1) {
               if( !xmlIsBlankNode(trotteur1) && (tmpc=xmlNodeListGetString(Doc,trotteur1->children,1)) ) {
                  if(      !strcmp((char*)trotteur1->name,"units")      ) { strncpy0(metvar->Units,(char *)tmpc,32); Dict_Encoding(metvar->Units,Encoding); }
                  else if( !strcmp((char*)trotteur1->name,"magnitude")  ) { metvar->Magnitude=atof((const char *)tmpc); }
                  else if( !strcmp((char*)trotteur1->name,"min")        ) { metvar->Min=atof((const char *)tmpc); }
                  else if( !strcmp((char*)trotteur1->name,"max")        ) { metvar->Max=atof((const char *)tmpc); }
                  else if( !strcmp((char*)trotteur1->name,"precision")  ) { metvar->Precision=atof((const char *)tmpc); }

                  xmlFree(tmpc);
               }

               trotteur1=trotteur1->next;
            }
         } else

         // Logical
         if (!strcmp((char*)trotteur->name,"logical")) {
            strcpy(metvar->Units,"bool");
            metvar->Nature|=DICT_LOGICAL;

            trotteur1=trotteur->children;
            i=-1;
            while (trotteur1) {
               if( !xmlIsBlankNode(trotteur1) ) {
                  if (!strcmp((char*)trotteur1->name,"value") ) {
                     if( (tmpc=xmlNodeListGetString(Doc,trotteur1->children,1)) ) {
                        i=atoi((const char *)tmpc);
                        xmlFree(tmpc);
                     }
                  } else if( (i==0||i==1) && !strcmp((char*)trotteur1->name,"meaning") ) {
                     Dict_ParseText((char*)metvar->Meanings[i],Doc,trotteur1,Encoding,DICT_LEN_MEANING,1);
                  }
               }
               trotteur1=trotteur1->next;
            }
         } else

         // Code
         if (!strcmp((char*)trotteur->name,"code")) {
            strcpy(metvar->Units,"code");
            metvar->Nature|=DICT_CODE;

            trotteur1=trotteur->children;
            i=0;
            while (trotteur1) {
               if( !xmlIsBlankNode(trotteur1) ) {
                  if (!strcmp((char*)trotteur1->name,"value") ) {
                     if( (tmpc=xmlNodeListGetString(Doc,trotteur1->children,1)) ) {
                        metvar->Codes[i++]=atoi((const char *)tmpc);
                        xmlFree(tmpc);
                     }
                  } else if (!strcmp((char*)trotteur1->name,"meaning") ) {
                     Dict_ParseText((char*)metvar->Meanings[i-1],Doc,trotteur1,Encoding,DICT_LEN_MEANING,1);
                  }
               }
               trotteur1=trotteur1->next;
            }
            metvar->NCodes=i;
         }
      }

      Node=Node->next;
   }

   // Will be usefull for the search later on
   if( metvar->IP1==-1 && metvar->Kind!=-1 && metvar->Level!=FLT_MAX ) {
      metvar->IP1 = IPEncode(metvar->Level,metvar->Kind,1);
   }

   Dict.Vars=TList_AddSorted(Dict.Vars,Dict_SortVar,metvar);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_ParseType>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Read and parse an XML node for TYPVAR info.
 *
 * Parametres  :
 *  <Doc>      : XML document
 *  <NS>       :
 *  <Node>     : XML node to parse
 *  <Encoding> : Encoding mode (DICT_ASCII,DICT_UTF8,DICT_ISO8859_1)
 *
 * Retour:
 *
 * Remarques :
 *    - Based heavily on r.dict code
 *----------------------------------------------------------------------------
*/
static int Dict_ParseType(xmlDocPtr Doc, xmlNsPtr NS, xmlNodePtr Node,TDict_Encoding Encoding) {

   TDictType *type;
   xmlNodePtr trotteur1;
   xmlChar   *tmpc;
   int       y,m,d;

   type=(TDictType*)calloc(1,sizeof(TDictType));

   if ((tmpc=xmlGetProp(Node,(const xmlChar *)"origin")) != NULL) {
      strncpy0(type->Origin,(char *)tmpc,32);
      xmlFree(tmpc);
   }

   if ((tmpc=xmlGetProp(Node,(const xmlChar *)"usage")) != NULL) {
      if (!strcmp((const char *)tmpc,"obsolete")) {
         type->Nature|=DICT_OBSOLETE;
      } else if (!strcmp((const char *)tmpc,"deprecated")) {
         type->Nature|=DICT_DEPRECATED;
      } else if (!strcmp((const char *)tmpc,"current")) {
         type->Nature|=DICT_CURRENT;
      } else if (!strcmp((const char *)tmpc,"future")) {
         type->Nature|=DICT_FUTURE;
      } else if (!strcmp((const char *)tmpc,"incomplete")) {
         type->Nature|=DICT_INCOMPLETE;
      }
      xmlFree(tmpc);
   }

   if ((tmpc=xmlGetProp(Node,(const xmlChar *)"date")) != NULL) {
      sscanf((const char *)tmpc,"%u-%u-%u",&y,&m,&d);
      type->Date=Meta_DateTime2Seconds(y,m,d,0,0,0,TRUE);
      xmlFree(tmpc);
   }

   Node=Node->children;
   while (Node) {
      if (!strcmp((char*)Node->name,"nomtype")) {
         tmpc=xmlNodeListGetString(Doc,Node->children,1);
         strncpy0(type->Name,(char *)tmpc,3);
         xmlFree(tmpc);
      }

      // DESCRIPTION
      if (!strcmp((char*)Node->name,"description")) {
         trotteur1=Node->children;
         while (trotteur1) {
            if (!strcmp((char*)trotteur1->name,"short") ) {
               Dict_ParseText((char*)type->Short,Doc,trotteur1,Encoding,128,0);
            } else if (!strcmp((char*)trotteur1->name,"long") ) {
               Dict_ParseText((char*)type->Long,Doc,trotteur1,Encoding,DICT_MAXLEN,1);
            }

            trotteur1=trotteur1->next;
         }
      }

      Node=Node->next;
   }

   Dict.Types=TList_AddSorted(Dict.Types,Dict_SortType,type);

   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_AddVar>
 * Creation : Juin 2014 - J.P. Gauthier
 *
 * But      : Add a type.
 *
 * Parametres  :
 *  <Var>      : Variable to add to dictionary
 *
 * Retour:
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
void Dict_AddVar(TDictVar *Var) {
   Dict.Vars=TList_AddSorted(Dict.Vars,Dict_SortVar,Var);
}


//! Compare to variables to order them
int Dict_SortVar(
    //! [in] First variable to compare
    const void * const var0,
    //! [in] Second variable to compare
    const void * const var1
) {
    //! \return 0 if variables are equal, negative if var0 is smaller than var1, one or greater otherwise
    const TDictVar * const v0 = var0;
    const TDictVar * const v1 = var1;
    const int cmp = strcasecmp(v0->Name, v1->Name);

    // If we don't have a tie, then return the result of the comparison
    if (cmp) return cmp;

    // We are in tiebreaker mode, place any status that is current above other statuses
    // so that they are returned first when searching for that variable
    return (v1->Nature & DICT_STATE) - (v0->Nature & DICT_STATE);
}

//! Check if a var satisfies search parameters
int Dict_CheckVar(
    //! [in] First variable to compare
    const void * const var0,
    //! [in] Variable name. Handled as a glob pattern if DictSearch.Mode == DICT_GLOB
    const void * const str
) {
    //! \return 1 if search parameters are satisfied, 0 otherwise
    if (!var0) return 0;

    if (DictSearch.State && !(((TDictVar*)var0)->Nature & DictSearch.State)) return 0;

    if (DictSearch.Origin && strcasecmp(((TDictVar*)var0)->Origin, DictSearch.Origin)) return 0;

    if (DictSearch.ETIKET && strcasecmp(((TDictVar*)var0)->ETIKET, DictSearch.ETIKET)) return 0;

    if (DictSearch.IP1 > 0 && ((TDictVar*)var0)->IP1 != DictSearch.IP1 && ((TDictVar*)var0)->IP1 != DictSearch.AltIP1) return 0;

    if (DictSearch.IP2 > 0 && ((TDictVar*)var0)->IP2 != DictSearch.IP2) return 0;

    if (DictSearch.IP3 > 0 && ((TDictVar*)var0)->IP3 != DictSearch.IP3) return 0;

    if (DictSearch.Mode == DICT_GLOB) {
        return str ? (!strmatch(((TDictVar*)var0)->Name, (char*)str)) : 1;
    } else {
        return str ? (!strcasecmp(((TDictVar*)var0)->Name, (char*)str)) : 0;
    }
}


//! Search for a variable
TDictVar * Dict_GetVar(
    //! [in] Variable name
    const char * const varName
) {
    //! \return Variable if found, NULL otherwise
    TList * const node = TList_Find(Dict.Vars, Dict_CheckVar, varName);
    return node ? (TDictVar*)(node->Data) : NULL;
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_IterateVar>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Iterator over a list of TDictVar
 *
 * Parametres  :
 *  <ITerator> : List pointer start (NULL for beginning of list)
 *  <Var>      : Var to look for (if NULL, iterate over all)
 *
 * Retour:
 *  <TDictVar> : Variable info
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
TDictVar *Dict_IterateVar(TList **Iterator,char *Var) {

   TDictVar *var=NULL;

   if (*Iterator!=(TList*)0x01) {

      // If NULL as iterator, start at beginning
      if (!(*Iterator)) {
         *Iterator=Dict.Vars;
      }

      // If a search proc and var is specified
      if ((*Iterator=TList_Find((*Iterator),Dict_CheckVar,Var))) {
         var=(TDictVar*)((*Iterator)->Data);
         *Iterator=(*Iterator)->Next;
      }
      *Iterator=(*Iterator==NULL?(TList*)0x01:*Iterator);
   }
   return(var);
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_AddType>
 * Creation : Juin 2014 - J.P. Gauthier
 *
 * But      : Add a type.
 *
 * Parametres  :
 *  <Type>     : Typevar to add to dictionary
 *
 * Retour:
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
void Dict_AddType(TDictType *Type) {
   Dict.Vars=TList_AddSorted(Dict.Types,Dict_SortType,Type);
}


//! Compare to types to ordered them
int Dict_SortType(
    //! [in] First type to compare
    const void * const type0,
    //! [in] Second type to compare
    const void * const type1
) {
    //! \return 0 if both types are equal, negative if type0 should go before type1, 1 or greather otherwise
    return strcasecmp(((TDictType*)type0)->Name, ((TDictType*)type1)->Name);
}


//! Check if a type satisfies search parameters
int Dict_CheckType(
    //! [in] Type to check
    const void * const type0,
    //! [in] Type name. Handled as a glob pattern if DictSearch.Mode == DICT_GLOB
    const void * const str
) {
    //! \return 1 if type0 satisfies parameters, 0 otherwise

    if (!type0) return 0;

    if (DictSearch.State && !(((TDictType*)type0)->Nature & DictSearch.State)) return 0;

    if (DictSearch.Origin && strcasecmp(((TDictType*)type0)->Origin, DictSearch.Origin)) return 0;

    if (DictSearch.Mode == DICT_GLOB) {
        return str ? (!strmatch(((TDictType*)type0)->Name, (char*)str)) : 1;
    } else {
        return str ? (!strcasecmp(((TDictType*)type0)->Name, (char*)str) || !strcasecmp(&((TDictType*)type0)->Name[1], (char*)str)) : 0;
    }
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_GetType>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Search for a type
 *
 * Parametres  :
 *  <Type>     : Type name
 *
 * Retour:
 *  <TDictType>: Type info
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
TDictType *Dict_GetType(char *Type) {

   TList *list;

   list=TList_Find(Dict.Types,Dict_CheckType,Type);
   return(list?(TDictType*)(list->Data):NULL);
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_IterateType>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Iterator over a list of TDictType
 *
 * Parametres  :
 *  <ITerator> : List pointer start (NULL for beginning of list)
 *  <Type>     : Type to look for (if NULL, iterate over all)
 *
 * Retour:
 *  <TDictType> : Type info
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
TDictType *Dict_IterateType(TList **Iterator,char *Type) {

   TDictType *type=NULL;

   if (*Iterator!=(TList*)0x01) {

      // If NULL as iterator, start at beginning
      if (!(*Iterator)) {
         *Iterator=Dict.Types;
      }

      // If a search proc and var is specified
      if ((*Iterator=TList_Find((*Iterator),Dict_CheckType,Type))) {
         type=(TDictType*)((*Iterator)->Data);
         *Iterator=(*Iterator)->Next;
      }
      *Iterator=(*Iterator==NULL?(TList*)0x01:*Iterator);
   }

   return(type);
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_PrintVars>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Print info about a list of variables
 *
 * Parametres :
 *  <Var>     : Variable to look fortement
 *  <Format>  : Print format (DICT_SHORT,DICT_LONG,DICT_XML)
 *  <Lang>    : Language (APP_FR,APP_EN)
 *
 * Retour:
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
void Dict_PrintVars(char *Var,int Format,TApp_Lang Lang) {

   TList *list;

   list=Dict.Vars;

   while ((list=TList_Find(list,Dict_CheckVar,Var)) != NULL) {
       Dict_PrintVar((TDictVar*)list->Data,Format,Lang);
       list=list->Next;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_PrintVar>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Print info about a variable
 *
 * Parametres :
 *  <Var>     : Variable to look fortement
 *  <Format>  : Print format (DICT_SHORT,DICT_LONG,DICT_XML)
 *  <Lang>    : Language (APP_FR,APP_EN)
 *
 * Retour:
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
void Dict_PrintVar(TDictVar *DVar,int Format,TApp_Lang Lang) {

   int         i;
   struct tm *tm;
   TDictVar  *var;

//   #include <sys/ioctl.h>
//   struct winsize w;
//   ioctl(0, TIOCGWINSZ,&w);
//    printf ("lines %d\n", w.ws_row);
//    printf ("columns %d\n", w.ws_col);

   if (DVar) {
      if (!(var=Dict_ApplyModifier(DVar,DictSearch.Modifier))) {
         return;
      }
      if (var->Nature&DICT_OBSOLETE)   printf(APP_COLOR_RED);
      if (var->Nature&DICT_DEPRECATED) printf(APP_COLOR_YELLOW);

      switch(Format) {
         case DICT_SHORT:
            printf("%-4s\t%-70s\t%-s",var->Name,var->Short[Lang],var->Units);
            if (var->Nature&DICT_OBSOLETE)
               printf(" \t%s",TOBSOLETE[Lang]);
            else if (var->Nature&DICT_DEPRECATED)
               printf(" \t%s",TDEPRECATED[Lang]);
             
            // Reset color before end of line
            if (var->Nature&DICT_OBSOLETE || var->Nature&DICT_DEPRECATED) printf(APP_COLOR_RESET);
            printf("\n");

            break;

         case DICT_LONG:
            printf("--------------------------------------------------------------------------------\n");
            printf("Nomvar              : %-s", var->Name);
            if (var->IP1==0)           printf(" IP1(%i)",var->IP1);
            if (var->IP1>0)            printf(" IP1(old=%i; new=%i; level=%g,kind=%i)",IPEncode(var->Level,var->Kind,0),IPEncode(var->Level,var->Kind,1),var->Level,var->Kind);
            if (var->IP2>=0)           printf(" IP2(%i)",var->IP2);
            if (var->IP3>=0)           printf(" IP3(%i)",var->IP3);
            if (var->ETIKET[0]!='\0')  printf(" ETIKET(%s)",var->ETIKET);

            printf("\n%-s : %-s\n", TSHORT[Lang],var->Short[Lang]);
            printf("%-s : %-s\n", TLONG[Lang],var->Long[Lang][0]!='\0'?var->Long[Lang]:"-");
            printf("%-s : %-s\n", TORIGIN[Lang],var->Origin[0]!='\0'?var->Origin:"-");
            printf("%-s : ", TSTATE[Lang]);

            if (var->Nature&DICT_OBSOLETE)
               printf("%s\n",TOBSOLETE[Lang]);
            else if (var->Nature&DICT_DEPRECATED)
               printf("%s\n",TDEPRECATED[Lang]);
            else if (var->Nature&DICT_CURRENT)
               printf("%s\n",TCURRENT[Lang]);
            else if (var->Nature&DICT_FUTURE)
               printf("%s\n",TFUTURE[Lang]);
             else if (var->Nature&DICT_INCOMPLETE)
               printf("%s\n",TINCOMPLETE[Lang]);

            if (var->Date) {
               tm=gmtime(&(var->Date));
               printf("%-s : %04i-%02i-%02i\n", TDATE[Lang],1900+tm->tm_year,tm->tm_mon+1,tm->tm_mday);
            } else {
               printf("%-s : %-s\n", TDATE[Lang],"-");
            }

            if (var->Pack>0)  {
               printf("%-s : %i\n",TPACK[Lang],var->Pack);
            } else {
               printf("%-s : %-s\n",TPACK[Lang],"-");
            }

            if (var->Nature & DICT_INTEGER) {
                  printf("%-s : %-s\n",TTYPE[Lang],TINT[Lang]);
                  printf("%-s : %s\n",TUNITES[Lang],var->Units);
                  if (var->Magnitude!=DICT_NOTSET)     printf("%-s : %e\n",TMAG[Lang],var->Magnitude);
                  if (var->Min!=var->Max) {
                     printf("%-s : ",TRANGE[Lang]);
                     if (var->Min!=DICT_NOTSET) printf("[%.0f ",var->Min);
                     if (var->Max!=DICT_NOTSET) printf("%.0f]\n",var->Max);
                     printf("\n");
                  }
                  break;

            } else if (var->Nature & DICT_REAL) {
                  printf("%-s : %-s\n",TTYPE[Lang],TREAL[Lang]);
                  printf("%-s : %s\n",TUNITES[Lang],var->Units);
                  if (var->Precision!=DICT_NOTSET)    printf("%-s : %e\n",TPREC[Lang],var->Precision);
                  if (var->Magnitude!=DICT_NOTSET)    printf("%-s : %e\n",TMAG[Lang],var->Magnitude);
                  if (var->Min!=var->Max) {
                     printf("%-s : ",TRANGE[Lang]);
                     if (var->Min!=DICT_NOTSET) printf("[%.0f ",var->Min);
                     if (var->Max!=DICT_NOTSET) printf("%.0f]\n",var->Max);
                     printf("\n");
                  }
                  break;

            } else if (var->Nature & DICT_LOGICAL) {
                  printf("%-s : %-s\n",TTYPE[Lang],TLOGIC[Lang]);
                  if( var->Meanings[0][0][0]!='\0' || var->Meanings[0][1][0]!='\0' || var->Meanings[1][0][0]!='\0' || var->Meanings[1][1][0]!='\0' ) {
                     printf("\tCode\t\t%s\n",TVAL[Lang]);
                     printf("\t----\t\t----------------\n");
                     printf("\t0\t\t%-s\n",var->Meanings[0][Lang]);
                     printf("\t1\t\t%-s\n",var->Meanings[1][Lang]);
                  }
                  break;

            } else if (var->Nature & DICT_CODE) {
                  printf("%-s : %-s\n",TTYPE[Lang],TCODE[Lang]);
                  printf("\tCode\t\t%s\n",TVAL[Lang]);
                  printf("\t----\t\t----------------\n");
                  for (i=0; i < var->NCodes; i++) {
                     printf("\t%i\t\t%-s\n", var->Codes[i], var->Meanings[i][Lang]);
                  }
                  break;
            }
            break;
            
         case DICT_XML:
            printf("<metvar usage=");
            
            if (var->Nature&DICT_OBSOLETE) {
               printf("\"obsolete\"");
            } else if (var->Nature&DICT_DEPRECATED) {
               printf("\"deprecated\"");
            } else if (var->Nature&DICT_CURRENT) {
               printf("\"current\"");
            } else if (var->Nature&DICT_FUTURE) {
               printf("\"future\"");
            } else if (var->Nature&DICT_INCOMPLETE) {
               printf("\"incomplete\"");
            }
            
            if (var->IP1>=0) printf(" ip1=\"%i\"",var->IP1);
            if (var->IP2>=0) printf(" ip2=\"%i\"",var->IP2);
            if (var->IP3>=0) printf(" ip3=\"%i\"",var->IP3);
            if (var->ETIKET[0]!='\0') printf(" etiket=\"%s\"",var->ETIKET);

            if (var->Level!=FLT_MAX)   printf(" level=\"%g\"",var->Level);
            if (var->Kind>=0)          printf(" kind=\"%i\"",var->Kind);

            printf(" origin=\"%s\"",var->Origin);
            
            if (var->Pack>0)  {
               printf(" pack=\"%i\"",var->Pack);
            }
            
            if (var->Date) {
               tm=gmtime(&(var->Date));
               printf(" date=\"%04i-%02i-%02i\">\n",1900+tm->tm_year,tm->tm_mon+1,tm->tm_mday);
            } else {
               printf(" date=\"\">\n");
            } 

            printf("\t<nomvar>%s</nomvar>\n",var->Name);
            printf("\t<description>\n");
            printf("\t\t<short lang=\"fr\">%s</short>\n",var->Short[0]);
            printf("\t\t<short lang=\"en\">%s</short>\n",var->Short[1]);
            printf("\t\t<long lang=\"fr\">%s</long>\n",var->Long[0]);
            printf("\t\t<long lang=\"en\">%s</long>\n",var->Long[1]);
            printf("\t</description>\n");
            printf("\t<measure>\n");

            if (var->Nature & DICT_INTEGER) {
               printf("\t\t<integer>\n");
               printf("\t\t\t<units>%s</units>\n",var->Units);
               if (var->Magnitude!=DICT_NOTSET)  
                  printf("\t\t\t<magnitude>%0.0f</magnitude>\n",var->Magnitude);
               if (var->Min!=var->Max) {
                  printf("\t\t\t<min>%0.0f</min>\n",var->Min);
                  printf("\t\t\t<max>%0.0f</max>\n",var->Max);
               }
               printf("\t\t</integer>\n");

            } else if (var->Nature & DICT_REAL) {
               printf("\t\t<real>\n");
               printf("\t\t\t<units>%s</units>\n",var->Units);
               if (var->Precision!=DICT_NOTSET)  
                  printf("\t\t\t<precision>%e</precision>\n",var->Precision);
               if (var->Magnitude!=DICT_NOTSET)  
                  printf("\t\t\t<magnitude>%e</magnitude>\n",var->Magnitude);
               if (var->Min!=var->Max) {
                  printf("\t\t\t<min>%e</min>\n",var->Min);
                  printf("\t\t\t<max>%e</max>\n",var->Max);
               }
               printf("\t\t</real>\n");
 
            } else if (var->Nature & DICT_LOGICAL) {
               printf("\t\t<logical>\n");
               printf("\t\t</logical>\n");

            } else if (var->Nature & DICT_CODE) {
               printf("\t\t<code>\n");
               for (i=0; i < var->NCodes; i++) {
                  printf("\t\t\t<value>%d</value>\n",var->Codes[i]);
                  printf("\t\t\t<meaning lang=\"fr\">%s</meaning>\n",var->Meanings[i][0]);
                  printf("\t\t\t<meaning lang=\"en\">%s</meaning>\n",var->Meanings[i][1]);
               }
               printf("\t\t</code>\n");
            }
            printf("\t</measure>\n");
            printf("</metvar>\n");
            
            break;
      }
      if (var!=DVar) free(var);
      if (var->Nature&DICT_OBSOLETE || var->Nature&DICT_DEPRECATED) printf(APP_COLOR_RESET);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_PrintTypes>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Print info about a list of Types
 *
 * Parametres :
 *  <Var>     : Variable to look fortement
 *  <Format>  : Print format (DICT_SHORT,DICT_LONG,DICT_XML)
 *  <Lang>    : Language (APP_FR,APP_EN)
 *
 * Retour:
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
void Dict_PrintTypes(char *Type,int Format,TApp_Lang Lang) {

   TList *list;
   int   head=0;

   list=Dict.Types;

   while ((list=TList_Find(list,Dict_CheckType,Type)) != NULL) {
       if (!head) {
          printf("\n12-TYPVAR-----------------------------------------------------------------------\n");
          head++;
       }
       Dict_PrintType((TDictType*)list->Data,Format,Lang);
       list=list->Next;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_PrintType>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Print info about a type
 *
 * Parametres :
 *  <Var>     : Variable to look fortement
 *  <Format>  : Print format (DICT_SHORT,DICT_LONG,DICT_XML)
 *  <Lang>    : Language (APP_FR,APP_EN)
 *
 * Retour:
 *
 * Remarques :
 *----------------------------------------------------------------------------
*/
void Dict_PrintType(TDictType *DType,int Format,TApp_Lang Lang) {

   struct tm *tm;

   if (DType) {

      switch(Format) {
         case DICT_SHORT:
            printf("%-2s  %-60s\n", DType->Name, DType->Short[Lang]);
            break;

         case DICT_LONG:
            printf("--------------------------------------------------------------------------------\n");
            printf("Typvar             : %-s", DType->Name);
            printf("\n%-s : %-s\n", TSHORT[Lang],DType->Short[Lang]);
            printf("%-s : %-s\n", TLONG[Lang],DType->Long[Lang][0]!='\0'?DType->Long[Lang]:"-");

            if (DType->Date) {
               tm=gmtime(&(DType->Date));
               printf("%-s : %04i-%02i-%02i\n", TDATE[Lang],1900+tm->tm_year,tm->tm_mon+1,tm->tm_mday);
            } else {
               printf("%-s : %-s\n", TDATE[Lang],"-");
            }

            printf("%-s : %-s\n", TORIGIN[Lang],DType->Origin[0]!='\0'?DType->Origin:"-");
            printf("%-s : ", TSTATE[Lang]);

            if (DType->Nature&DICT_OBSOLETE)
               printf("%s\n",TOBSOLETE[Lang]);
            else if (DType->Nature&DICT_DEPRECATED)
               printf("%s\n",TDEPRECATED[Lang]);
            else if (DType->Nature&DICT_CURRENT)
               printf("%s\n",TCURRENT[Lang]);
            else if (DType->Nature&DICT_FUTURE)
               printf("%s\n",TFUTURE[Lang]);
            else if (DType->Nature&DICT_INCOMPLETE)
               printf("%s\n",TINCOMPLETE[Lang]);

            break;
            
         case DICT_XML:
            printf("<typvar usage=");
            
            if (DType->Nature&DICT_OBSOLETE) {
               printf("\"obsolete\"");
            } else if (DType->Nature&DICT_DEPRECATED) {
               printf("\"deprecated\"");
            } else if (DType->Nature&DICT_CURRENT) {
               printf("\"current\"");
            } else if (DType->Nature&DICT_FUTURE) {
               printf("\"future\"");
            } else if (DType->Nature&DICT_INCOMPLETE) {
               printf("\"incomplete\"");
            }
            
            if (DType->Date) {
               tm=gmtime(&(DType->Date));
               printf(" date=\"%04i-%02i-%02i\">\n",1900+tm->tm_year,tm->tm_mon+1,tm->tm_mday);
            } else {
               printf(" date=\"\">\n");
            } 
            
           printf("\t<nomtype>%s</nomtype>\n",DType->Name);
           printf("\t<description>\n");
           printf("\t\t<short lang=\"fr\">%s</short>\n",DType->Short[0]);
           printf("\t\t<short lang=\"en\">%s</short>\n",DType->Short[1]);
           printf("\t\t<long lang=\"fr\">%s</long>\n",DType->Long[0]);
           printf("\t\t<long lang=\"en\">%s</long>\n",DType->Long[1]);
           printf("\t</description>\n");
           printf("</typvar\n");
            
            break;
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Dict_ApplyModifier>
 * Creation : Mai 2014 - J.P. Gauthier
 *
 * But      : Decode les étiquette au format RRPPPPPP[N,P,X]MMM
 *
 * Parametres :
 *  <Var>     : Variable to look fortement
 *  <Format>  : Print format (DICT_SHORT,DICT_LONG)
 *
 * Retour:
 *
 * Remarques :
 *
 *    RR     = No de passe
 *    PPPPPP = produit
 *    N,P,X  = Opérationnel, Parallèle, Expérimental
 *    MMM    = Membre (ou ALL pour tous)
 *----------------------------------------------------------------------------
*/
TDictVar* Dict_ApplyModifier(TDictVar *Var,char *Modifier) {

   char     *c,*l;
   TDictVar *var=NULL;

   if (!Modifier || Modifier[0]=='\0') {
      return(Var);
   }

   // Copy var since we'll be changing a few things
   if ((var=(TDictVar*)calloc(1,sizeof(TDictVar)))) {
      memcpy(var,Var,sizeof(TDictVar));

      // Loop on languages
      for(uint8_t lang=0;lang<2;lang++) {

         c=&Modifier[2];
         l=var->Short[lang];
         l+=strlen(var->Short[lang]);
         (*l++)=' ';

         if (!strncmp(c,"MIN___",6)) {
            strcpy(l,TMIN[lang]); l+=strlen(TMIN[lang]);
         } else if (!strncmp(c,"MAX___",6)) {
            strcpy(l,TMAX[lang]); l+=strlen(TMAX[lang]);
         } else if (!strncmp(c,"SSTD__",6)) {
            strcpy(l,TSSTD[lang]); l+=strlen(TSSTD[lang]);
         } else if (!strncmp(c,"PSTD__",6)) {
            strcpy(l,TPSTD[lang]); l+=strlen(TPSTD[lang]);
         } else if (!strncmp(c,"MEAN__",6)) {
            strcpy(l,TMEAN[lang]); l+=strlen(TMEAN[lang]);
         } else if (!strncmp(c,"EFI___",6)) {
            strcpy(l,TEFI[lang]); l+=strlen(TEFI[lang]);
         } else {

            // Decode operator
            if (c[0]=='C') {

               c+=1;
               (*l++)='(';
               while(*c!='_') (*l++)=*c++;
               strcpy(l,TCENTILE[lang]); l+=strlen(TCENTILE[lang]);   // Centile operator
               (*l++)=')';

            } else {

               if  (c[0]=='G' && c[1]=='T') {                         // Greater than
                  c+=2; (*l++)='>';
               } else if  (c[0]=='L' && c[1]=='T') {                  // Less than
                  c+=2; (*l++)='<';
               } else if  (c[0]=='E' && c[1]=='Q') {                  // Equal to
                  c+=2; (*l++)='=';
               } else if  (c[0]=='G' && c[1]=='E') {                  // Greater or equal
                  c+=2; (*l++)='>'; (*l++)='=';
               } else if  (c[0]=='L' && c[1]=='E') {                  // Less or equal
                  c+=2; (*l++)='<'; (*l++)='=';
               } else {
                  Lib_Log(APP_LIBMETA,APP_ERROR,"%s: Invalid modifier: %s\n",__func__,Modifier);
                  return(NULL);
               }
               (*l++)=' ';

               // Decode value of operator
               while(*c!='_') (*l++)=*c++;

               // Add variable unit
               (*l++)=' ';
               strcpy(l,Var->Units); l+=strlen(Var->Units);

               // New units -> %
               var->Units[0]='%';var->Units[1]='\0';
               var->Min=0.0;
               var->Max=100.0;
            }
         }
         
         // Check for range of time
         c=&Modifier[8];
         if (c[0]!='N') {
            (*l++)=' ';
            strcpy(l,TBETWEEN[lang]); l+=strlen(TBETWEEN[lang]);
            (*l++)=' ';
            
            if (c[0]=='D') {
               strcpy(l,TDAY[lang]); l+=strlen(TDAY[lang]);
            } else if (c[0]=='H') {
               strcpy(l,THOUR[lang]); l+=strlen(THOUR[lang]);
            }
            (*l++)=' ';
            (*l++)='1';
            (*l++)=' ';
            strcpy(l,TAND[lang]); l+=strlen(TAND[lang]);
            (*l++)=' ';
            if (c[1]!='0') (*l++)=c[1];
            if (c[1]!='0' || c[2]!='0') (*l++)=c[2];
            (*l++)=c[3];
         }
                  
         (*l++)='\0';
      }
   }
   return(var);
}
