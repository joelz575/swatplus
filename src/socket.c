#include <stdbool.h>
#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <json.h>
#include <json_tokener.h>
#include <stdint.h>
#ifdef _WIN32
	#include <WinSock2.h> // windows
#else
	#include <sys/socket.h>  // unicode
	#include <arpa/inet.h>
#endif


/*Note for developers:
    Only variable names with a length of up to 10 characters are supported!
*/

void jsonSTRtoFLTarray_(float floatCont[], int *arraySize, char *jsonString);
void jsonSTRtoINTarray_(int intCont[], int *arraySize, char *jsonString);

void opensocket_(int *portNum, char *hostNum, int *client){
#ifdef _WIN32
 printf("C function: Opening socket");
 printf("\nPortNum in C: %i",*portNum);

 int portNumber = *portNum;
 WSADATA winsockData;
 if (WSAStartup(MAKEWORD(2,2), &winsockData) != 0) {
            fprintf(stderr, "WSAStartup failed.\n");
            exit(1);
 }
 printf("\nWSAStartup was done successfully...");
 printf((char *)WSAGetLastError());
 SOCKET cliente;

 cliente = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
 printf("client socket was created successfully...");
 printf((char *)WSAGetLastError());
 printf("\nClient obj in C: %d", cliente);
 struct sockaddr_in server_address;
 server_address.sin_family  = AF_INET;
 server_address.sin_addr.s_addr = inet_addr(hostNum);
 server_address.sin_port = htons(portNumber);
 printf((char *)WSAGetLastError());

 struct sockaddr_in TCPClientAdd;
 int iTCPClientAdd = sizeof(TCPClientAdd);


 int connectRes;
 connectRes = connect(cliente, (struct sockaddr *) &server_address, sizeof(server_address));
 printf((char *)WSAGetLastError());

 if (connectRes == -1){
	printf((char *)WSAGetLastError());
 	}
 	printf("\nConnect Result: %d", connectRes);
 	printf("\nCliente= %i", cliente);
 	*client = (int *)cliente;

#else
 printf("C function: Opening socket");
 printf("\nPortNum in C: %i",*portNum);
 int portNumber = *portNum;
 printf("\nHost Number in C: "); printf(hostNum);
 struct sockaddr_in serv_addr;

 if ((*client = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        printf("\n Socket creation error \n");
        //return -1;
    }
 else{
    printf("This is the client in C: %d\n", client);
 }
 serv_addr.sin_family = AF_INET;
 serv_addr.sin_port = htons(portNumber);
 serv_addr.sin_addr.s_addr = inet_addr(hostNum);


    printf("Connecting now\n");
    fflush(stdout);
 if (connect(*client, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
    {
        printf("\nConnection Failed \n");

    }

#endif
}

void receive_(int *client, char command[], char var_nombre[], char tip_con[], int *tamano_con, int *pasos, char shape[]){
 #ifdef _WIN32
 	/*Cuando tenga una connexión exitosa, podemos recibir datos del socket.
 	int recvRes;
 	int n;
	char blankBuffer;
 	char contBuffer[20000];
 	struct json_object *parsed_json;
 	struct json_object *orden;
 	struct json_object *tamano;
 	struct json_object *var;
 	struct json_object *matr;
 	struct json_object *contenido;
 	struct json_object *tipo_cont;
 	char json_header[20000];

	n = recv(*client, &blankBuffer, 2, 0);
	sendr_(client, "RCVD");

 	n = recv(*client, json_header, ((int) blankBuffer), 0);

 	parsed_json = json_tokener_parse(json_header);

 	json_object_object_get_ex(parsed_json, "orden", &orden);
 	json_object_object_get_ex(parsed_json, "tamaño", &tamano);
 	json_object_object_get_ex(parsed_json, "var", &var);
 	json_object_object_get_ex(parsed_json, "matr", &matr);
 	json_object_object_get_ex(parsed_json, "tipo_cont", &tipo_cont);
	command = json_object_get_string(orden);

 	if(strncmp(command, "TOMAR_", 6) == 0){
 		var_nombre = json_object_get_string(var);

 		tip_con = json_object_get_string(tipo_cont);

 		recv(*client, contBuffer, 20000, 0);
 		contenido = json_tokener_parse(contBuffer);
 		*tamano_con = (int *)json_object_array_length(contenido);

 	 	if(*tamano_con > 0){
 			if(strncmp(tip_con, "int", 3) == 0){
 			tip_con = 'i';
			//intCont = (int *)malloc(sizeof(int)*((int) *tamano_con));
 	 		//charCont = (int *)malloc(sizeof(char)*0);
 	 		//floatCont = (int *)malloc(sizeof(double)*0);
 	 		//*intCont = (int *) json_object_get_array(contenido);
		    }
		    else if( strncmp(tip_con, "flt", 3) == 0){
			tip_con = 'f';
			floatCont = (int *)malloc(sizeof(double)*((int)*tamano_con));
 	 		//charCont = (int *)malloc(sizeof(char)*0);
 	 		intCont = (int *)malloc(sizeof(int)*0);
			*floatCont = (int *) json_object_get_array(contenido);
			printf("Json Object to array: %f", json_object_get_array(contenido)[0]);
			//for(i = 0; i < *tamano_con; i++){
			//	floatCont[i] = json_object_array_
			//}
			//memcpy(floatCont, json_object_get_array(contenido), *tamano_con+1);
		    }
	    else{
		    intCont = (int *)malloc(sizeof(int)*0);
 		    floatCont = (int *)malloc(sizeof(double)*0);
		}
	}
 		else{
 			intCont = (int *)malloc(sizeof(int)*0);
 			floatCont = (int *)malloc(sizeof(double)*0);
		}

 	}
*/
 #else
 	//Cuando tenga una connexión exitosa, podemos recibir datos del socket.

 	int recvRes;
 	int tmn;
 	int n;
 	int i;
 	char tipo_contenido;
	char blankBuffer;
 	char contBuffer[20000];
 	struct json_object *parsed_json;
 	struct json_object *orden;
 	struct json_object *tamano;
 	struct json_object *var;
 	struct json_object *matr;
 	struct json_object *contenido;
 	struct json_object *tipo_cont;
 	struct json_object *n_pasos;
 	struct json_object *forma;
 	char json_header[2000];

	//printf("Just before receive line...\n");
	//fflush( stdout );

	n = read(*client, &blankBuffer, 4);
    //printf("Size accoring to n variable: %d\n", n);
	//fflush( stdout );
	//printf("Size according to blank Buffer: %d\n", (int)blankBuffer);
	//fflush( stdout );

 	n = read(*client, &json_header, (int) blankBuffer);
 	//printf("Receive Results from json_header: %i\n", n);
 	//fflush( stdout );
 	//printf("Received json-header: %s\n", json_header);
 	//fflush( stdout );

 	parsed_json = json_tokener_parse(json_header);
 	//printf("Parsed json: %s", json_object_get_string(parsed_json));
 	//fflush( stdout );
 	json_object_object_get_ex(parsed_json, "tipo", &orden);
    strncpy(command, "       ", 7);
	strncpy(command, json_object_get_string(orden), strlen(json_object_get_string(orden)));
 	//printf("Received Command: %s\n", command);
 	//fflush( stdout );


 	if(strncmp(command, "cambiar", 7) == 0){
 	    printf("Command: %s\n", json_object_get_string(orden));
 	    fflush( stdout );

 	    json_object_object_get_ex(parsed_json, "tamaño", &tamano);

 	    json_object_object_get_ex(parsed_json, "var", &var);
 	    printf("strlen(json_object_get_string(var)): %d\n", strlen(json_object_get_string(var)));
        strncpy(var_nombre, "                     ", strlen(var_nombre));

        strncpy(var_nombre, json_object_get_string(var), strlen(json_object_get_string(var)));

 	    json_object_object_get_ex(parsed_json, "forma", &forma);
 	    strncpy(shape, "                      ", strlen(shape));
 	    strncpy(shape, json_object_get_string(forma), strlen(json_object_get_string(forma)));

 	    json_object_object_get_ex(parsed_json, "tipo_cont", &tipo_cont);
 		printf("Received Variable: %s\n", var_nombre);
 		printf("Tipo contenido: %s\n", json_object_get_string(tipo_cont));
 		tmn = atoi((const char *) json_object_get_string(tamano));
 		printf("Tamano: %d\n", tmn);
 		printf("Shape of received variable: %s\n", shape);
 		fflush( stdout );
 		strncpy(tip_con, json_object_get_string(tipo_cont), 5);
 	}
 	else if(strncmp(command, "incr  ", 4) == 0){
 	       printf("incr was received\n");
 	       fflush(stdout);
 	       json_object_object_get_ex(parsed_json, "n_pasos", &n_pasos);
 	       *pasos = atoi(json_object_get_string(n_pasos));
 	}
 	else if(strncmp(command, "cerrar", 6) == 0){
 	        printf("cerrar was received");
 	        fflush(stdout);
 	}
 	else if(strncmp(command, "leer  ", 4) == 0){
 	        printf("leer was received");
 	        fflush(stdout);
 	        json_object_object_get_ex(parsed_json, "var", &var);
 	        printf("strlen(json_object_get_string(var)): %d", strlen(json_object_get_string(var)));
            strncpy(var_nombre, "                              ", strlen(var_nombre));
            //memset(var_nombre, '\0', sizeof(var_nombre));
            strncpy(var_nombre, json_object_get_string(var), strlen(json_object_get_string(var)));
 	}
 #endif
}

void recvint_(int *client, int *intCont, int *arraySize){
    int64_t int64Buffer [*arraySize];
    int n;
    int i;
    int tmn = *arraySize * sizeof(int64_t);
    n = recv(*client, &int64Buffer, tmn, 0 );
   for(i = 0; i<*arraySize; i++){
        if (int64Buffer[i]-INT_MIN <= (int64_t)INT_MAX-INT_MIN){
           intCont[i] = int64Buffer[i];
            }
        else{
            printf("Error transferring int values, probably too large\n");
            printf("The error occured in the slot: %d\n", i);
        }
    }
}

void recvfloat_(int *client, float *floatCont, int *arraySize){
    int n;
    int tmn = *arraySize * sizeof(float);
    n = recv(*client, &floatCont, tmn, 0 );
}

void sendr_(int *client, int *intSenderBuffer, double *floatSenderBuffer, char shape[], int *intLength, int *floatLength, int *shapeLen){
 int i,n,y;
 int sendRes;
 char valLen[16];
 char tipo_cont[6] = "int";
 char jsonEncabezadoString[2048];
 int jsonStringLen;
 char intbufferBytes[sizeof(intSenderBuffer)];

 sprintf(intbufferBytes, "%p", intSenderBuffer);
 printf("These are the intbufferBytes: %s\n",intbufferBytes);

 printf("IntBuffer length: %d\n", *intLength);
 printf("floatBuffer length: %d\n", *floatLength);

 if(*floatLength != 0){
    sprintf( valLen, "%d", sizeof(floatSenderBuffer));
    sprintf(shape, "%d", -1);
    strncpy(tipo_cont, "\0\0\0\0\0\0", 5);
    strncpy(tipo_cont, "float", 5);
    printf("Content type: %s\n", tipo_cont);
    printf("Float ValLen: %s\n", valLen);
 }

 else{

    sprintf( valLen, "%d", sizeof(intSenderBuffer));
    sprintf(shape, "%d", -1);
    printf("Content type: %s\n", tipo_cont);
    printf("Int ValLen: %s\n", valLen);
 }

 strncpy(jsonEncabezadoString, "{\"tamaño\": ", 13) ;
 strcat(jsonEncabezadoString, valLen);
 strcat(jsonEncabezadoString, ", \"tipo_cont\": \"");
 strcat(jsonEncabezadoString, tipo_cont);
 strcat(jsonEncabezadoString, "\", \"forma\": ");
 strcat(jsonEncabezadoString, shape);
 strcat(jsonEncabezadoString, "}");

 printf("Content type: %s\n", tipo_cont);
 printf("Sending encabezado: %s\n", jsonEncabezadoString);
 for (y = 0; y<sizeof(intSenderBuffer); y++){
    printf("Sending int value: %d\n", intSenderBuffer[y]);
    if(y > *intLength-1){
        intSenderBuffer[y] = 0;
    }
    printf("Sending int value: %d\n", intSenderBuffer[y]);
    printf("In slot y: %d\n", y);
 }

 printf("Sending float value: %s\n", floatSenderBuffer);
 fflush(stdout);

 i = strlen(jsonEncabezadoString);
 printf("Sending length: %d\n", i);

 sendRes = send(*client, &i, sizeof (i), 0);

 printf("Send Result: %d\n", sendRes);
 fflush(stdout);

 if (sendRes==-1){
	printf("send failed...");
    }
#ifdef _WIN32

	printf((char *)WSAGetLastError());

#endif

 sendRes = send(*client, (char *)jsonEncabezadoString, strlen(jsonEncabezadoString),0);
 printf("Send Result: %d\n", sendRes);
 fflush(stdout);

 if (sendRes==-1){
	printf("send failed...");
	}

#ifdef _WIN32

	printf((char *)WSAGetLastError());

#endif
 if(strncmp(tipo_cont, "float", 5)==0){
    sendRes = send(*client, floatSenderBuffer, sizeof(floatSenderBuffer), 0);
    printf("Float content Send Result: %d\n", sendRes);
    fflush(stdout);
    }
 else{
    sendRes = send(*client, intSenderBuffer, sizeof(intSenderBuffer), 0);
    printf("Int content Send Result: %d\n", sendRes);
    fflush(stdout);
 }
 if (sendRes==-1){
	printf("send failed...");

#ifdef _WIN32

	printf((char *)WSAGetLastError());

#endif
    }
}


void closesock_(int *client){
#ifdef _WIN32
 int closeRes;
 closeRes = closesocket(*client);

 int iWSACleanup;	//print error if it didnt work
 iWSACleanup = WSACleanup();
#else
 shutdown(*client, 2);

#endif
}