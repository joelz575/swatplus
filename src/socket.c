#include <stdbool.h>
#include <stdio.h>
#include <float.h>
#include <limits.h>
#include <math.h>
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
 //printf((char *)WSAGetLastError());
 SOCKET cliente;

 cliente = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
 printf("client socket was created successfully...");
 //printf((char *)WSAGetLastError());
 printf("\nClient obj in C: %d", cliente);
 struct sockaddr_in server_address;
 server_address.sin_family  = AF_INET;
 server_address.sin_addr.s_addr = inet_addr(hostNum);
 server_address.sin_port = htons(portNumber);
 //printf((char *)WSAGetLastError());

 struct sockaddr_in TCPClientAdd;
 int iTCPClientAdd = sizeof(TCPClientAdd);


 int connectRes;
 connectRes = connect(cliente, (struct sockaddr *) &server_address, sizeof(server_address));
 //printf((char *)WSAGetLastError());

 if (connectRes == -1){
	//printf((char *)WSAGetLastError());
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
    printf("This is the client in C: %ls\n", client);
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

void receive_(int *client, char command[], char var_name[], char content_type[], int *passes, int *shape, int *precision){
 #ifdef _WIN32
    //Cuando tenga una connexión exitosa, podemos recibir datos del socket.
 	int recvRes;
 	int tmn;
 	int n;
 	int i;
 	char tipo_contenido[5];
	char blankBuffer[4];
	char shapeBuffer[6];
 	struct json_object *parsed_json;
 	struct json_object *orden;
 	struct json_object *tamano;
 	struct json_object *var;
 	struct json_object *matr;
 	struct json_object *contenido;
 	struct json_object *tipo_cont;
 	struct json_object *n_pasos;
 	struct json_object *forma;
 	char json_header[100000];

	recv(*client, &blankBuffer, 4, 0);

 	recv(*client, json_header, ((int) blankBuffer), 0);


 #else
 	//Cuando tenga una connexión exitosa, podemos recibir datos del socket.

 	int recvRes;
 	int tmn;
 	int n;
 	int i;
 	char tipo_contenido[5];
	char blankBuffer;
	char shapeBuffer[6];
 	struct json_object *parsed_json;
 	struct json_object *orden;
 	struct json_object *tamano;
 	struct json_object *var;
 	struct json_object *matr;
 	struct json_object *contenido;
 	struct json_object *tipo_cont;
 	struct json_object *n_pasos;
 	struct json_object *forma;
 	struct json_object *precisn;
 	char json_header[2000];

    fflush( stdout );

	read(*client, &blankBuffer, 4);
 	read(*client, &json_header, (int) blankBuffer);
 #endif
 	parsed_json = json_tokener_parse(json_header);

 	json_object_object_get_ex(parsed_json, "tipo", &orden);
    strncpy(command, "       ", 7);
	strncpy(command, json_object_get_string(orden), strlen(json_object_get_string(orden)));

 	if(strncmp(command, "cambiar", 7) == 0){
 	    json_object_object_get_ex(parsed_json, "tamaño", &tamano);

 	    json_object_object_get_ex(parsed_json, "var", &var);
 	    strncpy(var_name, "                     ", strlen(var_name));
        strncpy(var_name, json_object_get_string(var), strlen(json_object_get_string(var)));

 	    json_object_object_get_ex(parsed_json, "forma", &forma);
 	    strncpy(shapeBuffer, json_object_get_string(forma)+1, strlen(json_object_get_string(forma))-2);
 	    *shape = atoi(shapeBuffer);

 	    json_object_object_get_ex(parsed_json, "tipo_cont", &tipo_cont);

 		tmn = atoi((const char *) json_object_get_string(tamano));

 		strncpy(content_type, json_object_get_string(tipo_cont), 5);

 		json_object_object_get_ex(parsed_json, "precisión", &precisn);
 	    *precision = atoi(json_object_get_string(precisn));
 	}
 	else if(strncmp(command, "incr", 4) == 0){
 	       json_object_object_get_ex(parsed_json, "n_pasos", &n_pasos);
 	       *passes = atoi(json_object_get_string(n_pasos));
 	}
 	else if(strncmp(command, "cerrar", 6) == 0){
 	}
 	else if(strncmp(command, "leer", 4) == 0){
 	        fflush( stdout );
 	        json_object_object_get_ex(parsed_json, "var", &var);
 	        strncpy(var_name, "                              ", strlen(var_name));
            strncpy(var_name, json_object_get_string(var), strlen(json_object_get_string(var)));
            json_object_object_get_ex(parsed_json, "precisión", &precisn);
 	        *precision = atoi(json_object_get_string(precisn));
 	}
}

void recvint_(int *client, int *intCont, int *arraySize){
    int64_t int64Cont [*arraySize];
    int n, i;
    int tmn = *arraySize * sizeof(int64_t);
    n = recv(*client, &int64Cont, tmn, 0 );
   for(i = 0; i<*arraySize; i++){
        if (int64Cont[i]-INT_MIN <= (int64_t)INT_MAX-INT_MIN){
           intCont[i] = int64Cont[i];
            }
        else{
            printf("Error transferring int values, int values probably out of bounds for C int\n");
            printf("The error occured in the slot: %d\n", i);
            printf("The value will be replaced using NaN");
            intCont[i] = NAN;
        }
    }
}

void recvfloat_(int *client, int *intCont, int *arraySize, int *precision_factor){

    double doubleCont [*arraySize];
    int n, i;
    int tmn = *arraySize * sizeof(double);
    n = recv(*client, &doubleCont, tmn, 0 );
    fflush(stdout);
    for(i = 0; i<*arraySize; i++){
        if (doubleCont[i]-FLT_MIN <= (double)FLT_MAX-FLT_MIN){
           intCont[i] = (int) round(*precision_factor * doubleCont[i]);
           }
        else{
            printf("Error transferring float values, float values probably out of bounds for C float\n");
            printf("The error occured in the slot: %d\n", i);
            printf("The value will be replaced using NaN");
            intCont[i] = NAN;
        }
    }
}

void sendr_(int *client, int *intSenderBuffer, float *floatSenderBuffer, char shape[], int *intLength, int *floatLength,
            int *precision_factor){
 int i,index;
 int sendRes;
 char *valLen[64];
 char jsonEncabezadoString[2048];
 int jsonStringLen;
 float int2float_buffer [*intLength];

 strncpy(jsonEncabezadoString, "{\"tamaño\": ", 13);
 if(*floatLength != 0){
    sprintf( valLen, "%ld", sizeof(float)* (*floatLength));
    strcat(jsonEncabezadoString, valLen);
    strcat(jsonEncabezadoString, ", \"tipo_cont\": \"float32\"");
 }
 else if(*precision_factor > 1){
    sprintf( valLen, "%ld", sizeof(float)* (*intLength));
    strcat(jsonEncabezadoString, valLen);
    strcat(jsonEncabezadoString, ", \"tipo_cont\": \"float32\"");
 }
 else{
    sprintf(valLen, "%ld", sizeof(int)* (*intLength));
    strcat(jsonEncabezadoString, valLen);
    strcat(jsonEncabezadoString, ", \"tipo_cont\": \"int32\"");
 }
 strcat(jsonEncabezadoString, ", \"forma\": ");
 strcat(jsonEncabezadoString, shape);
 strcat(jsonEncabezadoString, "}");

 i = strlen(jsonEncabezadoString);

 sendRes = send(*client, &i, sizeof (i), 0);

 if (sendRes==-1){
	printf("Message size send failed...");
    }

 sendRes = send(*client, (char *)jsonEncabezadoString, strlen(jsonEncabezadoString),0);

 if (sendRes==-1){
	printf("Message send failed...");
	}
 if(*floatLength != 0){
    sendRes = send(*client, floatSenderBuffer, (sizeof(float)* (*floatLength)), 0);
    }
 else if(*precision_factor > 1) {
    for(index = 0; index < *intLength; index++){
        int2float_buffer[index] = (float) intSenderBuffer[index]/ *precision_factor;
    }
    sendRes = send(*client, int2float_buffer, (sizeof(float)* (*intLength)), 0);
 }
 else{
    sendRes = send(*client, intSenderBuffer, (sizeof(int)* (*intLength)), 0);
 }
 if (sendRes==-1){
	printf("Data send failed...");
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