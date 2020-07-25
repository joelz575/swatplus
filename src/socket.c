#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <json.h>
#include <json_tokener.h>
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
 printf("\nClient obj in C: %i", *client);
 int portNumber = *portNum;
 printf("\nHost Number in C: "); printf(hostNum);
 struct sockaddr_in serv_addr;

 if ((*client = socket(AF_INET, SOCK_STREAM, 0)) < 0)
    {
        printf("\n Socket creation error \n");
        //return -1;
    }
 else{
 	printf("This is the client in C: %d", client);
 }
 serv_addr.sin_family = AF_INET;
 serv_addr.sin_port = htons(portNumber);
 if(inet_pton(AF_INET, hostNum, &serv_addr.sin_addr)<=0)
    {
        printf("\nInvalid address/ Address not supported \n");
        //return -1;
    }
 if (connect(*client, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0)
    {
        printf("\nConnection Failed \n");

    }


#endif
}

void receive_(int *client, char command, char *var_nombre, char *tip_con, int *tamano_con, int *intCont[], float *floatCont[] //, char *charCont[]
){
 #ifdef _WIN32
 	//Cuando tenga una connexión exitosa, podemos recibir datos del socket.
 	//return
 	//setvbuf(stdout, NULL, _IONBF, 0);
 	printf("\n Am in receive now...");
 	printf("\n INT buffer: %i", intCont);
 	printf("\n FLOAT buffer: %f", floatCont);
 	//printf("\n CHAR buffer: %s", charCont);
 	int recvRes;
 	int n;
 	char tipo_contenido;
	char blankBuffer;
 	char contBuffer;
 	struct json_object *parsed_json;
 	struct json_object *orden;
 	//struct json_object *tamano;
 	struct json_object *var;
 	struct json_object *matr;
 	struct json_object *contenido;
 	struct json_object *tipo_cont;
 	char json_header;

	printf("Just before receive line...");
	n = recv(*client, &blankBuffer, 2, 0);
	printf("Size accoring to n variable: %d", n);
	printf("Size according to blank Buffer: %d", (int)blankBuffer);
	sendr_(client, "RCVD");
	printf((char *)WSAGetLastError());

 	printf("About to receive json_header....\n");
 	n = recv(*client, &json_header, ((int) blankBuffer), 0);
 	printf((char *)WSAGetLastError());
 	printf("Receive Results from json_header: %i\n", n);
 	//printf("Received json-header: %s\n", (char) json_header);
 	sendr_(client, "RCVD");

 	parsed_json = json_tokener_parse(json_header);
 	printf("Parsed json: %s", json_object_get_string(parsed_json));
 	json_object_object_get_ex(parsed_json, "orden", &orden);
 	printf("Orden: ", json_object_get_string(orden));
 	//json_object_object_get_ex(parsed_json, "tamaño", &tamano);
 	//printf(tamano);
 	json_object_object_get_ex(parsed_json, "var", &var);
 	printf(json_object_get_string(var));
 	json_object_object_get_ex(parsed_json, "matr", &matr);
 	printf(json_object_get_string(matr));
 	json_object_object_get_ex(parsed_json, "tipo_cont", &tipo_cont);
	command = json_object_get_string(orden);
 	printf("Received Command: %s", command);

 	if(command == "TOMAR_"){
 		var_nombre = json_object_get_string(var);
 		printf("Received Variable: %s", var_nombre);

 		tipo_contenido = json_object_get_string(tipo_cont);

 		recv(*client, &contBuffer, 5, 0);

 		contenido = json_tokener_parse(contBuffer);

 		printf("This is the matrix: %s", contenido);
 		*tamano_con = json_object_array_length(contenido);

 	 	if(tamano_con != 0){
 			switch(tipo_contenido){
 				case 'int':
 	 				intCont = (int *)malloc(sizeof(int)*((int) tamano_con));
 	 				//charCont = (int *)malloc(sizeof(char)*0);
 	 				floatCont = (int *)malloc(sizeof(double)*0);
 	 				*intCont = (int *) json_object_get_array(contenido);
 	 			case 'flt':
 	 				floatCont = (int *)malloc(sizeof(double)*((int)tamano_con));
 	 				//charCont = (int *)malloc(sizeof(char)*0);
 	 				intCont = (int *)malloc(sizeof(int)*0);
 	 				*floatCont = (int *) json_object_get_array(contenido);
 	 			case 'str':
 	 				printf("Transfering Data of string types are not yet supported.");
 	 				//charCont = (int *)malloc(sizeof(char)*((int)tamano_con));
 	 				intCont = (int *)malloc(sizeof(int)*0);
 	 				floatCont = (int *)malloc(sizeof(double)*0);
 	 			default :
 	 				exit(-1);
 	 			}
			}
 		else{
 			//charCont = (int *)malloc(sizeof(char)*0);
 			printf("No data was transferred, in the exchange.");
 			intCont = (int *)malloc(sizeof(int)*0);
 			floatCont = (int *)malloc(sizeof(double)*0);
		}

 	}



 	printf((char *)WSAGetLastError());
	sendr_(client, "RCVD");
 #else
 	//Porque tenga una connexión exitosa, podemos recibir datos del socket.
 	printf("\n Am in receive now...");
 	printf("\nClient obj in C: %i", *client);
 	printf("\n Am in receive now...");
 	int recvRes;
 	//printf("\nLength is: %i", *len);
 	int tmn;
 	int n;
 	char blankBuffer;
 	struct json_object *parsed_json;
 	struct json_object *tipo;
 	struct json_object *tamano;
 	struct json_object *var;
 	struct json_object *matr;
 	char json_header;


	n = tmn = read(*client, blankBuffer, 4);

 	read(*client, json_header, tmn);
 	parsed_json = json_tokener_parse(json_header);
 	json_object_object_get_ex(parsed_json, "tipo", &tipo);
 	json_object_object_get_ex(parsed_json, "tamaño", &tamano);
 	json_object_object_get_ex(parsed_json, "var", &var);
 	json_object_object_get_ex(parsed_json, "matr", &matr);
 	tamano_con = json_object_get_int(tamano);
 	if (tamano != 0){
 		read(*client, *contenido, tamano_con);
	}
 	else {
 		*contenido = ' ';
 	}

 #endif
	}

void sendr_(int *client, char *senderBuffer){


 int iSenderBuffer;
 printf("Sending: %s", senderBuffer);
 iSenderBuffer = strlen(senderBuffer);
 int sendRes;
 //printf("Sending now....");
 sendRes = send(*client, senderBuffer, iSenderBuffer ,0);
 printf("Send Result: %d", sendRes);
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