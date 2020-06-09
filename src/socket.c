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
 printf("\nClient obj in C: %i", *client);
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

void receive_(int *client, char *contenido, char *var_nombre, int *tamano_con){
 #ifdef _WIN32
 //Cuando tenga una connexión exitosa, podemos recibir datos del socket.
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


 n = tmn = recv(*client, blankBuffer, 4, 0);

 recv(*client, json_header, tmn, 0);
 parsed_json = json_tokener_parse(json_header);
 json_object_object_get_ex(parsed_json, "tipo", &tipo);
 json_object_object_get_ex(parsed_json, "tamaño", &tamano);
 json_object_object_get_ex(parsed_json, "var", &var);
 json_object_object_get_ex(parsed_json, "matr", &matr);
 tamano_con = json_object_get_int(tamano);

 if (tamano != 0){
 	recv(*client, *contenido, tamano_con, 0);
	}
 else {
 	*contenido = ' ';
 }

 //for(i=0; i<n)

 printf((char *)WSAGetLastError());

   // while (tmñ > 0 && n < *len) {
   //     length = read(*client,&receiveBuffer[n],*len - n);
		//printf("current receiveBuffer %c", receiveBuffer[n]);
        /* FIXME: Error checking */

   //     n += length;
    //}


    //for(int i=0; i<*len; i++){
    //	printf("%c",ntohl(receiveBuffer[i]));
    //	}

 //printf("\n Managed to receive: %i",strlen(receiveBuffer));
 //printf((char *)WSAGetLastError());
 //if (recvRes == -1){
 //		printf("receive failed...");
 //		printf((char *)WSAGetLastError());
 //		}
 //printf("\n%s", receiveBuffer);

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
 //n = length = read(*client,receiveBuffer, *len, 0);
 //printf("\nn= %d", n);
 //printf("\nlength= %d", length);
 //   while (length > 0 && n < *len) {
 //       length = read(*client,&receiveBuffer[n],*len - n);
//		printf("length %d",length);
//		printf("current receiveBuffer %c", receiveBuffer[n]);
        /* FIXME: Error checking */

//        n += length;
//    }

//    for(int i=0; i<*len; i++){
//    	printf("%c",ntohl(receiveBuffer[i]));
//    	}

// if (recvRes == -1){
//		printf("receive failed...");
//		}


 //printf("In C function receive Buffer is...");
 //printf("\n%s", receiveBuffer);
 #endif
	}

void sendr_(int *client, char *senderBuffer){


 int iSenderBuffer;
 iSenderBuffer = strlen(senderBuffer);
 int sendRes;
 //printf("Sending now....");
 sendRes = send(*client, senderBuffer, iSenderBuffer ,0);

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